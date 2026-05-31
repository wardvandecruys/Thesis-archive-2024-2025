################################################################################
# C3 — Portfolio Manipulator
#
# Purpose
# - Portfolio-level “what-if” engine to manipulate pricing levers and visualize
#   the impact on lapse risk:
#     • Apply absolute or relative changes to profit-sharing rate (ps_rate)
#       and/or annual premium (annual_prem) with optional clamping.
#     • Recompute lapse probabilities and classes (ŷ ≥ THR ⇒ “Lapse”).
#     • Summarize mitigated/worsened cases and produce a compact plot suite.
#
# What this script provides
# 1) Scenario engine
#    - `run_scenario(df_year, model, ...)` returns:
#        • $summary  : portfolio totals before/after, mitigated/worsened counts.
#        • $detailed : row-level p_base, p_new, flips, delta_p, plus title attrs.
#        • $modified_data : the counterfactual data used for prediction.
#    - Accepts RELATIVE (percent) or ABSOLUTE changes; robust input parsing
#      for values like "-5" or "-5%"; per-variable clamps (e.g., ps ∈ [0,1]).
#
# 2) Convenience wrappers
#    - `scenario_ps_only()`, `scenario_prem_only()`, `scenario_both()` for quick
#      single-lever and joint-lever experiments.
#
# 3) Year targeting
#    - `pick_most_interesting_year()` selects the year with the largest share of
#      high-risk policies (ŷ ≥ THR), using existing `p_lapse` or the model.
#
# 4) Visualization suite (ggplot2 + patchwork)
#    - Distributions before/after; flip counts; base vs new scatter; class
#      shares before→after; Δp by baseline decile.
#    - `pack_scenario_plots()` produces a tidy 2×3 or 1×5 panel per scenario.
#    - `combine_all_scenarios_3x5()` lines up multiple scenarios (A/B/C) in a
#      3×5 grid with shared legends and a bold title.
#
# 5) Styling & titling
#    - KU Leuven-inspired palette and minimal theme.
#    - Smart titles/subtitles: compact change labels (e.g., “PS rate −5% ·
#      Annual premium +10%”) and mode notes (relative/absolute) are auto-built.
#
# Inputs & assumptions
# - Data: `lapses_data` with at least `data_year`, `ps_rate`, `annual_prem`,
#   and (optionally) `p_lapse`; some plots assume binary threshold at THR=0.5.
# - Model: caret classifier (e.g., XGBoost) supporting `predict(..., type="prob")`.
#   Positive class is auto-detected (`TRUE` or last level) — adjust if needed.
#
# Key configuration
# - Threshold: `cls_threshold` (default 0.5).
# - Change specs: `mode = "relative" | "absolute"`, `amount` (e.g., -5 or "-5%"),
#   and `clamp` ranges per variable.
# - Plot packing: choose layout `"2x3"` or `"1x5"`; export via `save_fig()/ggsave`.
#
# Outputs
# - Printed plots for each scenario and combined grids.
# - PNGs saved by the example usage section (e.g., `scenarios_3x5.png`).
# - Tibbles with before/after probabilities, classes, flips, and deltas for
#   downstream reporting.
#
# Performance tips
# - Use `use_existing_p = TRUE` when `p_lapse` already exists to avoid
#   re-predicting large portfolios.
# - Keep titles compact with `.short_change_title()`; long subtitles are wrapped.
#
# Reproducibility
# - Deterministic transforms; set a random seed only if your upstream model or
#   data preparation introduces stochasticity.
################################################################################

# ==============================================================================
# 1. Packages
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(caret)
  library(xgboost)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# ==============================================================================
# 2. Palette & Theme
# ==============================================================================
KUL_BLUE_DARK  <- "#00407A"
KUL_BLUE_LIGHT <- "#52BDEC"

theme_kul <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", color = KUL_BLUE_DARK),
      plot.subtitle = element_text(color = KUL_BLUE_DARK),
      axis.title    = element_text(color = KUL_BLUE_DARK),
      legend.title  = element_text(color = KUL_BLUE_DARK)
    )
}

# ==============================================================================
# 3. Title Builders (wrap, short change labels, mode notes, subtitles)
# =============================================================================
.wrap_title <- function(s, width = 50) paste(strwrap(s, width = width), collapse = "\n")

.short_change_title <- function(change_ps, change_prem, compact_names = TRUE) {
  name_of <- function(var) {
    if (!compact_names) return(switch(var,
                                      "ps_rate" = "profit-sharing rate", "annual_prem" = "annual premium", var))
    switch(var, "ps_rate" = "PS rate", "annual_prem" = "Annual premium", var)
  }
  fmt <- function(var, mode, amt) {
    if (is.null(amt) || amt == 0) return(NULL)
    if (mode == "relative") paste0(name_of(var), " ", ifelse(amt >= 0, "+", ""), round(100*amt, 1), "%")
    else                    paste0(name_of(var), " ", ifelse(amt >= 0, "+", ""), amt)
  }
  parts <- c(
    fmt("ps_rate",     change_ps$mode,   change_ps$amount),
    fmt("annual_prem", change_prem$mode, change_prem$amount)
  )
  if (length(parts) == 0) "no change" else paste(parts, collapse = " · ")
}

.mode_note <- function(change_ps, change_prem) {
  tag <- function(var, mode, amt) {
    if (is.null(amt) || amt == 0) return(NULL)
    paste0(if (var == "ps_rate") "PS rate" else "Annual premium", " (", mode, ")")
  }
  parts <- c(tag("ps_rate", change_ps$mode, change_ps$amount),
             tag("annual_prem", change_prem$mode, change_prem$amount))
  if (length(parts)) paste(parts, collapse = " · ") else NULL
}

# Build multi-line subtitle: base line (+ mode note), and when BOTH vars change add a 3rd line
.build_subtitle <- function(d, base_text, width = 80, include_mode_note = TRUE, add_change_title_if_both = TRUE) {
  lines <- c(.wrap_title(base_text, width))
  if (include_mode_note) {
    mn <- attr(d, "change_mode_note")
    if (!is.null(mn) && nzchar(mn)) lines <- c(lines, .wrap_title(mn, width))
  }
  if (isTRUE(attr(d, "both_changed")) && add_change_title_if_both) {
    ct <- attr(d, "change_title_short")
    if (!is.null(ct) && nzchar(ct)) lines <- c(lines, .wrap_title(ct, width))
  }
  paste(lines, collapse = "\n")
}

# ==============================================================================
# 4. Data Type Coercion (factors, logicals)
# ==============================================================================
coerce_types <- function(df) {
  df %>%
    mutate(
      data_year    = as.integer(data_year),
      surrenders   = if (is.logical(surrenders)) surrenders else as.logical(surrenders),
      surrenders   = factor(surrenders, levels = c(FALSE, TRUE)),
      gender       = as.factor(gender),
      prem_freq    = as.factor(prem_freq),
      risk_class   = as.factor(risk_class),
      living_place = as.factor(living_place),
      fund         = as.factor(fund)
    )
}

# ==============================================================================
# 5. Positive Class Detection & Probability Prediction
# ==============================================================================
get_pos_class <- function(model) {
  levs <- tryCatch(model$levels, error = function(e) NULL)
  if (!is.null(levs)) {
    if ("TRUE" %in% levs) return("TRUE")
    return(tail(levs, 1))
  }
  "TRUE"
}

predict_prob <- function(model, newdata) {
  pos <- get_pos_class(model)
  pr  <- predict(model, newdata = newdata, type = "prob")
  if (is.data.frame(pr) && pos %in% colnames(pr)) {
    as.numeric(pr[[pos]])
  } else if (is.numeric(pr)) {
    as.numeric(pr)
  } else {
    stop("Could not locate positive-class probability in model predictions.")
  }
}

# ==============================================================================
# 6. Year Selection: pick_most_interesting_year()
# ==============================================================================
pick_most_interesting_year <- function(df, threshold = 0.5, use_existing_p = TRUE, model = NULL) {
  df <- coerce_types(df)
  if (!use_existing_p) {
    if (is.null(model)) stop("To recompute p_lapse, provide 'model'.")
    df$p_lapse <- predict_prob(model, df)
  } else {
    if (!"p_lapse" %in% names(df)) stop("Column 'p_lapse' not found; set use_existing_p = FALSE to recompute with model.")
  }
  counts <- df %>%
    group_by(data_year) %>%
    summarize(n_high = sum(p_lapse >= threshold, na.rm = TRUE), .groups = "drop")
  max_n <- max(counts$n_high, na.rm = TRUE)
  year_chosen <- counts %>% filter(n_high == max_n) %>% arrange(desc(data_year)) %>% pull(data_year) %>% .[1]
  list(year = year_chosen, counts = counts, data = df %>% filter(data_year == year_chosen))
}

# ==============================================================================
# 7. Numeric Adjustment Helpers (relative/absolute, clamping, parsing)
# ==============================================================================
adjust_numeric <- function(x, mode = c("relative","absolute"), amount = 0, clamp = c(-Inf, Inf)) {
  mode <- match.arg(mode)
  out <- if (mode == "relative") x * (1 + amount) else x + amount
  pmax(clamp[1], pmin(clamp[2], out))
}

.normalize_amount <- function(amount, mode = c("relative","absolute")) {
  mode <- match.arg(mode)
  if (is.null(amount) || is.na(amount)) return(0)

  if (mode == "relative") {
    # Accept numeric percent points (recommended) or "%"-strings.
    if (is.character(amount)) {
      a <- suppressWarnings(as.numeric(gsub("%", "", amount)))
      return(a / 100)  # e.g., "-5%" -> -0.05
    }
    if (!is.numeric(amount)) stop("Relative 'amount' must be numeric or a string like '-5%'.")
    if (abs(amount) > 1) return(amount / 100)  # -5 => -0.05
    if (amount != 0) {
      warning("Relative amounts should be given in percent points (e.g., -5 for -5%). ",
              "You passed a fractional value (", amount, "); interpreting as a fraction.")
    }
    return(amount)
  } else {
    if (is.character(amount)) {
      a <- suppressWarnings(as.numeric(amount))
      if (is.na(a)) stop("Absolute 'amount' must be numeric.")
      return(a)
    }
    return(as.numeric(amount))
  }
}

# ==============================================================================
# 8. Scenario Engine: run_scenario() (+ attributes for titles)
# ==============================================================================
.compact_change_title <- function(change_ps, change_prem) {
  .short_change_title(change_ps, change_prem, compact_names = FALSE)
}

run_scenario <- function(df_year,
                         model,
                         cls_threshold = 0.5,
                         change_ps   = list(mode = "relative", amount = 0, clamp = c(0,1)),
                         change_prem = list(mode = "relative", amount = 0, clamp = c(0, Inf))) {

  df_year <- coerce_types(df_year)

  # Normalize inputs (supports "-5%" or -5 meaning -5%)
  change_ps$mode     <- ifelse(is.null(change_ps$mode),   "relative", change_ps$mode)
  change_prem$mode   <- ifelse(is.null(change_prem$mode), "relative", change_prem$mode)
  change_ps$amount   <- .normalize_amount(change_ps$amount,     change_ps$mode)
  change_prem$amount <- .normalize_amount(change_prem$amount,   change_prem$mode)

  # Baseline predictions
  base_prob <- predict_prob(model, df_year)
  base_cls  <- ifelse(base_prob >= cls_threshold, "Lapse", "In force")

  # Apply changes
  df_mod <- df_year
  if (!is.null(change_ps$amount) && change_ps$amount != 0) {
    df_mod$ps_rate <- adjust_numeric(df_mod$ps_rate,
                                     mode   = change_ps$mode,
                                     amount = change_ps$amount,
                                     clamp  = if (is.null(change_ps$clamp)) c(0,1) else change_ps$clamp)
  }
  if (!is.null(change_prem$amount) && change_prem$amount != 0) {
    df_mod$annual_prem <- adjust_numeric(df_mod$annual_prem,
                                         mode   = change_prem$mode,
                                         amount = change_prem$amount,
                                         clamp  = if (is.null(change_prem$clamp)) c(0, Inf) else change_prem$clamp)
  }

  # New predictions
  new_prob <- predict_prob(model, df_mod)
  new_cls  <- ifelse(new_prob >= cls_threshold, "Lapse", "In force")

  # Summaries
  before_high <- base_cls == "Lapse"
  after_high  <- new_cls  == "Lapse"
  summary_tbl <- tibble::tibble(
    n_total         = nrow(df_year),
    n_high_before   = sum(before_high, na.rm = TRUE),
    n_low_before    = sum(!before_high, na.rm = TRUE),
    n_high_after    = sum(after_high,  na.rm = TRUE),
    n_low_after     = sum(!after_high, na.rm = TRUE),
    n_mitigated     = sum(before_high & !after_high, na.rm = TRUE),
    n_worsened      = sum(!before_high & after_high, na.rm = TRUE),
    share_mitigated = ifelse(sum(before_high)>0, mean((before_high & !after_high)[before_high]), NA_real_),
    share_worsened  = ifelse(sum(!before_high)>0, mean((!before_high & after_high)[!before_high]), NA_real_)
  )

  detailed <- df_year %>%
    mutate(p_base = base_prob, cls_base = base_cls) %>%
    mutate(p_new  = new_prob,  cls_new  = new_cls) %>%
    mutate(
      delta_p = p_new - p_base,
      flipped = cls_base != cls_new,
      flip_dir = dplyr::case_when(
        cls_base == "Lapse"    & cls_new == "In force" ~ "Lapse→In force",
        cls_base == "In force" & cls_new == "Lapse"    ~ "In force→Lapse",
        TRUE ~ "none"
      )
    )

  # Attributes for auto-titles/subtitles
  attr(detailed, "change_title_short") <- .short_change_title(change_ps, change_prem, compact_names = TRUE)
  attr(detailed, "change_mode_note")   <- .mode_note(change_ps, change_prem)
  attr(detailed, "change_title")       <- .compact_change_title(change_ps, change_prem)  # legacy
  attr(detailed, "both_changed")       <- (isTRUE(change_ps$amount != 0) && isTRUE(change_prem$amount != 0))

  list(summary = summary_tbl, detailed = detailed, modified_data = df_mod)
}

# ==============================================================================
# 9. Convenience Wrappers: ps_only / prem_only / both
# ==============================================================================
scenario_ps_only <- function(df_year, model,
                             cls_threshold = 0.5,
                             mode = c("relative","absolute"),
                             amount = 0,
                             clamp = c(0,1)) {
  run_scenario(df_year, model, cls_threshold,
               change_ps   = list(mode = match.arg(mode), amount = amount, clamp = clamp),
               change_prem = list(mode = "relative", amount = 0, clamp = c(0, Inf)))
}

scenario_prem_only <- function(df_year, model,
                               cls_threshold = 0.5,
                               mode = c("relative","absolute"),
                               amount = 0,
                               clamp = c(0, Inf)) {
  run_scenario(df_year, model, cls_threshold,
               change_ps   = list(mode = "relative", amount = 0, clamp = c(0,1)),
               change_prem = list(mode = match.arg(mode), amount = amount, clamp = clamp))
}

scenario_both <- function(df_year, model,
                          cls_threshold = 0.5,
                          ps_mode = c("relative","absolute"), ps_amount = 0, ps_clamp = c(0,1),
                          prem_mode = c("relative","absolute"), prem_amount = 0, prem_clamp = c(0, Inf)) {
  run_scenario(df_year, model, cls_threshold,
               change_ps   = list(mode = match.arg(ps_mode),   amount = ps_amount,   clamp = ps_clamp),
               change_prem = list(mode = match.arg(prem_mode), amount = prem_amount, clamp = prem_clamp))
}

# ==============================================================================
# 10. Grid Scan over Scenario Amounts
# ==============================================================================
run_grid <- function(df_year, model, cls_threshold = 0.5,
                     ps_amounts = c(-10, -5, 0, 5, 10),         # interpreted as %
                     prem_amounts = c(-10, -5, 0, 5, 10),       # interpreted as %
                     ps_mode = "relative", prem_mode = "relative") {
  grid <- tidyr::expand_grid(ps = ps_amounts, prem = prem_amounts)
  purrr::pmap_dfr(grid, function(ps, prem) {
    out <- scenario_both(df_year, model,
                         cls_threshold = cls_threshold,
                         ps_mode = ps_mode, prem_mode = prem_mode,
                         ps_amount = ps, prem_amount = prem)
    tibble::tibble(ps_amount = ps, prem_amount = prem) %>%
      bind_cols(out$summary)
  })
}

# ==============================================================================
# 11. Flip Columns Guard (.ensure_flip_cols)
# ==============================================================================
.ensure_flip_cols <- function(d, threshold = 0.5) {
  d %>%
    mutate(
      cls_base = if (!"cls_base" %in% names(d)) ifelse(p_base >= threshold, "Lapse", "In force") else cls_base,
      cls_new  = if (!"cls_new"  %in% names(d)) ifelse(p_new  >= threshold, "Lapse", "In force") else cls_new,
      flip_dir = dplyr::case_when(
        cls_base == "Lapse"    & cls_new == "In force" ~ "Lapse→In force",
        cls_base == "In force" & cls_new == "Lapse"    ~ "In force→Lapse",
        TRUE ~ "none"
      )
    )
}

# ==============================================================================
# 12. Plot Helpers
#    • Probability Distributions (Before vs After)
#    • Flip Counts
#    • Base vs New Scatter
#    • Above/Below Threshold Shares
#    • Δp by Baseline Decile
# ==============================================================================
# 1) Distributions
plot_prob_distributions <- function(detailed, threshold = 0.5, title = NULL, bins = 50) {
  d <- .ensure_flip_cols(detailed, threshold)
  if (is.null(title)) {
    ct <- attr(d, "change_title_short")
    title <- .wrap_title(ifelse(is.null(ct), "no change", ct))  # removed "Scenario — "
  } else title <- .wrap_title(title)

  subtitle <- .build_subtitle(d, base_text = "Before vs After")

  dd <- dplyr::bind_rows(
    d %>% transmute(value = p_base, period = "Before"),
    d %>% transmute(value = p_new,  period = "After")
  )
  ggplot(dd, aes(x = value, y = after_stat(density), fill = period)) +
    geom_histogram(bins = bins, alpha = 0.5, position = "identity") +
    geom_vline(xintercept = threshold, linetype = "dashed", color = KUL_BLUE_DARK) +
    annotate("text", x = threshold, y = Inf, vjust = 1.3,
             label = paste0("threshold = ", threshold), size = 3, color = KUL_BLUE_DARK) +
    scale_fill_manual(values = c(Before = KUL_BLUE_LIGHT, After = KUL_BLUE_DARK)) +
    labs(x = "Probability of lapse", y = "Density", title = title, subtitle = subtitle) +
    theme_kul()
}

# 2) Flips
plot_flip_counts <- function(detailed, threshold = 0.5, title = NULL) {
  d <- .ensure_flip_cols(detailed, threshold)
  if (is.null(title)) {
    ct <- attr(d, "change_title_short")
    title <- .wrap_title(ifelse(is.null(ct), "no change", ct))
  } else title <- .wrap_title(title)

  subtitle <- .build_subtitle(
    d,
    base_text = "Mitigated (Lapse→In force), Worsened (In force→Lapse)"
  )

  counts <- d %>%
    count(flip_dir, name = "n") %>%
    mutate(flip_dir = factor(flip_dir, levels = c("Lapse→In force", "In force→Lapse", "none")))

  # --- dynamic headroom so top labels never clip ---
  y_max   <- max(counts$n, na.rm = TRUE)
  y_upper <- if (is.finite(y_max) && y_max > 0) y_max * 1.12 else 1  # ~12% headroom

  ggplot(counts, aes(x = flip_dir, y = n)) +
    geom_col(fill = KUL_BLUE_LIGHT, color = KUL_BLUE_DARK) +
    geom_text(aes(label = n), vjust = -0.25, size = 3.3, color = KUL_BLUE_DARK) +
    scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0.02, 0.02))) +
    coord_cartesian(clip = "off") +  # allow labels to draw above panel
    labs(x = NULL, y = "Policies", title = title, subtitle = subtitle) +
    theme_kul()
}



# 3) Base vs New
plot_base_vs_new <- function(detailed, threshold = 0.5, title = NULL) {
  d <- .ensure_flip_cols(detailed, threshold)
  if (is.null(title)) {
    ct <- attr(d, "change_title_short")
    title <- .wrap_title(ifelse(is.null(ct), "no change", ct))
  } else title <- .wrap_title(title)

  subtitle <- .build_subtitle(
    d,
    base_text = "Points below the diagonal improved"
  )

  ggplot(d, aes(x = p_base, y = p_new)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = KUL_BLUE_DARK) +
    geom_point(aes(shape = flip_dir), alpha = 0.6, color = KUL_BLUE_DARK) +
    geom_vline(xintercept = threshold, linetype = "dashed", color = KUL_BLUE_DARK) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = KUL_BLUE_DARK) +
    coord_equal() +
    labs(x = "Baseline p_lapse", y = "Scenario p_lapse", title = title, subtitle = subtitle) +
    theme_kul()
}

# 4)
plot_above_below_threshold <- function(detailed, threshold = 0.5, title = NULL) {
  d <- .ensure_flip_cols(detailed, threshold)
  if (is.null(title)) {
    ct <- attr(d, "change_title_short")
    title <- .wrap_title(ifelse(is.null(ct), "no change", ct))  # removed "Scenario — "
  } else title <- .wrap_title(title)

  subtitle <- .build_subtitle(
    d,
    base_text = paste0("Threshold = ", threshold, " · Before vs After")
  )

  tab <- tibble(
    period = c("Before","After"),
    Lapse  = c(mean(d$p_base >= threshold, na.rm = TRUE),
               mean(d$p_new  >= threshold, na.rm = TRUE))
  ) %>%
    mutate(`In force` = 1 - Lapse) %>%
    pivot_longer(c("Lapse","In force"), names_to = "class", values_to = "share") %>%
    mutate(period = factor(period, levels = c("Before","After")))  # enforce order

  ggplot(tab, aes(period, share, fill = class)) +
    geom_col(color = KUL_BLUE_DARK) +
    scale_fill_manual(values = c("Lapse" = KUL_BLUE_DARK, "In force" = KUL_BLUE_LIGHT)) +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
              position = position_stack(vjust = 0.5), size = 3.2, color = "white") +
    labs(x = NULL, y = "Share of policies", fill = NULL, title = title, subtitle = subtitle) +
    theme_kul()
}

# 5) Δp by baseline decile
plot_delta_by_decile <- function(detailed, k = 10, title = NULL) {
  stopifnot("p_base" %in% names(detailed), "p_new" %in% names(detailed))
  if (is.null(title)) {
    ct <- attr(detailed, "change_title_short")
    title <- .wrap_title(ifelse(is.null(ct), "no change", ct))  # removed "Scenario — "
  } else title <- .wrap_title(title)

  subtitle <- .build_subtitle(
    detailed,
    base_text = "Negative bars = improvement"
  )

  d <- detailed %>%
    mutate(delta_p = p_new - p_base, decile = dplyr::ntile(p_base, k)) %>%
    group_by(decile) %>%
    summarise(mean_delta = mean(delta_p, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(x = factor(decile), y = mean_delta, group = 1)) +
    geom_col(fill = KUL_BLUE_LIGHT, color = KUL_BLUE_DARK) +
    geom_hline(yintercept = 0, linetype = "dotted", color = KUL_BLUE_DARK) +
    labs(x = "Baseline p_lapse decile (In force → Lapse)",
         y = "Mean Δp (p_new − p_base)",
         title = title,
         subtitle = subtitle) +
    theme_kul()
}

# ==============================================================================
# 13. Multi-Plot Assembly (patchwork): make/pack/combine
# ==============================================================================
# Build 5 standard plots (unpolished; used for single prints)
make_plots_for <- function(detailed) {
  list(
    `1·Lapse dist.`   = plot_prob_distributions(detailed),
    `2·Flips`         = plot_flip_counts(detailed),
    `3·Base vs New`   = plot_base_vs_new(detailed),
    `4·Shares (B→A)`  = plot_above_below_threshold(detailed),
    `5·Δp by decile`  = plot_delta_by_decile(detailed)
  )
}

# Compact theming for grids (prevents overlaps)
.polish_for_grid <- function(p) {
  p +
    theme(
      plot.tag.position = c(0.01, 0.99),
      plot.tag          = element_text(face = "bold", color = KUL_BLUE_DARK, margin = margin(b = 2)),
      plot.margin       = margin(t = 8, r = 10, b = 12, l = 10),
      plot.title        = element_text(size = 11, lineheight = 1.05, margin = margin(b = 4)),
      plot.subtitle     = element_text(size = 9.5, lineheight = 1.06, margin = margin(t = 2, b = 6)),
      plot.caption      = element_text(size = 8.5, margin = margin(t = 6)),
      legend.box.margin = margin(t = 4)
    )
}

# Build 5 plots and apply grid polish (used only in composites)
make_plots_for_grid <- function(detailed) {
  pls <- make_plots_for(detailed)
  lapply(pls, .polish_for_grid)
}

# 3×5 merged grid
combine_all_scenarios_3x5 <- function(details_named, main_title = NULL, title = NULL) {
  if (!is.null(title) && is.null(main_title)) main_title <- title
  if (is.null(main_title)) main_title <- "All scenarios — 5 views each"

  scen_keys <- names(details_named)
  nscen <- length(scen_keys)

  plots <- unlist(lapply(seq_len(nscen), function(i) {
    nm  <- scen_keys[i]
    tag <- letters[i]  # a, b, c, ...
    pls <- make_plots_for_grid(details_named[[nm]])
    # tag only the first plot of this scenario's row
    lapply(seq_along(pls), function(j) {
      if (j == 1) pls[[j]] + ggplot2::labs(tag = tag) else pls[[j]] + ggplot2::labs(tag = NULL)
    })
  }), recursive = FALSE)

  g <- patchwork::wrap_plots(plots, ncol = 5) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  g + patchwork::plot_annotation(
    title = main_title,
    theme = ggplot2::theme(
      plot.title  = ggplot2::element_text(face = "bold", color = KUL_BLUE_DARK),
      plot.margin = margin(t = 10, r = 12, b = 10, l = 12)
    )
  )
}


# Packed 5-per-scenario — layout = "2x3" or "1x5"; accepts main_title/title
pack_scenario_plots <- function(detailed, layout = c("2x3","1x5"), main_title = NULL, title = NULL) {
  if (!is.null(title) && is.null(main_title)) main_title <- title
  layout <- match.arg(layout)

  pls <- make_plots_for_grid(detailed)

  p <- if (layout == "2x3") {
    (pls[[1]] | pls[[2]]) /
      (pls[[3]] | pls[[4]]) /
      (pls[[5]] | patchwork::plot_spacer())
  } else {
    patchwork::wrap_plots(pls, ncol = 5)
  }

  p <- p + patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(main_title)) {
    p <- p + patchwork::plot_annotation(
      title = main_title,
      theme = ggplot2::theme(
        plot.title  = ggplot2::element_text(face = "bold", color = KUL_BLUE_DARK),
        plot.margin = margin(t = 10, r = 12, b = 10, l = 12)
      )
    )
  }
  p
}


# ==============================================================================
# 14. Saving Helper (ggsave wrapper)
# ==============================================================================
save_fig <- function(plot_obj, filename, width = 7, height = 5, dpi = 300) {
  ggsave(filename, plot = plot_obj, width = width, height = height, dpi = dpi)
}

# ==============================================================================
# 15. Usage Example (load data/model, scenarios A/B/C, export)
# ==============================================================================
setwd("~/Desktop/R Run")
load("lapses_data_pred")
dat <- lapses_data
model_xgb <- readRDS("model_RDS_xgb_downsamp_bigdata.rds")

sel  <- pick_most_interesting_year(dat, threshold = 0.5, use_existing_p = TRUE)
df_yr <- sel$data

scA <- scenario_prem_only(df_yr, model_xgb, cls_threshold = 0.5, mode = "absolute", amount = -50)
scB <- scenario_ps_only  (df_yr, model_xgb, cls_threshold = 0.5, mode = "relative", amount = -5)
scC <- scenario_both     (df_yr, model_xgb, cls_threshold = 0.5,
                          ps_mode = "relative", ps_amount = -5,
                          prem_mode = "relative", prem_amount = 10)

print(plot_prob_distributions(scA$detailed)); print(plot_flip_counts(scA$detailed))
print(plot_base_vs_new(scA$detailed));        print(plot_above_below_threshold(scA$detailed))
print(plot_delta_by_decile(scA$detailed))

setwd("~/Desktop/R Run/Test")
g_all <- combine_all_scenarios_3x5(list(A = scA$detailed, B = scB$detailed, C = scC$detailed),
                                   main_title = "A/B/C — each with 5 views")
print(g_all)
gA <- pack_scenario_plots(scA$detailed, layout = "2x3", main_title = "A — 5 views")
gB <- pack_scenario_plots(scB$detailed, layout = "2x3", main_title = "B — 5 views")
gC <- pack_scenario_plots(scC$detailed, layout = "2x3", main_title = "C — 5 views")
print(gA); print(gB); print(gC)

ggsave("scenarios_3x5.png", g_all, width = 20, height = 12, dpi = 300)
ggsave("scenarioA_pack.png", gA, width = 12, height = 12, dpi = 300)
ggsave("scenarioB_pack.png", gB, width = 12, height = 12, dpi = 300)
ggsave("scenarioC_pack.png", gC, width = 12, height = 12, dpi = 300)
