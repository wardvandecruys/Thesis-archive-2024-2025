################################################################################
# C3 — Transition Plots: 1D/2D/3D Decision Surfaces & Minimal-Change Recommendations
#
# Purpose
# - Visualize how the predicted lapse probability (ŷ) responds to key drivers.
# - Find the nearest change in product levers that moves a policy below a
#   decision threshold (default ŷ < 0.5) under practical constraints.
#
# What this script builds
# 1) 1D transition curves:
#    - For a single numeric feature (e.g., ps_rate or annual_prem), plot ŷ over
#      the in-sample range and mark the first threshold crossing.
#
# 2) 2D transition surfaces:
#    - Heatmap of ŷ over (ps_rate, annual_prem) with an example isocontour ŷ = 0.5.
#
# 3) 2D “ZOOM” with minimal-change search:
#    - Computes the shortest move from the current point to a strict safe region
#      (ŷ ≤ threshold − below_margin) using a Mahalanobis metric in a whitened
#      space, with automatic weights:
#        • "elasticity" (local finite-difference sensitivity of ŷ),
#        • "iqr" / "stdev" / "range" (global scale heuristics),
#        • "fixed" (user-supplied).
#    - Enforces constraints such as annual_prem ≥ y_floor_frac × original,
#      optional caps on ps_rate, and zoomed plotting window.
#
# 4) 3D transition box (optional):
#    - Plotly volume of ŷ over (ps_rate, annual_prem, change_10) with an
#      isosurface at the decision threshold and the shortest path to a safe point.
#
# Inputs & Assumptions
# - Model: caret classifier loaded from RDS (`model_xgb`) with two class levels; the
#   positive class is interpreted as the SECOND level in `model$levels`.
#   (Ensure this matches your training convention, e.g., "No","Yes".)
# - Data: object `lapses_data` (loaded via `load("lapses_data_pred")`) containing
#   at least: `policy_id` (optional), `ps_rate`, `annual_prem`, and `change_10`.
# - Typical feature ranges assumed in plotting caps: ps_rate ∈ [0, 0.25],
#   annual_prem up to ~2500 (adjustable).
#
# Key Parameters (defaults shown in functions)
# - Threshold: thresh = 0.5; strict margin for safety: below_margin = 0.002.
# - 2D ZOOM constraints: y_floor_frac = 0.80; ps_rate caps via ps_cap_lower/upper.
# - Weighting modes for Mahalanobis geometry: "elasticity" | "iqr" | "stdev"
#   | "range" | "fixed" (see `compute_maha_weights()`).
# - Grid resolutions: e.g., grid_points = 140 (overview), grid_points_zoom = 260.
#
# Outputs
# - ggplot objects for 1D/2D and ZOOM views (printed to device).
# - Plotly 3D widget (printed) for interactive exploration.
# - A tibble of recommendations from the ZOOM search with old/new values,
#   raw Euclidean and Mahalanobis distances, and weight diagnostics.
#
# Notes
# - `make_posdef()` guards covariance matrices used for whitening/metrics.
# - The “elasticity” mode uses small, data-aware steps (based on 1–99% quantiles)
#   to stabilize local finite differences of ŷ.
# - If your positive class is NOT the second level, adjust `predict_prob()`.
################################################################################


# ==============================================================================
# 1. Packages
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(caret)
  library(xgboost)
  library(scales)
  library(grid)
  library(MASS)
  library(plotly)
})

# ==============================================================================
# 2. Plot Styling & Theme
# ==============================================================================
kuleuven_blue <- "#1E64C8"
paper_gray    <- "#3A3A3A"

theme_paper <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = paper_gray),
      plot.title = element_text(face = "bold", size = base_size + 1, margin = margin(b = 3)),
      plot.subtitle = element_text(size = base_size - 1, color = "#5A5A5A", margin = margin(b = 6)),
      axis.title = element_text(size = base_size),
      axis.text  = element_text(size = base_size - 1),
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.margin = margin(6, 6, 6, 6)
    )
}

# ==============================================================================
# 3. Helper Functions (prediction, policy id, PSD covariance)
# ==============================================================================
predict_prob <- function(model, newdata) {
  levs <- model$levels; stopifnot(length(levs) == 2)
  pos <- levs[2]
  as.numeric(predict(model, newdata = newdata, type = "prob")[, pos])
}
get_policy_id <- function(data, row_id) {
  if ("policy_id" %in% names(data)) as.character(data[row_id, "policy_id", drop = TRUE]) else as.character(row_id)
}
make_posdef <- function(S) {
  R <- try(chol(S), silent = TRUE)
  if (!inherits(R, "try-error")) return(S)
  lam <- 1e-10 * mean(diag(S), na.rm = TRUE); if (!is.finite(lam) || lam <= 0) lam <- 1e-8
  for (k in 1:6) {
    S2 <- S + diag(lam, nrow(S))
    R <- try(chol(S2), silent = TRUE)
    if (!inherits(R, "try-error")) return(S2)
    lam <- lam * 10
  }
  S
}

# ==============================================================================
# 4. Mahalanobis Weights & Distance Metrics
# ==============================================================================
# modes:
#  - "elasticity" (default): local finite-difference sensitivity of ŷ wrt vars
#  - "iqr"/"stdev"/"range": global scale heuristics
#  - "fixed": use provided weights_fixed (e.g., c(1,4))
compute_maha_weights <- function(model, row, data_all,
                                 mode = c("elasticity","fixed","iqr","stdev","range"),
                                 weights_fixed = c(1,1)) {
  mode <- match.arg(mode)
  ps_col <- "ps_rate"; ap_col <- "annual_prem"
  ps0 <- as.numeric(row[[ps_col]]); ap0 <- as.numeric(row[[ap_col]])

  norm_min1 <- function(v) {
    v <- as.numeric(v)
    if (any(!is.finite(v))) return(c(1,1))
    m <- suppressWarnings(min(v[v>0], na.rm = TRUE))
    if (!is.finite(m) || m <= 0) return(c(1,1))
    v / m
  }

  if (mode == "fixed") return(weights_fixed)

  ps_vals <- data_all[[ps_col]]; ap_vals <- data_all[[ap_col]]
  ps_vals <- ps_vals[is.finite(ps_vals)]; ap_vals <- ap_vals[is.finite(ap_vals)]

  if (mode %in% c("iqr","stdev","range")) {
    ps_s <- switch(mode,
                   iqr   = IQR(ps_vals, na.rm = TRUE),
                   stdev = stats::sd(ps_vals, na.rm = TRUE),
                   range = diff(stats::quantile(ps_vals, c(0.01, 0.99), na.rm = TRUE))
    )
    ap_s <- switch(mode,
                   iqr   = IQR(ap_vals, na.rm = TRUE),
                   stdev = stats::sd(ap_vals, na.rm = TRUE),
                   range = diff(stats::quantile(ap_vals, c(0.01, 0.99), na.rm = TRUE))
    )
    w_ps <- ifelse(is.finite(ps_s) && ps_s>0, ps_s, 1)
    w_ap <- ifelse(is.finite(ap_s) && ap_s>0, ap_s, 1)
    return(norm_min1(c(w_ps, w_ap)))
  }

  # elasticity mode
  q_ps <- stats::quantile(ps_vals, c(0.01,0.99), na.rm = TRUE)
  q_ap <- stats::quantile(ap_vals, c(0.01,0.99), na.rm = TRUE)
  eps_ps <- max(1e-6, 0.001 * diff(q_ps))
  eps_ap <- max(1e-6, 0.001 * diff(q_ap))

  p0 <- predict_prob(model, row)
  r_ps <- row; r_ps[[ps_col]] <- ps0 + eps_ps
  r_ap <- row; r_ap[[ap_col]] <- ap0 + eps_ap
  p_ps <- predict_prob(model, r_ps)
  p_ap <- predict_prob(model, r_ap)
  d_ps <- abs((p_ps - p0) / eps_ps)
  d_ap <- abs((p_ap - p0) / eps_ap)
  d_ps <- ifelse(!is.finite(d_ps) || d_ps <= 0, 1e-8, d_ps)
  d_ap <- ifelse(!is.finite(d_ap) || d_ap <= 0, 1e-8, d_ap)

  norm_min1(c(d_ps, d_ap))
}

# ==============================================================================
# 5. 1D Transition Curve
# ==============================================================================
transition_plot_1d <- function(model, data, row_id, feature,
                               grid_points = 200, thresh = 0.5,
                               x_cap = NULL, title_suffix = NULL) {
  stopifnot(is.numeric(data[[feature]]))
  x0   <- data[row_id, , drop = FALSE]
  pid  <- get_policy_id(data, row_id)
  xold <- x0[[feature]]
  yhat_old <- predict_prob(model, x0)

  rng <- range(data[[feature]], na.rm = TRUE)
  grid <- seq(rng[1], rng[2], length.out = grid_points)
  newdat <- x0[rep(1, grid_points), , drop = FALSE]
  newdat[[feature]] <- grid
  yhat_vec <- predict_prob(model, newdat)

  idx <- which(diff(yhat_vec >= thresh) != 0)
  cx  <- if (length(idx)) approx(yhat_vec[idx + 0:1], grid[idx + 0:1], xout = thresh)$y else NA_real_

  df_point <- data.frame(x = xold, y = yhat_old)
  df_cross <- if (!is.na(cx)) data.frame(x = cx, y = 0.97) else NULL

  g <- ggplot(data.frame(x = grid, yhat = yhat_vec), aes(x, yhat)) +
    geom_hline(yintercept = thresh, linetype = "dashed", color = "#9A9A9A", linewidth = 0.5) +
    geom_line(linewidth = 1.1, color = kuleuven_blue)
  g <- g + geom_point(data = df_point, aes(x = x, y = y), size = 2.6, color = "black")
  if (!is.null(df_cross)) {
    g <- g + annotate("label", x = df_cross$x, y = df_cross$y,
                      label = paste0("Crossing at ", feature, " ≈ ", signif(df_cross$x, 4)),
                      size = 3, vjust = 1, label.size = 0, fill = "white", alpha = 0.9, color = paper_gray)
  }
  x_max <- if (is.null(x_cap)) max(grid) else min(max(grid), x_cap)

  g +
    coord_cartesian(xlim = c(min(grid), x_max), ylim = c(0, 1)) +
    labs(
      title = paste0("Policy ID: ", pid, " — 1D response ", feature),
      subtitle = "Dashed line shows contour: ŷ = 0.5.",
      x = feature, y = "ŷ (predicted lapse probability)"
    ) +
    theme_paper()
}

# ==============================================================================
# 6. 2D Transition Surface — Overview
# ==============================================================================

transition_surface_2d_main <- function(model, data, row_id,
                                       feat_x = "ps_rate", feat_y = "annual_prem",
                                       grid_points = 140,
                                       thresh = 0.5,
                                       x_cap = 0.25, y_cap = 2500,
                                       show_example_boundary = TRUE) {
  base_row <- data[row_id, , drop = FALSE]
  pid <- get_policy_id(data, row_id)

  x_seq <- seq(min(data[[feat_x]], na.rm = TRUE), max(data[[feat_x]], na.rm = TRUE), length.out = grid_points)
  y_seq <- seq(min(data[[feat_y]], na.rm = TRUE), max(data[[feat_y]], na.rm = TRUE), length.out = grid_points)

  grid_df <- expand.grid(X = x_seq, Y = y_seq)
  newdat <- base_row[rep(1, nrow(grid_df)), , drop = FALSE]
  newdat[[feat_x]] <- grid_df$X
  newdat[[feat_y]] <- grid_df$Y
  grid_df$yhat <- predict_prob(model, newdat)

  orig_x <- base_row[[feat_x]]
  orig_y <- base_row[[feat_y]]
  yhat0  <- predict_prob(model, base_row)

  yr <- diff(range(y_seq)); lab_y <- min(orig_y + 0.04 * yr, y_cap * 0.98)

  p <- ggplot(grid_df, aes(X, Y, fill = yhat)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradientn(colours = c("#F5F5F5", kuleuven_blue),
                         values = scales::rescale(c(0, 0.5, 1)),
                         name = "ŷ", limits = c(0, 1))

  if (show_example_boundary) {
    p <- p + geom_contour(aes(z = yhat), breaks = thresh, color = "black", size = 0.6)
  }

  p +
    annotate("point", x = orig_x, y = orig_y, shape = 21, size = 3,
             fill = "black", color = "black", stroke = 0.3) +
    annotate("label", x = orig_x, y = lab_y,
             label = paste0("Point: ŷ=", percent(yhat0, 0.1)),
             label.size = 0, fill = "white", alpha = 0.92,
             size = 3, color = paper_gray, vjust = 0, hjust = 0.5) +
    coord_cartesian(
      xlim = c(min(x_seq, na.rm = TRUE), min(max(x_seq, na.rm = TRUE), x_cap)),
      ylim = c(0, min(max(y_seq, na.rm = TRUE), y_cap)),
      expand = FALSE
    ) +
    labs(
      title = paste0("Policy ID: ", pid, " — 2D transition surface"),
      subtitle = "Filled surface shows ŷ. The drawn isocontour (ŷ = 0.5) is an example decision boundary",
      x = feat_x, y = feat_y
    ) +
    theme_paper()
}

# ==============================================================================
# 7. 2D Transition Surface — Zoomed Minimal-Change Search
# ==============================================================================
transition_surface_2d_zoom <- function(model, data, row_id,
                                       feat_x = "ps_rate", feat_y = "annual_prem",
                                       grid_points = 140,
                                       grid_points_zoom = 260,
                                       thresh = 0.5,
                                       below_margin = 0.002,   # strict sub-threshold margin
                                       weights_mode = c("elasticity","fixed","iqr","stdev","range"),
                                       weights_fixed = c(1,1),
                                       y_floor_frac = 0.90,    # ≥90% of original premium
                                       ps_cap_lower = NA_real_,
                                       ps_cap_upper = NA_real_,
                                       zoom_pad_mult = 0.6,
                                       min_pad_frac  = 0.01,
                                       x_cap = 0.25, y_cap = 2500,
                                       verbose = TRUE) {
  weights_mode <- match.arg(weights_mode)
  perc2 <- function(p) sprintf("%.2f%%", 100*p)

  # ----- base + original values -----
  base_row <- data[row_id, , drop = FALSE]
  pid <- get_policy_id(data, row_id)
  orig_x <- base_row[[feat_x]]; orig_y <- base_row[[feat_y]]
  yhat_old <- predict_prob(model, base_row)

  if (verbose) cat(sprintf("[zoom] policy=%s | weights_mode=%s\n", pid, weights_mode))

  # ----- Mahalanobis geometry (original space) -----
  cov_xy <- stats::cov(data[, c(feat_x, feat_y)], use = "pairwise.complete.obs")
  cov_xy <- make_posdef(cov_xy)
  L <- try(chol(cov_xy), silent = TRUE); if (inherits(L, "try-error")) L <- chol(cov_xy + diag(1e-8, 2))

  # auto weights in whitened space
  w <- compute_maha_weights(model, base_row, data, mode = weights_mode, weights_fixed = weights_fixed)
  names(w) <- c("w_ps","w_ap")

  md2_w <- function(xy) {
    d <- c(xy[1] - orig_x, xy[2] - orig_y)
    z <- backsolve(L, d, transpose = TRUE)
    sum((w * z)^2)
  }

  # ----- bounds (+ ≥ y_floor_frac and optional ps caps) -----
  qx <- quantile(data[[feat_x]], c(0.01, 0.99), na.rm = TRUE)
  qy <- quantile(data[[feat_y]], c(0.01, 0.99), na.rm = TRUE)
  lb <- c(qx[1], max(0, qy[1])); ub <- c(qx[2], qy[2])
  names(lb) <- c(feat_x, feat_y); names(ub) <- c(feat_x, feat_y)
  lb[feat_y] <- max(lb[feat_y], y_floor_frac * orig_y)
  if (is.finite(ps_cap_lower)) lb[feat_x] <- max(lb[feat_x], ps_cap_lower)
  if (is.finite(ps_cap_upper)) ub[feat_x] <- min(ub[feat_x], ps_cap_upper)
  big <- 1e9; lb[!is.finite(lb)] <- -big; ub[!is.finite(ub)] <- big

  # ----- penalized objective (strict sub-threshold) -----
  pos_part <- function(u) ifelse(u > 0, u, 0)
  f_pen <- function(xy, lambda) {
    base_row[[feat_x]] <- xy[1]; base_row[[feat_y]] <- xy[2]
    yhat <- predict_prob(model, base_row)
    md2_w(xy) + lambda * (pos_part(yhat - (thresh - below_margin)))^2
  }

  # ----- coarse grid for second start -----
  x_seq <- seq(lb[feat_x], ub[feat_x], length.out = grid_points)
  y_seq <- seq(lb[feat_y], ub[feat_y], length.out = grid_points)
  grid_df <- expand.grid(X = x_seq, Y = y_seq)
  newdat <- base_row[rep(1, nrow(grid_df)), , drop = FALSE]
  newdat[[feat_x]] <- grid_df$X; newdat[[feat_y]] <- grid_df$Y
  grid_df$yhat <- predict_prob(model, newdat)
  nonlapse <- subset(grid_df, yhat <= (thresh - below_margin))
  start2 <- if (nrow(nonlapse)) {
    d2 <- apply(nonlapse[, c("X","Y")], 1, function(v) md2_w(c(v[1], v[2])))
    as.numeric(nonlapse[which.min(d2), c("X","Y")])
  } else c(orig_x, orig_y)
  starts <- rbind(c(orig_x, orig_y), start2)

  # ----- optimize with penalty escalation -----
  best <- NULL; best_val <- Inf
  for (lambda in c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8)) {
    for (k in 1:nrow(starts)) {
      st <- pmin(pmax(starts[k, ], lb), ub)
      opt <- optim(par = st, fn = function(x) f_pen(x, lambda),
                   method = "L-BFGS-B", lower = lb, upper = ub,
                   control = list(maxit = 300))
      if (opt$value < best_val) { best <- opt$par; best_val <- opt$value }
    }
    base_row[[feat_x]] <- best[1]; base_row[[feat_y]] <- best[2]
    yb <- predict_prob(model, base_row)
    if (yb <= (thresh - below_margin + 1e-4)) break
  }

  # ----- earliest strict crossing along the ray -----
  seg_prob <- function(t) {
    px <- orig_x + t * (best[1] - orig_x)
    py <- orig_y + t * (best[2] - orig_y)
    base_row[[feat_x]] <- px; base_row[[feat_y]] <- py
    predict_prob(model, base_row)
  }
  ts <- seq(0, 1, length.out = 400); ps <- vapply(ts, seg_prob, numeric(1))
  idx <- which(ps <= (thresh - below_margin))
  t_star <- if (length(idx)) {
    hi <- ts[min(idx)]; lo <- if (min(idx) == 1) 0 else ts[min(idx)-1]
    for (i in 1:60) { mid <- 0.5*(lo+hi); pm <- seg_prob(mid); if (pm <= (thresh - below_margin)) hi <- mid else lo <- mid; if ((hi-lo) < 1e-4) break }
    hi
  } else 1

  thr_x <- orig_x + t_star * (best[1] - orig_x)
  thr_y <- orig_y + t_star * (best[2] - orig_y)
  base_row[[feat_x]] <- thr_x; base_row[[feat_y]] <- thr_y
  yhat_new <- predict_prob(model, base_row)

  # ----- distances -----
  dx <- thr_x - orig_x; dy <- thr_y - orig_y
  raw_e <- sqrt(dx^2 + dy^2)
  inv_cov <- try(solve(cov_xy), silent = TRUE); if (inherits(inv_cov, "try-error")) inv_cov <- MASS::ginv(cov_xy)
  dvec <- c(dx, dy); dM <- sqrt(sum(dvec * as.vector(inv_cov %*% dvec)))

  # ----- zoom window -----
  xr <- diff(range(x_seq)); yr <- diff(range(y_seq))
  xpad <- max(xr * min_pad_frac, zoom_pad_mult * max(abs(dx), xr * 1e-6))
  ypad <- max(yr * min_pad_frac, zoom_pad_mult * max(abs(dy), yr * 1e-6))
  zx_min <- max(0, min(orig_x, thr_x) - xpad); zx_max <- min(x_cap, max(orig_x, thr_x) + xpad)
  zy_min <- max(0, min(orig_y, thr_y) - ypad); zy_max <- min(y_cap, max(orig_y, thr_y) + ypad)

  xz <- seq(zx_min, zx_max, length.out = grid_points_zoom)
  yz <- seq(zy_min, zy_max, length.out = grid_points_zoom)
  grid_zoom <- expand.grid(X = xz, Y = yz)
  newdat2 <- base_row[rep(1, nrow(grid_zoom)), , drop = FALSE]
  newdat2[[feat_x]] <- grid_zoom$X; newdat2[[feat_y]] <- grid_zoom$Y
  grid_zoom$yhat <- predict_prob(model, newdat2)

  info_text <- paste0(
    "Policy ID: ", pid, "\n",
    "ŷ_old=", perc2(yhat_old), "  →  ŷ_new=", perc2(yhat_new), " (<50%)\n",
    feat_x, "_old=", signif(orig_x, 4), "   ", feat_x, "_new=", signif(thr_x, 4), "\n",
    feat_y, "_old=", signif(orig_y, 4), "   ", feat_y, "_new=", signif(thr_y, 4), "\n",
    "weights (ps, ap) = (", paste0(signif(w,3), collapse=","), ")"
  )
  info_x <- zx_min + 0.02 * (zx_max - zx_min)
  info_y <- zy_max - 0.02 * (zy_max - zy_min)

  p <- ggplot(grid_zoom, aes(X, Y)) +
    geom_contour(aes(z = yhat), breaks = thresh, color = "black", linewidth = 0.9) +
    annotate("point", x = orig_x, y = orig_y, shape = 21, size = 3,
             fill = "black", color = "black", stroke = 0.3) +
    annotate("segment", x = orig_x + 0.00075, y = orig_y, xend = thr_x - 0.00075, yend = thr_y,
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             color = "black", linewidth = 0.8, linetype = "dashed") +
    annotate("point", x = thr_x, y = thr_y, shape = 21, size = 3, fill = "white", color = "black") +
    annotate("label", x = info_x, y = info_y, hjust = 0, vjust = 1,
             label = info_text, label.size = 0, fill = "white", alpha = 0.96,
             size = 3.1, color = "#3A3A3A") +
    scale_x_continuous(limits = c(zx_min, zx_max), n.breaks = 4) +
    scale_y_continuous(limits = c(zy_min, zy_max), n.breaks = 4) +
    labs(
      title = paste0("Policy ID: ", pid, " — Minimal distance to prevent lapse"),
      subtitle = paste0("Smallest change (Mahalanobis metric, ", weights_mode, ") with ", feat_y, " ≥ ", round(100*y_floor_frac), "% of original"),
      x = feat_x, y = feat_y
    ) +
    theme_classic(base_size = 12) +
    theme(
      text = element_text(color = "#3A3A3A"),
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "#EAEAEA", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

  rec <- tibble::tibble(
    policy_id = pid,
    y_hat_old = yhat_old,
    y_hat_new = yhat_new,
    ps_rate_old = orig_x,
    ps_rate_new = thr_x,
    ps_rate_delta = dx,
    annual_prem_old = orig_y,
    annual_prem_new = thr_y,
    annual_prem_delta = dy,
    raw_euclid = raw_e,
    d_mahalanobis = dM,
    weights_mode = weights_mode,
    weight_ps = w[1],
    weight_ap = w[2]
  )

  list(plot = p, recommendations = rec)
}

# ==================================================
# 3D Transition Box
# ==================================================
transition_3d_box <- function(model, data, row_id,
                              feat_x = "ps_rate",
                              feat_y = "annual_prem",
                              feat_z = "change_10",
                              grid_points = 28,
                              thresh = 0.5,
                              below_margin = 0.002,
                              weights = c(1, 4, 1.2),
                              y_floor_frac = 0.80,
                              x_cap = NULL, y_cap = NULL, z_cap = NULL) {
  base_row <- data[row_id, , drop = FALSE]
  pid <- get_policy_id(data, row_id)
  x0 <- base_row[[feat_x]]; y0 <- base_row[[feat_y]]; z0 <- base_row[[feat_z]]
  yhat_old <- predict_prob(model, base_row)

  qx <- quantile(data[[feat_x]], c(0.01,0.99), na.rm = TRUE)
  qy <- quantile(data[[feat_y]], c(0.01,0.99), na.rm = TRUE)
  qz <- quantile(data[[feat_z]], c(0.01,0.99), na.rm = TRUE)
  x_min <- qx[1]; x_max <- qx[2]; if (!is.null(x_cap)) x_max <- min(x_max, x_cap)
  y_min <- max(0, qy[1], y_floor_frac*y0); y_max <- qy[2]; if (!is.null(y_cap)) y_max <- min(y_max, y_cap)
  z_min <- qz[1]; z_max <- qz[2]; if (!is.null(z_cap)) z_max <- min(z_max, z_cap)

  x_rng <- if (x_min < x_max) c(x_min,x_max) else c(x0*0.8, x0*1.2)
  y_rng <- if (y_min < y_max) c(y_min,y_max) else c(y0*0.8, y0*1.2)
  z_rng <- if (z_min < z_max) c(z_min,z_max) else c(z0*0.8, z0*1.2)

  x_seq <- seq(x_rng[1], x_rng[2], length.out = grid_points)
  y_seq <- seq(y_rng[1], y_rng[2], length.out = grid_points)
  z_seq <- seq(z_rng[1], z_rng[2], length.out = grid_points)

  grid <- expand.grid(X = x_seq, Y = y_seq, Z = z_seq)
  newdat <- base_row[rep(1, nrow(grid)), , drop = FALSE]
  newdat[[feat_x]] <- grid$X; newdat[[feat_y]] <- grid$Y; newdat[[feat_z]] <- grid$Z
  grid$yhat <- predict_prob(model, newdat)

  cov_xyz <- stats::cov(data[, c(feat_x, feat_y, feat_z)], use = "pairwise.complete.obs")
  cov_xyz <- make_posdef(cov_xyz)
  L <- try(chol(cov_xyz), silent=TRUE); if (inherits(L,"try-error")) L <- chol(cov_xyz + diag(1e-8,3))
  wt <- weights
  md2_w <- function(x,y,z){
    d <- c(x - x0, y - y0, z - z0)
    zc <- backsolve(L, d, transpose = TRUE)
    sum((wt * zc)^2)
  }
  safe_cut <- thresh - below_margin
  safe <- subset(grid, yhat <= safe_cut)
  if (nrow(safe)) {
    i <- which.min(mapply(md2_w, safe$X, safe$Y, safe$Z))
    x1 <- safe$X[i]; y1 <- safe$Y[i]; z1 <- safe$Z[i]
  } else {
    i <- which.min(grid$yhat); x1 <- grid$X[i]; y1 <- grid$Y[i]; z1 <- grid$Z[i]
  }
  row_new <- base_row
  row_new[[feat_x]] <- x1; row_new[[feat_y]] <- y1; row_new[[feat_z]] <- z1
  yhat_new <- predict_prob(model, row_new)

  t_path <- seq(0, 1, length.out = 60)
  x_path <- x0 + t_path*(x1 - x0)
  y_path <- y0 + t_path*(y1 - y0)
  z_path <- z0 + t_path*(z1 - z0)

  vol_colorscale <- list(c(0, "#f5f5f5"), c(1, "#1E64C8"))
  p <- plot_ly()
  p <- add_trace(
    p, type = "volume",
    x = grid$X, y = grid$Y, z = grid$Z, value = grid$yhat,
    isomin = 0, isomax = 1, surface = list(count = 10),
    opacity = 0.10,
    opacityscale = list(list(0, 0.06), list(1, 0.12)),
    colorscale = vol_colorscale, showscale = TRUE, colorbar = list(title = "ŷ"),
    caps = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    name = "ŷ volume", showlegend = FALSE
  )
  p <- add_trace(
    p, type = "isosurface",
    x = grid$X, y = grid$Y, z = grid$Z, value = grid$yhat,
    isomin = thresh, isomax = thresh, surface = list(show = TRUE, count = 1),
    opacity = 0.28, showscale = FALSE, name = "ŷ = threshold",
    colorscale = list(c(0, "#BDBDBD"), c(1, "#BDBDBD")),
    lighting = list(ambient = 1, diffuse = 0, specular = 0, roughness = 1, fresnel = 0),
    lightposition = list(x = 0, y = 0, z = 0),
    caps = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE))
  )
  p <- add_markers(p, x = x0, y = y0, z = z0,
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 4, color = "black"),
                   name = "Original", showlegend = TRUE)
  p <- add_markers(p, x = x1, y = y1, z = z1,
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 5, color = "white", line = list(color = "black", width = 1.5)),
                   name = "Nearest non-lapse", showlegend = TRUE)
  p <- add_trace(p, type = "scatter3d", mode = "lines",
                 x = x_path, y = y_path, z = z_path,
                 line = list(width = 2, color = "black"),
                 hoverinfo = "skip", name = "Shortest path", showlegend = TRUE)
  p <- layout(
    p,
    title = paste0("Policy ID: ", pid, " — 3D transition (color = ŷ)"),
    scene = list(
      xaxis = list(title = feat_x, range = x_rng),
      yaxis = list(title = feat_y, range = y_rng),
      zaxis = list(title = feat_z, range = z_rng),
      aspectmode = "cube",
      camera = list(projection = list(type = "orthographic"))
    ),
    legend = list(orientation = "h", x = 0.02, y = -0.05)
  )

  inv_cov <- try(solve(cov_xyz), silent=TRUE); if (inherits(inv_cov,"try-error")) inv_cov <- MASS::ginv(cov_xyz)
  dvec <- c(x1-x0, y1-y0, z1-z0)
  rec <- tibble::tibble(
    policy_id      = pid,
    y_hat_old      = yhat_old,
    y_hat_new      = yhat_new,
    x_old = x0, x_new = x1,
    y_old = y0, y_new = y1,
    z_old = z0, z_new = z1,
    raw_euclid     = sqrt(sum(dvec^2)),
    d_mahalanobis  = sqrt(sum(dvec * as.vector(inv_cov %*% dvec)))
  )

  list(plot_3d = p, recommendations = rec)
}

# ==============================================================================
# 9. Parameters: Data/Model Load & Row Selection and Plot Generation
# ==============================================================================
# ---- Load model and data ----
setwd("D:/R Run")
load("lapses_data_pred")
model_xgb <- readRDS("model_RDS_xgb_downsamp_bigdata.rds")
df_prob <- lapses_data


row_to_plot <- 175025 # row of lapse policy 21963!

# 1D
p1d_ps   <- transition_plot_1d(model_xgb, df_prob, row_to_plot, "ps_rate",     x_cap = 0.25)
p1d_prem <- transition_plot_1d(model_xgb, df_prob, row_to_plot, "annual_prem", x_cap = 2500)
print(p1d_ps); print(p1d_prem)

# 2D surface (overview)
main_plot <- transition_surface_2d_main(model_xgb, df_prob, row_id = row_to_plot,
                                        feat_x = "ps_rate", feat_y = "annual_prem",
                                        x_cap = 0.25, y_cap = 2500,
                                        show_example_boundary = TRUE)
print(main_plot)

# 2D ZOOM with auto weights
out <- transition_surface_2d_zoom(model_xgb, df_prob, row_id = row_to_plot,
                                  feat_x = "ps_rate", feat_y = "annual_prem",
                                  below_margin = 0.002,
                                  weights_mode = "elasticity",
                                  weights_fixed = c(1,4),
                                  y_floor_frac = 0.80,
                                  ps_cap_lower = 0.00, ps_cap_upper = 0.25,
                                  x_cap = 0.25, y_cap = 2500,
                                  grid_points_zoom = 260,
                                  verbose = TRUE)
print(out$plot)
print(out$recommendations)
