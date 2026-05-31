################################################################################
# C3 — Portfolio Flip Analysis
#
# Purpose
# - Portfolio-level analysis of the minimal change required for each policy to
#   cross the decision boundary (flip) under practical constraints.
# - Computes per-policy axis-wise flips (ps_rate OR annual_prem) and a joint
#   2D flip using a Mahalanobis metric.
#
# What this script computes
# A) Axis-wise first crossing (coarse grid + bisection)
#    - A: Δ in ps_rate with annual_prem fixed (direction chosen by current label).
#    - B: Δ in annual_prem with ps_rate fixed.
# B) Joint 2D minimal-change search (method C)
#    - C(maha_opt): penalized optimization in whitened space (Mahalanobis),
#      with penalty ladder to enforce a strict safe region (ŷ ≤/≥ THR).
#    - C(ray): fallback 2D ray/bisection in scaled space if optimizer times out
#      or when distance ≠ "mahalanobis".
#
# Methods & Ingredients
# - Positive-class inference: auto-detects probability column (e.g., "Yes").
# - Scaling: none | range | z (used by ray search and distance calculations).
# - Distances: euclidean | manhattan | chebyshev | lp | mahalanobis | custom.
# - Auto-weights (Mahalanobis): "elasticity", "iqr", "stdev", "range", "fixed".
# - Bounds: data-driven min/max per feature with optional operational caps.
# - Constraints: annual_prem ≥ ap_floor_frac × original; ps_rate caps supported.
# - Parallelization: future.apply + multisession; progress via progressr.
# - Robustness: positive-definite adjustments for covariance; optional timeouts.
#
# Inputs & Assumptions
# - Data frame `lapses_data` with columns:
#     data_year, policy_id (or another id), ps_rate, annual_prem
# - Model `model` (caret) that supports `predict(..., type = "prob")`.
# - Decision threshold THR (default 0.5).
#
# Key Configuration (defaults shown above)
# - SCALE_METHOD, ANGLE_COUNT, GRID_N, MAX_ITER, TOL
# - Distance config (method, use_scaled, p, weights, S/custom_fun)
# - two_d_solver: "maha_opt" (preferred with Mahalanobis) or "ray"
# - Constraints: ap_floor_frac, ps_cap_lower/upper
# - Optimizer: maha_lambda (penalty ladder), maha_maxit, maha_timeout_sec
# - Parallel: workers (multisession), future_scheduling, BLAS/OMP pinned to 1
#
# Outputs
# - `res` (list):
#     $all        : tibble with per-row results (A_, B_, C_ deltas & distances)
#     $under_05   : subset where base ŷ < THR
#     $overeq_05  : subset where base ŷ ≥ THR
#     $bounds_used, $pos_prob_col, $workers_used, $distance, $params
# - Plots (examples):
#     p1–p4 for non-lapsed, p5–p8 for lapsed — histograms of |Δ| and distances.
#
# Columns of interest in `res$all` (abridged)
# - Base state: base_ps_rate, base_annual_prem, base_p, base_label
# - Axis flips:
#     A_* for ps_rate-only (Δ, distance, metric, relative-of-value/range)
#     B_* for annual_prem-only (Δ, distance, metric, relative-of-value/range)
# - Joint 2D:
#     C_new_ps_rate, C_new_annual_prem, C_delta_ps, C_delta_ap,
#     C_dist_metric (Mahalanobis or chosen metric), C_eucl_equiv, C_angle_deg
#
# Performance Tips
# - Reduce GRID_N / ANGLE_COUNT for exploratory runs; increase for finals.
# - Keep BLAS/OMP at 1 thread per worker to avoid oversubscription.
# - Use `distance$method = "mahalanobis"` + `two_d_solver = "maha_opt"` for
#   geometry-aware shortest moves; otherwise ray fallback is faster.
# - Consider setting `set.seed()` if using covariance subsampling.
#
# Reproducibility
# - Deterministic penalty ladder and bisection; set a global seed to stabilize
#   any internal sampling (e.g., covariance downsampling).
################################################################################
options(future.globals.maxSize= 1048576000)

# ==============================================================================
# 1. Packages
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(rlang)
  library(future.apply)
  library(caret)
  library(xgboost)
  library(progressr)
})

# ==============================================================================
# 2. Global Configuration & Constants, Helpers and Utils
# ==============================================================================
THR <- 0.5

DATA_MINMAX_BOUNDS <- function(df) {
  list(
    ps_rate     = c(min(df$ps_rate,     na.rm = TRUE),
                    max(df$ps_rate,     na.rm = TRUE)),
    annual_prem = c(min(df$annual_prem, na.rm = TRUE),
                    max(df$annual_prem, na.rm = TRUE))
  )
}

SCALE_METHOD <- "range"
ANGLE_COUNT <- 64
GRID_N <- 64
MAX_ITER <- 30
TOL      <- 1e-7

workers <- max(1, future::availableCores() - 1)
plan(multisession, workers = workers)

if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)
}

# Optional timeout support
have_Rutils <- requireNamespace("R.utils", quietly = TRUE)

set_progress_handlers <- function() {
  if (!requireNamespace("progressr", quietly = TRUE)) return(invisible(FALSE))
  if (requireNamespace("cli", quietly = TRUE)) {
    progressr::handlers(progressr::handler_cli)
  } else if (requireNamespace("progress", quietly = TRUE)) {
    progressr::handlers(progressr::handler_progress)
  } else {
    progressr::handlers(progressr::handler_txtprogressbar)
  }
  invisible(TRUE)
}

guess_pos_col <- function(model, df) {
  pr <- predict(model, newdata = df[1, , drop = FALSE], type = "prob")
  nms <- colnames(pr)
  if ("TRUE" %in% nms) return("TRUE")
  if ("Yes"  %in% nms) return("Yes")
  if ("1"    %in% nms) return("1")
  tail(nms, 1)
}

get_prob <- function(model, newdata, pos_col) {
  pr <- predict(model, newdata = newdata, type = "prob")
  as.numeric(pr[[pos_col]])
}

label_from_prob <- function(p, thr = THR) p >= thr

make_scalers <- function(df, bounds, method = "range") {
  if (method == "none") {
    return(list(
      to_s = function(apv, psv) c(apv, psv),
      from_s = function(aps, pss) c(aps, pss),
      ap_bounds_s = bounds$annual_prem,
      ps_bounds_s = bounds$ps_rate
    ))
  }
  if (method == "range") {
    ap_min <- bounds$annual_prem[1]; ap_max <- bounds$annual_prem[2]
    ps_min <- bounds$ps_rate[1];     ps_max <- bounds$ps_rate[2]
    ap_den <- if (ap_max > ap_min) (ap_max - ap_min) else 1
    ps_den <- if (ps_max > ps_min) (ps_max - ps_min) else 1
    return(list(
      to_s   = function(apv, psv) c((apv - ap_min)/ap_den, (psv - ps_min)/ps_den),
      from_s = function(aps, pss) c(aps*ap_den + ap_min,   pss*ps_den + ps_min),
      ap_bounds_s = c(0,1),
      ps_bounds_s = c(0,1)
    ))
  }
  if (method == "z") {
    ap_mu <- mean(df$annual_prem, na.rm=TRUE); ap_sd <- sd(df$annual_prem, na.rm=TRUE); if (ap_sd == 0) ap_sd <- 1
    ps_mu <- mean(df$ps_rate,     na.rm=TRUE); ps_sd <- sd(df$ps_rate,     na.rm=TRUE); if (ps_sd == 0) ps_sd <- 1
    ap_bounds_s <- c((min(df$annual_prem, na.rm=TRUE)-ap_mu)/ap_sd,
                     (max(df$annual_prem, na.rm=TRUE)-ap_mu)/ap_sd)
    ps_bounds_s <- c((min(df$ps_rate,     na.rm=TRUE)-ps_mu)/ps_sd,
                     (max(df$ps_rate,     na.rm=TRUE)-ps_mu)/ps_sd)
    return(list(
      to_s   = function(apv, psv) c((apv - ap_mu)/ap_sd, (psv - ps_mu)/ps_sd),
      from_s = function(aps, pss) c(aps*ap_sd + ap_mu,   pss*ps_sd + ps_mu),
      ap_bounds_s = ap_bounds_s,
      ps_bounds_s = ps_bounds_s
    ))
  }
  abort("Unknown SCALE_METHOD")
}

clone_with <- function(row, ap = NULL, ps = NULL) {
  r <- row
  if (!is.null(ap)) r$annual_prem <- ap
  if (!is.null(ps)) r$ps_rate     <- ps
  r
}

# ==============================================================================
# 3. Distance Factory (Euclidean / Manhattan / Chebyshev / Lp / Mahalanobis / Custom)
# ==============================================================================
make_distance <- function(space_df, scalers,
                          method = c("euclidean","manhattan","chebyshev","lp","mahalanobis","custom"),
                          use_scaled = TRUE, p = 2,
                          weights = c(1,1), S = NULL, custom_fun = NULL,
                          cov_sample_n = 50000) {
  method <- match.arg(method)
  stopifnot(length(weights) == 2)
  w <- as.numeric(weights)

  to_space <- if (isTRUE(use_scaled)) function(ap,ps) scalers$to_s(ap,ps) else function(ap,ps) c(ap,ps)

  make_cov <- function() {
    df <- space_df %>% dplyr::select(annual_prem, ps_rate) %>% dplyr::filter(is.finite(annual_prem), is.finite(ps_rate))
    if (nrow(df) == 0) return(diag(2))
    if (!is.null(cov_sample_n) && nrow(df) > cov_sample_n) {
      idx <- sample.int(nrow(df), cov_sample_n)
      df <- df[idx, , drop = FALSE]
    }
    xy <- t(vapply(seq_len(nrow(df)), function(i) to_space(df$annual_prem[i], df$ps_rate[i]), numeric(2)))
    stats::cov(xy, use = "pairwise.complete.obs")
  }

  L <- NULL
  if (identical(method, "mahalanobis")) {
    if (is.null(S)) S <- make_cov()
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
    S <- make_posdef(S)
    L <- try(chol(S), silent = TRUE)
    if (inherits(L, "try-error")) L <- chol(S + diag(1e-8, 2))
  }

  dist_fun <- switch(method,
                     euclidean = function(ap1,ps1, ap2,ps2) {
                       v1 <- to_space(ap1,ps1); v2 <- to_space(ap2,ps2)
                       sqrt(sum(w * (v1 - v2)^2))
                     },
                     manhattan = function(ap1,ps1, ap2,ps2) {
                       v1 <- to_space(ap1,ps1); v2 <- to_space(ap2,ps2)
                       sum(w * abs(v1 - v2))
                     },
                     chebyshev = function(ap1,ps1, ap2,ps2) {
                       v1 <- to_space(ap1,ps1); v2 <- to_space(ap2,ps2)
                       max(abs(v1 - v2) * w)
                     },
                     lp = function(ap1,ps1, ap2,ps2) {
                       v1 <- to_space(ap1,ps1); v2 <- to_space(ap2,ps2)
                       (sum(w * abs(v1 - v2)^p))^(1/p)
                     },
                     mahalanobis = function(ap1,ps1, ap2,ps2) {
                       d <- to_space(ap1,ps1) - to_space(ap2,ps2)
                       z <- backsolve(L, d, transpose = TRUE)
                       as.numeric(sqrt(sum((w * z)^2)))
                     },
                     custom = {
                       if (is.null(custom_fun)) stop("custom_fun must be provided when method='custom'")
                       function(ap1,ps1, ap2,ps2) custom_fun(ap1,ps1, ap2,ps2)
                     }
  )

  list(dist = dist_fun, name = method, meta = list(use_scaled = use_scaled, p = p, weights = w))
}

# Nearest crossing along ONE variable (coarse grid + bisection)
nearest_flip_axis <- function(row, var, direction, bound_min, bound_max,
                              model, pos_col, base_label, flip_to_label,
                              grid_n = GRID_N, thr = THR,
                              max_iter = MAX_ITER, tol = TOL) {
  x0 <- as.numeric(row[[var]])
  end <- if (direction == "down") bound_min else bound_max
  if (!is.finite(x0) || !is.finite(end) || x0 == end) {
    return(list(x = NA_real_, p = NA_real_))
  }

  xs <- seq(x0, end, length.out = grid_n + 1L)[-1]
  rgrid <- row[rep(1, length(xs)), , drop = FALSE]
  rgrid[[var]] <- xs
  lbls <- label_from_prob(get_prob(model, rgrid, pos_col), thr)

  idx <- which(lbls != base_label)[1]
  if (is.na(idx)) return(list(x = NA_real_, p = NA_real_))

  x_hi <- xs[idx]
  x_lo <- if (idx == 1L) x0 else xs[idx - 1L]

  test_lbl <- function(x) {
    rr <- row; rr[[var]] <- x
    label_from_prob(get_prob(model, rr, pos_col), thr)
  }

  left <- x_lo; right <- x_hi
  for (i in seq_len(max_iter)) {
    mid <- (left + right)/2
    if (test_lbl(mid) == base_label) left <- mid else right <- mid
    if (abs(right - left) <= tol * max(1, abs(mid))) break
  }
  x_star <- right
  r_star <- row; r_star[[var]] <- x_star
  p_star <- get_prob(model, r_star, pos_col)
  list(x = x_star, p = p_star)
}

# 2D ray search (fallback)
search_2d_minflip <- function(row, model, pos_col,
                              bounds, scalers,
                              base_label,
                              move_sign_ap,
                              move_sign_ps,
                              angle_count = ANGLE_COUNT,
                              thr = THR,
                              max_iter = MAX_ITER, tol = TOL,
                              dist_fun = NULL) {
  ap0 <- as.numeric(row$annual_prem); ps0 <- as.numeric(row$ps_rate)
  s0  <- scalers$to_s(ap0, ps0); ap0s <- s0[1]; ps0s <- s0[2]
  apL <- scalers$ap_bounds_s[1]; apU <- scalers$ap_bounds_s[2]
  psL <- scalers$ps_bounds_s[1]; psU <- scalers$ps_bounds_s[2]

  best_t <- Inf; best_xy <- c(NA_real_, NA_real_); best_p <- NA_real_; best_theta <- NA_real_
  best_d <- Inf

  thetas <- seq(0, pi/2, length.out = angle_count + 2L)[-c(1, angle_count + 2L)]
  for (theta in thetas) {
    d_ap <- move_sign_ap * cos(theta)
    d_ps <- move_sign_ps * sin(theta)

    t_max_ap <- if (d_ap > 0) (apU - ap0s)/d_ap else (apL - ap0s)/d_ap
    t_max_ps <- if (d_ps > 0) (psU - ps0s)/d_ps else (psL - ps0s)/d_ps
    t_max <- min(t_max_ap, t_max_ps)
    if (!is.finite(t_max) || t_max <= 0) next

    s_max <- c(ap0s + d_ap * t_max, ps0s + d_ps * t_max)
    o_max <- scalers$from_s(s_max[1], s_max[2])
    r_max <- clone_with(row, ap = o_max[1], ps = o_max[2])
    if (label_from_prob(get_prob(model, r_max, pos_col), thr) == base_label) next

    left <- 0; right <- t_max
    for (i in seq_len(max_iter)) {
      mid <- (left + right)/2
      s_mid <- c(ap0s + d_ap * mid, ps0s + d_ps * mid)
      o_mid <- scalers$from_s(s_mid[1], s_mid[2])
      r_mid <- clone_with(row, ap = o_mid[1], ps = o_mid[2])
      if (label_from_prob(get_prob(model, r_mid, pos_col), thr) == base_label) left <- mid else right <- mid
      if (abs(right - left) <= tol) break
    }
    t_star <- right
    s_star <- c(ap0s + d_ap * t_star, ps0s + d_ps * t_star)
    o_star <- scalers$from_s(s_star[1], s_star[2])
    r_star <- clone_with(row, ap = o_star[1], ps = o_star[2])
    p_star <- get_prob(model, r_star, pos_col)

    d_metric <- if (is.null(dist_fun)) NA_real_ else dist_fun(ap0, ps0, o_star[1], o_star[2])

    if (!is.na(d_metric) && (d_metric < best_d || (isTRUE(all.equal(d_metric, best_d)) && t_star < best_t))) {
      best_d <- d_metric
      best_t <- t_star; best_xy <- o_star; best_p <- p_star; best_theta <- theta
    }
  }

  list(
    ap = best_xy[1], ps = best_xy[2],
    p  = best_p,
    t_scaled = best_t,
    angle_rad = best_theta,
    d_metric = if (is.infinite(best_d)) NA_real_ else best_d
  )
}

# ------------------------- Precompute helpers --------------------------
.make_posdef <- function(S) {
  R <- try(chol(S), silent = TRUE)
  if (!inherits(R, "try-error")) return(S)
  lam <- 1e-10 * mean(diag(S), na.rm = TRUE); if (!is.finite(lam) || lam <= 0) lam <- 1e-8
  for (k in 1:6) {
    S2 <- S + diag(lam, nrow(S))
    R <- try(chol(S2), silent = TRUE)
    if (!inherits(R,"try-error")) return(S2)
    lam <- lam * 10
  }
  S
}

make_precomp_maha <- function(space_df) {
  df <- space_df %>% dplyr::select(ps_rate, annual_prem) %>%
    dplyr::filter(is.finite(ps_rate), is.finite(annual_prem))
  if (nrow(df) == 0) {
    return(list(
      L = chol(diag(2)),
      ps_min_global = 0, ps_max_global = 1,
      ap_min_global = 0, ap_max_global = 1
    ))
  }
  cov_xy <- stats::cov(df[, c("ps_rate","annual_prem")], use = "pairwise.complete.obs")
  cov_xy <- .make_posdef(cov_xy)
  L <- try(chol(cov_xy), silent = TRUE); if (inherits(L, "try-error")) L <- chol(cov_xy + diag(1e-8, 2))
  list(
    L = L,
    ps_min_global = min(df$ps_rate, na.rm = TRUE),
    ps_max_global = max(df$ps_rate, na.rm = TRUE),
    ap_min_global = min(df$annual_prem, na.rm = TRUE),
    ap_max_global = max(df$annual_prem, na.rm = TRUE)
  )
}

precompute_weight_stats <- function(space_df) {
  ps_vals <- space_df$ps_rate
  ap_vals <- space_df$annual_prem
  ps_vals <- ps_vals[is.finite(ps_vals)]
  ap_vals <- ap_vals[is.finite(ap_vals)]
  q_ps <- stats::quantile(ps_vals, c(0.01,0.99), na.rm = TRUE)
  q_ap <- stats::quantile(ap_vals, c(0.01,0.99), na.rm = TRUE)
  list(
    iqr_ps = IQR(ps_vals, na.rm = TRUE),
    iqr_ap = IQR(ap_vals, na.rm = TRUE),
    sd_ps  = stats::sd(ps_vals, na.rm = TRUE),
    sd_ap  = stats::sd(ap_vals, na.rm = TRUE),
    r01_ps = diff(q_ps),
    r01_ap = diff(q_ap),
    q_ps   = q_ps,
    q_ap   = q_ap
  )
}

# 2D Mahalanobis-opt search with constraints (precomputed L / bounds)
search_2d_maha_opt <- function(row, model, pos_col,
                               thr, flip_to,
                               L,
                               ps_min_global, ps_max_global,
                               ap_min_global, ap_max_global,
                               ap_floor_frac = 0,
                               ps_cap_lower = -Inf,
                               ps_cap_upper = +Inf,
                               grid_points = 64,
                               lambda_seq = c(1e3, 1e4, 1e5, 1e6),
                               maxit = 150,
                               weights = c(1,1)) {
  ap0 <- as.numeric(row$annual_prem); ps0 <- as.numeric(row$ps_rate)
  w <- as.numeric(weights)

  md2_w <- function(ps, ap) {
    d <- c(ps - ps0, ap - ap0)
    z <- backsolve(L, d, transpose = TRUE)
    sum((w * z)^2)
  }

  ap_min <- max(ap_min_global, ap0 * ap_floor_frac)
  ap_max <- ap_max_global
  ps_min <- max(ps_min_global, ps_cap_lower)
  ps_max <- min(ps_max_global, ps_cap_upper)

  safe_fun <- function(p) if (flip_to) (p - thr) else (thr - p) # <= 0 ok

  ps_seq <- seq(ps_min, ps_max, length.out = grid_points)
  ap_seq <- seq(ap_min, ap_max, length.out = grid_points)
  grid <- expand.grid(ps = ps_seq, ap = ap_seq)

  rgrid <- row[rep(1, nrow(grid)), , drop = FALSE]
  rgrid$ps_rate <- grid$ps; rgrid$annual_prem <- grid$ap
  p_grid <- get_prob(model, rgrid, pos_col)

  ok <- safe_fun(p_grid) <= 0
  starts <- rbind(c(ps0, ap0))
  if (any(ok)) {
    d2 <- mapply(md2_w, grid$ps[ok], grid$ap[ok])
    m <- which.min(d2)
    starts <- rbind(starts, c(grid$ps[ok][m], grid$ap[ok][m]))
  }

  best <- c(ps0, ap0); best_val <- Inf
  for (lambda in lambda_seq) {
    for (k in 1:nrow(starts)) {
      st <- pmin(pmax(starts[k,], c(ps_min, ap_min)), c(ps_max, ap_max))
      opt <- optim(par = st,
                   fn = function(x) {
                     ps <- x[1]; ap <- x[2]
                     val <- md2_w(ps, ap)
                     rr <- row; rr$ps_rate <- ps; rr$annual_prem <- ap
                     p  <- get_prob(model, rr, pos_col)
                     pen <- max(0, safe_fun(p))^2
                     val + lambda * pen
                   },
                   method = "L-BFGS-B", lower = c(ps_min, ap_min), upper = c(ps_max, ap_max),
                   control = list(maxit = maxit))
      if (opt$value < best_val) { best <- opt$par; best_val <- opt$value }
    }
    rr <- row; rr$ps_rate <- best[1]; rr$annual_prem <- best[2]
    if (safe_fun(get_prob(model, rr, pos_col)) <= 1e-4) break
  }

  seg_prob <- function(t) {
    ps <- ps0 + t * (best[1] - ps0)
    ap <- ap0 + t * (best[2] - ap0)
    rr <- row; rr$ps_rate <- ps; rr$annual_prem <- ap
    get_prob(model, rr, pos_col)
  }
  ts <- seq(0, 1, length.out = 200)
  prob_seq <- vapply(ts, seg_prob, numeric(1))
  if (flip_to) { crossed <- which(prob_seq >= thr) } else { crossed <- which(prob_seq < thr) }
  if (length(crossed)) {
    hi_idx <- min(crossed); lo_idx <- max(1, hi_idx - 1)
    lo <- ts[lo_idx]; hi <- ts[hi_idx]
    for (i in 1:60) {
      mid <- 0.5 * (lo + hi)
      pm <- seg_prob(mid)
      if ((flip_to && pm >= thr) || (!flip_to && pm < thr)) hi <- mid else lo <- mid
      if ((hi - lo) < 1e-6) break
    }
    t_star <- hi
  } else {
    t_star <- 1
  }
  ps_star <- ps0 + t_star * (best[1] - ps0)
  ap_star <- ap0 + t_star * (best[2] - ap0)
  rr <- row; rr$ps_rate <- ps_star; rr$annual_prem <- ap_star
  p_star <- get_prob(model, rr, pos_col)

  d_maha <- sqrt(md2_w(ps_star, ap_star))
  d_eucl <- sqrt((ps_star - ps0)^2 + (ap_star - ap0)^2)

  list(ps = ps_star, ap = ap_star, p = p_star,
       d_maha = d_maha, d_eucl = d_eucl)
}

relative_metrics <- function(delta, base, range_span) {
  tibble::tibble(
    rel_of_value = ifelse(is.na(base) | base == 0, NA_real_, delta / base),
    rel_of_range = ifelse(is.na(range_span) | range_span == 0, NA_real_, delta / range_span)
  )
}

# ==============================================================================
#4. Precompute Weight Statistics (IQR/SD/range & quantiles)
# ==============================================================================
compute_maha_weights <- function(model, row, pos_col, stats_precomp,
                                 mode = c("elasticity","fixed","iqr","stdev","range"),
                                 weights_fixed = c(1,1)) {
  mode <- match.arg(mode)
  ps_col <- "ps_rate"; ap_col <- "annual_prem"
  ps0 <- as.numeric(row[[ps_col]]); ap0 <- as.numeric(row[[ap_col]])

  norm_min1 <- function(v) {
    v <- as.numeric(v)
    if (any(!is.finite(v))) return(c(1,1))
    m <- min(v[v>0], na.rm = TRUE)
    if (!is.finite(m) || m <= 0) return(c(1,1))
    v / m
  }

  if (mode == "fixed") return(weights_fixed)

  if (mode %in% c("iqr","stdev","range")) {
    w_ps <- switch(mode,
                   iqr   = stats_precomp$iqr_ps,
                   stdev = stats_precomp$sd_ps,
                   range = stats_precomp$r01_ps
    )
    w_ap <- switch(mode,
                   iqr   = stats_precomp$iqr_ap,
                   stdev = stats_precomp$sd_ap,
                   range = stats_precomp$r01_ap
    )
    w_ps <- ifelse(is.finite(w_ps) && w_ps>0, w_ps, 1)
    w_ap <- ifelse(is.finite(w_ap) && w_ap>0, w_ap, 1)
    return(norm_min1(c(w_ps, w_ap)))
  }

  # Elasticity (local): finite-difference partials at the policy
  eps_ps <- max(1e-6, 0.001 * stats_precomp$r01_ps)
  eps_ap <- max(1e-6, 0.001 * stats_precomp$r01_ap)

  p0 <- get_prob(model, row, pos_col)
  r_ps <- row; r_ps[[ps_col]] <- ps0 + eps_ps
  r_ap <- row; r_ap[[ap_col]] <- ap0 + eps_ap
  p_ps <- get_prob(model, r_ps, pos_col)
  p_ap <- get_prob(model, r_ap, pos_col)
  d_ps <- abs((p_ps - p0) / eps_ps)
  d_ap <- abs((p_ap - p0) / eps_ap)
  d_ps <- ifelse(!is.finite(d_ps) || d_ps <= 0, 1e-8, d_ps)
  d_ap <- ifelse(!is.finite(d_ap) || d_ap <= 0, 1e-8, d_ap)

  norm_min1(c(d_ps, d_ap))
}

# ==============================================================================
# 5. Main Orchestrator: analyze_flip_distances()
# ==============================================================================
analyze_flip_distances <- function(lapses_data, model, year = 2003,
                                   thr = 0.5,
                                   bounds = NULL,
                                   scale_method = "range",
                                   angle_count = 64,
                                   grid_n = 64,
                                   id_col = "policy_id",
                                   distance = list(method = "euclidean",
                                                   use_scaled = !identical(scale_method, "none"),
                                                   p = 2,
                                                   weights = c(1,1),
                                                   S = NULL,
                                                   custom_fun = NULL),
                                   two_d_solver = c("maha_opt","ray"),
                                   ap_floor_frac = 0.0,
                                   ps_cap_lower = -Inf,
                                   ps_cap_upper = +Inf,
                                   progress_every = 2000,
                                   weights_mode = c("elasticity","fixed","iqr","stdev","range"),
                                   weights_maha = c(1,1),
                                   show_progress = TRUE,
                                   future_scheduling = Inf,     # fine-grained scheduling
                                   maha_maxit = 150,            # inner optimizer cap
                                   maha_timeout_sec = 20,       # per-row guard (requires R.utils for effect)
                                   maha_lambda = c(1e3,1e4,1e5,1e6) # penalty ladder
) {

  if (is.null(bounds)) {
    bounds <- list(
      ps_rate     = c(min(lapses_data$ps_rate,     na.rm=TRUE),
                      max(lapses_data$ps_rate,     na.rm=TRUE)),
      annual_prem = c(min(lapses_data$annual_prem, na.rm=TRUE),
                      max(lapses_data$annual_prem, na.rm=TRUE))
    )
  }

  df <- lapses_data %>% dplyr::filter(.data[["data_year"]] == year)
  if ("surrenders" %in% names(df)) df <- dplyr::select(df, -surrenders)
  df <- df %>% dplyr::mutate(across(where(is.logical), as.factor))

  pos_col <- guess_pos_col(model, lapses_data)
  scalers <- make_scalers(lapses_data, bounds, method = scale_method)
  two_d_solver <- match.arg(two_d_solver)
  weights_mode <- match.arg(weights_mode)

  ps_span <- diff(bounds$ps_rate)
  ap_span <- diff(bounds$annual_prem)

  message(sprintf("[analyze] year=%s | n=%d | workers=%d | thr=%.3f", year, nrow(df), workers, thr)); flush.console()
  message(sprintf("[analyze] 2D solver=%s | ap_floor_frac=%.2f | ps_cap=[%s,%s]", two_d_solver, ap_floor_frac,
                  ifelse(is.finite(ps_cap_lower), format(ps_cap_lower), "-Inf"),
                  ifelse(is.finite(ps_cap_upper), format(ps_cap_upper), "+Inf"))); flush.console()
  message(sprintf("[analyze] weights_mode=%s", weights_mode)); flush.console()

  # Distance (pluggable)
  dist_cfg <- do.call(make_distance, c(list(space_df = lapses_data, scalers = scalers), distance))
  dist_fun <- dist_cfg$dist

  # Precomputations (global)
  pre_maha <- make_precomp_maha(lapses_data)
  wstats   <- precompute_weight_stats(lapses_data)

  base_p <- get_prob(model, df, pos_col)
  base_y <- base_p >= thr

  idxs <- seq_len(nrow(df))
  n_tot <- length(idxs)

  use_progress <- isTRUE(show_progress) && requireNamespace("progressr", quietly = TRUE)
  if (use_progress) set_progress_handlers()

  # Worker
  .worker <- function(i, p = NULL) {
    if (!is.null(p)) p(sprintf("row %d of %d", i, n_tot)) else if (i %% progress_every == 0) {
      cat(sprintf("[progress] %d/%d\n", i, n_tot)); flush.console()
    }

    row <- df[i, , drop = FALSE]
    ap0 <- as.numeric(row$annual_prem); ps0 <- as.numeric(row$ps_rate)
    p0  <- base_p[i]; y0 <- base_y[i]

    if (!y0) { ps_dir <- "down"; ap_dir <- "up";   move_ap <- +1; move_ps <- -1; flip_to <- TRUE  }
    else     { ps_dir <- "up";   ap_dir <- "down"; move_ap <- -1; move_ps <- +1; flip_to <- FALSE }

    ps_res <- nearest_flip_axis(row, "ps_rate", ps_dir, bounds$ps_rate[1], bounds$ps_rate[2],
                                model, pos_col, y0, flip_to, grid_n = grid_n, thr = thr)
    A_new_ps <- ps_res$x; A_new_p <- ps_res$p
    A_d_ps   <- ifelse(is.na(A_new_ps), NA_real_, A_new_ps - ps0)
    A_dist   <- ifelse(is.na(A_d_ps), NA_real_, abs(A_d_ps))
    A_dist_metric <- ifelse(is.na(A_new_ps), NA_real_, dist_fun(ap0, ps0, ap0, A_new_ps))
    A_rel_ps <- relative_metrics(A_d_ps, ps0, ps_span)

    ap_res <- nearest_flip_axis(row, "annual_prem", ap_dir, bounds$annual_prem[1], bounds$annual_prem[2],
                                model, pos_col, y0, flip_to, grid_n = grid_n, thr = thr)
    B_new_ap <- ap_res$x; B_new_p <- ap_res$p
    B_d_ap   <- ifelse(is.na(B_new_ap), NA_real_, B_new_ap - ap0)
    B_dist   <- ifelse(is.na(B_d_ap), NA_real_, abs(B_d_ap))
    B_dist_metric <- ifelse(is.na(B_new_ap), NA_real_, dist_fun(ap0, ps0, B_new_ap, ps0))
    B_rel_ap <- relative_metrics(B_d_ap, ap0, ap_span)

    if (two_d_solver == "maha_opt" && distance$method == "mahalanobis") {
      w_row <- compute_maha_weights(model, row, pos_col, wstats,
                                    mode = weights_mode,
                                    weights_fixed = weights_maha)

      run_maha <- function() {
        search_2d_maha_opt(row, model, pos_col,
                           thr = thr, flip_to = !y0,
                           L = pre_maha$L,
                           ps_min_global = pre_maha$ps_min_global,
                           ps_max_global = pre_maha$ps_max_global,
                           ap_min_global = pre_maha$ap_min_global,
                           ap_max_global = pre_maha$ap_max_global,
                           ap_floor_frac = ap_floor_frac,
                           ps_cap_lower = ps_cap_lower,
                           ps_cap_upper = ps_cap_upper,
                           grid_points = max(32, ceiling(sqrt(grid_n))*2),
                           lambda_seq = maha_lambda,
                           maxit = maha_maxit,
                           weights = w_row)
      }

      Copt <- try({
        if (have_Rutils && is.finite(maha_timeout_sec) && maha_timeout_sec > 0) {
          R.utils::withTimeout(run_maha(), timeout = maha_timeout_sec, onTimeout = "silent")
        } else {
          run_maha()
        }
      }, silent = TRUE)

      if (inherits(Copt, "try-error") || is.null(Copt)) {
        C_res <- search_2d_minflip(row, model, pos_col, bounds, scalers,
                                   base_label = y0, move_sign_ap = move_ap, move_sign_ps = move_ps,
                                   angle_count = angle_count, thr = thr, dist_fun = dist_fun)
        C_ap <- C_res$ap; C_ps <- C_res$ps; C_p <- C_res$p
        C_d_ap <- ifelse(is.na(C_ap), NA_real_, C_ap - ap0)
        C_d_ps <- ifelse(is.na(C_ps), NA_real_, C_ps - ps0)
        C_dist_metric <- C_res$d_metric
        C_eucl_equiv  <- ifelse(any(is.na(c(C_d_ap,C_d_ps))), NA_real_, sqrt(C_d_ap^2 + C_d_ps^2))
        C_dist_scaled <- ifelse(is.finite(C_res$t_scaled), C_res$t_scaled, NA_real_)
        C_angle_deg   <- ifelse(is.na(C_res$angle_rad), NA_real_, C_res$angle_rad * 180/pi)
      } else {
        C_ps <- Copt$ps; C_ap <- Copt$ap; C_p <- Copt$p
        C_d_ps <- ifelse(is.na(C_ps), NA_real_, C_ps - ps0)
        C_d_ap <- ifelse(is.na(C_ap), NA_real_, C_ap - ap0)
        C_dist_metric <- Copt$d_maha
        C_eucl_equiv  <- Copt$d_eucl
        C_dist_scaled <- NA_real_
        C_angle_deg   <- NA_real_
      }
    } else {
      C_res <- search_2d_minflip(row, model, pos_col, bounds, scalers,
                                 base_label = y0, move_sign_ap = move_ap, move_sign_ps = move_ps,
                                 angle_count = angle_count, thr = thr, dist_fun = dist_fun)
      C_ap <- C_res$ap; C_ps <- C_res$ps; C_p <- C_res$p
      C_d_ap <- ifelse(is.na(C_ap), NA_real_, C_ap - ap0)
      C_d_ps <- ifelse(is.na(C_ps), NA_real_, C_ps - ps0)
      C_dist_metric <- C_res$d_metric
      C_eucl_equiv  <- ifelse(any(is.na(c(C_d_ap,C_d_ps))), NA_real_, sqrt(C_d_ap^2 + C_d_ps^2))
      C_dist_scaled <- ifelse(is.finite(C_res$t_scaled), C_res$t_scaled, NA_real_)
      C_angle_deg   <- ifelse(is.na(C_res$angle_rad), NA_real_, C_res$angle_rad * 180/pi)
    }

    tibble::tibble(
      row_id = i, !!id_col := row[[id_col]], data_year = row$data_year,
      base_ps_rate = ps0, base_annual_prem = ap0, base_p = p0, base_label = y0,
      A_new_ps_rate = A_new_ps, A_new_p = A_new_p, A_delta_ps = A_d_ps, A_dist = A_dist,
      A_dist_metric = A_dist_metric,
      A_rel_ps_of_value = A_rel_ps$rel_of_value, A_rel_ps_of_range = A_rel_ps$rel_of_range,
      B_new_annual_prem = B_new_ap, B_new_p = B_new_p, B_delta_ap = B_d_ap, B_dist = B_dist,
      B_dist_metric = B_dist_metric,
      B_rel_ap_of_value = B_rel_ap$rel_of_value, B_rel_ap_of_range = B_rel_ap$rel_of_range,
      C_new_ps_rate = C_ps, C_new_annual_prem = C_ap, C_new_p = C_p,
      C_delta_ps = C_d_ps, C_delta_ap = C_d_ap,
      C_dist_metric = C_dist_metric,
      C_eucl_equiv  = C_eucl_equiv,
      C_dist_scaled = C_dist_scaled,
      C_angle_deg = C_angle_deg,
      scale_method = scale_method,
      distance_method = dist_cfg$name,
      weights_mode = weights_mode
    )
  }

  if (use_progress) {
    out <- progressr::with_progress({
      p <- progressr::progressor(steps = n_tot)
      results <- future.apply::future_lapply(
        idxs, .worker, p = p,
        future.seed = TRUE,
        future.packages = c("caret","xgboost"),
        future.scheduling = future_scheduling
      )
      dplyr::bind_rows(results)
    })
  } else {
    results <- future.apply::future_lapply(
      idxs, .worker, p = NULL,
      future.seed = TRUE,
      future.packages = c("caret","xgboost"),
      future.scheduling = future_scheduling
    )
    out <- dplyr::bind_rows(results)
  }

  message("[analyze] completed. summarising..."); flush.console()
  list(
    all = out,
    under_05  = out %>% dplyr::filter(!base_label),
    overeq_05 = out %>% dplyr::filter( base_label),
    bounds_used = bounds,
    pos_prob_col = pos_col,
    workers_used = workers,
    distance = dist_cfg,
    params = list(ap_floor_frac = ap_floor_frac, ps_cap_lower = ps_cap_lower, ps_cap_upper = ps_cap_upper,
                  thr = thr, year = year, weights_mode = weights_mode,
                  future_scheduling = future_scheduling,
                  maha_maxit = maha_maxit, maha_timeout_sec = maha_timeout_sec, maha_lambda = maha_lambda)
  )
}

# ==============================================================================
# 6. Run Analysis: Parameters & Call
# ==============================================================================

res <- analyze_flip_distances(
  lapses_data, model_xgb, year = 2003,
  thr = 0.5,
  scale_method = "none",
  distance = list(method = "mahalanobis", use_scaled = FALSE),
  two_d_solver = "maha_opt",
  ap_floor_frac = 0.90,
  ps_cap_lower = 0.00, ps_cap_upper = 0.25,
  progress_every = 5000,
  weights_mode = "elasticity",
  show_progress = TRUE,
  future_scheduling = Inf,
  maha_maxit = 150,
  maha_timeout_sec = 20,
  maha_lambda = c(1e3,1e4,1e5,1e6)
)

# ==============================================================================
# 7. Visualization Helpers
# ==============================================================================

library(ggplot2)
ku_hist <- function(df, var, title, change_txt, xlab, bins = 100) {
  kuleuven_blue <- "#00A3E0"; kuleuven_blue_dark <- "#005B82"
  unreach <- mean(is.na(df[[var]])) * 100
  ggplot(dplyr::filter(df, !is.na(.data[[var]])), aes(x = .data[[var]])) +
    geom_histogram(bins = bins, fill = kuleuven_blue, color = kuleuven_blue_dark, alpha = 0.9) +
    labs(title = title, subtitle = sprintf("%s   •   Unreachable: %.1f%%", change_txt, unreach), x = xlab, y = "count") +
    theme_minimal(base_size = 12) +
    theme(plot.title.position = "plot", plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
}

# Non-lapsed (< thr)
p1 <- ku_hist(res$under_05, "A_dist",        "Non-lapsed: Δ ps_rate to first flip", "ps_rate ↓ (annual_prem fixed)", "|Δ ps_rate| (decrease)")
p2 <- ku_hist(res$under_05, "B_dist",        "Non-lapsed: Δ annual_prem to first flip", "annual_prem ↑ (ps_rate fixed)", "|Δ annual_prem| (increase)")
p3 <- ku_hist(res$under_05, "C_dist_metric", "Non-lapsed: 2D Mahalanobis distance", "joint (Mahalanobis-opt)", "Mahalanobis distance")
p4 <- ku_hist(res$under_05, "C_eucl_equiv",  "Non-lapsed: Euclidean @ Mahalanobis point", "same point as p3", "Euclidean distance")

# Lapsed (≥ thr)
p5 <- ku_hist(res$overeq_05, "A_dist",        "Lapsed: Δ ps_rate to first flip", "ps_rate ↑ (annual_prem fixed)", "|Δ ps_rate| (increase)")
p6 <- ku_hist(res$overeq_05, "B_dist",        "Lapsed: Δ annual_prem to first flip", "annual_prem ↓ (ps_rate fixed)", "|Δ annual_prem| (decrease)")
p7 <- ku_hist(res$overeq_05, "C_dist_metric", "Lapsed: 2D Mahalanobis distance", "joint (Mahalanobis-opt)", "Mahalanobis distance")
p8 <- ku_hist(res$overeq_05, "C_eucl_equiv",  "Lapsed: Euclidean @ Mahalanobis point", "same point as p7", "Euclidean distance")
