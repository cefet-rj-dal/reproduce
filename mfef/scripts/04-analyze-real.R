# =========================================================
# 04-analyze-real.R
# Real-data application (DATASUS). Runs MFEF+JP on the fixed
# short-list configuration (F, 12, 0.95), extracts the internal
# Joinpoint detections, and computes segment metrics
# (mean/variance/delta via block bootstrap; APC via GLS AR(1)),
# AAPC, and the fuzzy-score heatmap.
#
# Input : data/real/raw/txs_<tipo_dado>_<tipo_evento>.csv
# Output: console tables and figures
# =========================================================

library(harbinger)
library(daltoolbox)
library(daltoolboxdp)

library(ggplot2)
library(patchwork)
library(tidyverse)
library(lubridate)
library(nlme)
library(here)

# =========================================================
# Seed (same as the simulation for consistency)
# =========================================================
source(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/examples/seed.R"))
set_example_seed(42)

# =========================================================
# Data loading and filtering
# =========================================================

load_and_filter_data <- function(data_file, local = "Brasil", sexo = NULL, faixa_etaria = NULL) {
  if (grepl("mortalidade", data_file)) {
    df <- suppressMessages(
      readr::read_csv(data_file, locale = locale(encoding = "Latin1"))
    )
  } else {
    df <- suppressMessages(readr::read_csv(data_file))
  }
  df <- df %>%
    filter(!is.na(Local), Local == local, !is.na(Ano), !is.na(Mes))
  if (grepl("internacao", data_file)) {
    df <- df %>% filter(Ano >= 2009)
  }
  if (grepl("mortalidade", data_file)) {
    df <- df %>% filter(Ano >= 2000 & Ano < 2025)
  }
  if (!is.null(sexo) && !is.null(faixa_etaria)) {
    df <- df %>%
      filter(Sexo == sexo, Faixa_Etaria == faixa_etaria) %>%
      mutate(value = Taxa_Modificada)
  } else if (!is.null(sexo)) {
    df <- df %>%
      filter(Sexo == sexo) %>%
      mutate(value = Taxa_Padronizada_Por_Sexo)
  } else if (!is.null(faixa_etaria)) {
    df <- df %>%
      filter(Faixa_Etaria == faixa_etaria) %>%
      mutate(value = Taxa_Padronizada_Por_Faixa_Etaria)
  } else {
    df <- df %>%
      mutate(value = Taxa_Padronizada)
  }
  df <- df %>%
    mutate(
      Ano = as.integer(Ano),
      Mes = as.integer(pmin(pmax(round(Mes), 1), 12)),
      timestamp_str = paste0(Ano, "-", sprintf("%02d", Mes), "-01"),
      timestamp = suppressWarnings(as.Date(timestamp_str))
    ) %>%
    filter(!is.na(timestamp)) %>%
    select(timestamp, value) %>%
    arrange(timestamp) %>%
    distinct(timestamp, .keep_all = TRUE)
  df
}

# =========================================================
# Fixed short-list parameters (aligned with the simulation)
# =========================================================

REAL_TIME_TOLERANCE <- 12
REAL_THRESHOLD      <- 0.95
REAL_THRESHOLD_TYPE <- "percentile"
REAL_USE_NMS        <- TRUE
REAL_JP_LOG         <- FALSE

binseg_Q <- 20

bocpd_hazard       <- 100
bocpd_dist         <- "gaussian"
bocpd_threshold    <- 0.5
bocpd_min_distance <- 12
bocpd_burn_in      <- 6

joinpoint_min_between <- 6
joinpoint_min_end     <- 6
joinpoint_k_max       <- 7

# =========================================================
# Freeze helpers
# =========================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

hcp_frozen_detection <- function(detection, score = NULL) {
  if (!"event" %in% names(detection)) {
    stop("detection must contain an `event` column.")
  }
  events <- as.logical(detection$event)
  events[is.na(events)] <- FALSE
  if (is.null(score)) {
    score <- attr(detection, "score") %||% attr(detection, "res")
  }
  if (is.null(score) || length(score) != length(events)) {
    score <- as.numeric(events)
  }
  obj <- harbinger()
  obj$events <- events
  obj$score <- as.numeric(score)
  class(obj) <- append("hcp_frozen_detection", class(obj))
  obj
}

fit.hcp_frozen_detection <- function(obj, serie, ...) {
  if (is.null(serie)) stop("No data was provided", call. = FALSE)
  if (length(obj$events) != length(serie)) {
    stop("Frozen detection length differs from series length.", call. = FALSE)
  }
  obj <- obj$har_store_refs(obj, serie)
  obj
}

detect.hcp_frozen_detection <- function(obj, serie, ...) {
  if (is.null(serie)) stop("No data provided", call. = FALSE)
  if (length(obj$events) != length(serie)) {
    stop("Frozen detection length differs from series length.", call. = FALSE)
  }
  obj <- obj$har_store_refs(obj, serie)
  detection <- obj$har_restore_refs(
    obj,
    change_points = obj$events,
    res = obj$score
  )
  detection$event <- obj$events
  attr(detection, "score") <- obj$score
  attr(detection, "res") <- obj$score
  attr(detection, "type") <- ifelse(obj$events, "changepoint", "")
  detection
}

freeze_once <- function(model, serie, cfg = list()) {
  model     <- daltoolbox::fit(model, serie)
  detection <- run_detect(model, serie, cfg)
  hcp_frozen_detection(detection)
}

# =========================================================
# Detection wrapper
# =========================================================

run_detect <- function(model, serie, cfg = list()) {
  harbinger::detect(
    model,
    serie,
    time_tolerance = cfg$time_tolerance %||% REAL_TIME_TOLERANCE,
    threshold      = cfg$threshold %||% REAL_THRESHOLD,
    threshold_type = cfg$threshold_type %||% REAL_THRESHOLD_TYPE,
    use_nms        = cfg$use_nms %||% REAL_USE_NMS
  )
}

# =========================================================
# Series preprocessing (ensures strictly positive values)
# =========================================================

preprocess_series_for_methods <- function(serie) {
  if (!any(serie <= 0, na.rm = TRUE)) {
    return(list(serie = serie, shift = 0, applied = FALSE))
  }
  pos_vals <- serie[serie > 0]
  pos_vals <- pos_vals[!is.na(pos_vals)]
  epsilon <- if (length(pos_vals) > 0) {
    max(min(pos_vals, na.rm = TRUE) / 100, 1e-6)
  } else 1e-6
  min_val <- min(serie, na.rm = TRUE)
  shift <- epsilon - min_val
  serie_p <- serie + shift
  list(serie = serie_p, shift = shift, applied = TRUE)
}

# =========================================================
# Frozen banks
# =========================================================

build_ae_member_bank_frozen <- function(serie) {
  cfg <- list(time_tolerance = REAL_TIME_TOLERANCE,
              threshold = REAL_THRESHOLD,
              threshold_type = REAL_THRESHOLD_TYPE,
              use_nms = REAL_USE_NMS)
  list(
    WAYPOINT_ED = freeze_once(
      hcp_waypoint(
        input_size = 12, encode_size = 4,
        warmup = 60, retrain_size = 30, buffer_size = 40,
        k_factor = 0.35, h_low = 3.5, h_high = 6, prob_tau = 0.997,
        epochs_init = 100, epochs_retrain = 100,
        encoderclass = autoenc_ed
      ),
      serie, cfg
    ),
    WAYPOINT_LSTM = freeze_once(
      hcp_waypoint(
        input_size = 12, encode_size = 4,
        warmup = 60, retrain_size = 30, buffer_size = 40,
        k_factor = 0.35, h_low = 3.5, h_high = 6, prob_tau = 0.997,
        epochs_init = 100, epochs_retrain = 100,
        encoderclass = autoenc_lstm_ed
      ),
      serie, cfg
    ),
    WAYPOINT_CONV = freeze_once(
      hcp_waypoint(
        input_size = 12, encode_size = 4,
        warmup = 60, retrain_size = 30, buffer_size = 40,
        k_factor = 0.35, h_low = 3.5, h_high = 6, prob_tau = 0.997,
        epochs_init = 100, epochs_retrain = 100,
        encoderclass = autoenc_conv_ed
      ),
      serie, cfg
    )
  )
}

build_cf_member_bank_frozen <- function(serie) {
  cfg <- list(time_tolerance = REAL_TIME_TOLERANCE,
              threshold = REAL_THRESHOLD,
              threshold_type = REAL_THRESHOLD_TYPE,
              use_nms = REAL_USE_NMS)
  list(
    CF_ARIMA = freeze_once(hcp_cf_arima(), serie, cfg),
    CF_LR    = freeze_once(hcp_cf_lr(),    serie, cfg),
    CF_GARCH = freeze_once(hcp_garch(),    serie, cfg),
    CF_ETS   = freeze_once(hcp_cf_ets(),   serie, cfg)
  )
}

build_static_frozen_bank <- function(serie) {
  cfg <- list(time_tolerance = REAL_TIME_TOLERANCE,
              threshold = REAL_THRESHOLD,
              threshold_type = REAL_THRESHOLD_TYPE,
              use_nms = REAL_USE_NMS)
  bank <- list()
  bank$BINSEG <- freeze_once(hcp_binseg(Q = binseg_Q), serie, cfg)
  bank$PELT   <- freeze_once(hcp_pelt(), serie, cfg)
  bank$GFT    <- freeze_once(hcp_gft(), serie, cfg)
  bank$SCP    <- freeze_once(hcp_scp(), serie, cfg)
  bank$BOCPD  <- freeze_once(
    hcp_bocpd(
      hazard = bocpd_hazard,
      dist = bocpd_dist,
      threshold = bocpd_threshold,
      min_distance = bocpd_min_distance,
      burn_in = bocpd_burn_in
    ),
    serie, cfg
  )
  bank$JOINPOINT_FALSE <- freeze_once(
    hcp_joinpoint(
      min_between = joinpoint_min_between,
      min_end = joinpoint_min_end,
      k_max = joinpoint_k_max,
      log_transform = FALSE
    ),
    serie, cfg
  )
  bank
}

# =========================================================
# Ensemble constructors
# =========================================================

build_ae_ensemble_from_frozen <- function(ae_member_bank) {
  har_ensemble_fuzzy(
    WAYPOINT_ED   = ae_member_bank$WAYPOINT_ED,
    WAYPOINT_LSTM = ae_member_bank$WAYPOINT_LSTM,
    WAYPOINT_CONV = ae_member_bank$WAYPOINT_CONV
  )
}

build_cf_ensemble_from_frozen <- function(cf_member_bank) {
  har_ensemble_fuzzy(
    CF_ARIMA = cf_member_bank$CF_ARIMA,
    CF_LR    = cf_member_bank$CF_LR,
    CF_GARCH = cf_member_bank$CF_GARCH,
    CF_ETS   = cf_member_bank$CF_ETS
  )
}

build_level2_mfef_jp <- function(ae_frozen, cf_frozen, static_bank) {
  har_ensemble_fuzzy(
    BINSEG      = static_bank$BINSEG,
    PELT        = static_bank$PELT,
    GFT         = static_bank$GFT,
    SCP         = static_bank$SCP,
    BOCPD       = static_bank$BOCPD,
    ENSEMBLE_AE = ae_frozen,
    ENSEMBLE_CF = cf_frozen,
    JOINPOINT   = static_bank$JOINPOINT_FALSE
  )
}

# =========================================================
# Extract Joinpoint detections from within the ensemble
# =========================================================

extract_joinpoint_dates <- function(detection, timestamps) {
  model_events <- attr(detection, "model_events")
  if (is.null(model_events) || is.null(model_events$JOINPOINT)) {
    return(as.Date(character(0)))
  }
  jp_events <- as.logical(model_events$JOINPOINT$raw)
  jp_events[is.na(jp_events)] <- FALSE
  timestamps[jp_events]
}

# =========================================================
# Sub-threshold fuzzy-score peaks (qualitative)
# =========================================================

find_subthreshold_peaks <- function(score, timestamps, threshold_value,
                                    min_peak_height = 0.2,
                                    min_distance = 12) {
  n <- length(score)
  peaks <- integer(0)
  for (i in 2:(n - 1)) {
    if (score[i] > score[i - 1] && score[i] >= score[i + 1]) {
      if (score[i] >= min_peak_height && score[i] < threshold_value) {
        peaks <- c(peaks, i)
      }
    }
  }
  if (length(peaks) > 1) {
    peaks_ordered <- peaks[order(score[peaks], decreasing = TRUE)]
    selected <- integer(0)
    for (p in peaks_ordered) {
      if (length(selected) == 0 || all(abs(p - selected) >= min_distance)) {
        selected <- c(selected, p)
      }
    }
    peaks <- sort(selected)
  }
  tibble(
    timestamp = timestamps[peaks],
    score = round(score[peaks], 3)
  )
}

# =========================================================
# Simple delta-metrics (magnitude) per change-point
# =========================================================

compute_delta_metrics <- function(serie, detected_positions, timestamps) {
  if (length(detected_positions) == 0) {
    return(tibble())
  }
  n <- length(serie)
  breakpoints <- sort(detected_positions)
  segment_starts <- c(1, breakpoints)
  segment_ends   <- c(breakpoints - 1, n)
  results <- list()
  for (i in seq_along(breakpoints)) {
    cp_pos <- breakpoints[i]
    before_start <- segment_starts[i]
    before_end   <- cp_pos - 1
    after_start  <- cp_pos
    after_end    <- segment_ends[i + 1]
    before_vals <- serie[before_start:before_end]
    after_vals  <- serie[after_start:after_end]
    mean_before <- mean(before_vals, na.rm = TRUE)
    mean_after  <- mean(after_vals, na.rm = TRUE)
    delta_mean  <- mean_after - mean_before
    pct_change  <- 100 * delta_mean / mean_before
    results[[i]] <- tibble(
      change_point = timestamps[cp_pos],
      mean_before = round(mean_before, 3),
      mean_after = round(mean_after, 3),
      delta_mean = round(delta_mean, 3),
      pct_change = round(pct_change, 1)
    )
  }
  bind_rows(results)
}

# =========================================================
# Segment metrics with autocorrelation correction.
# mean, variance, deltas: block bootstrap.
# APC: GLS with AR(1) correlation structure.
# =========================================================

compute_segment_metrics <- function(serie, change_positions, timestamps,
                                    periods_per_year = 12,
                                    B = 1000) {
  n <- length(serie)
  if (length(change_positions) == 0) {
    breakpoints <- integer(0)
  } else {
    breakpoints <- sort(change_positions)
  }
  segment_starts <- c(1, breakpoints)
  segment_ends   <- c(breakpoints - 1, n)
  n_segments <- length(segment_starts)
  
  # Block bootstrap for a single statistic
  block_boot <- function(vals, stat_fn, B = 1000) {
    vals <- vals[!is.na(vals)]
    m <- length(vals)
    if (m < 4) return(c(NA, NA))
    L <- max(2, floor(m^(1/3)))          # block length
    n_blocks <- ceiling(m / L)
    boot_vals <- numeric(B)
    for (b in seq_len(B)) {
      idx <- c()
      for (k in seq_len(n_blocks)) {
        start <- sample(1:(m - L + 1), 1)
        idx <- c(idx, start:(start + L - 1))
      }
      idx <- idx[1:m]
      boot_vals[b] <- stat_fn(vals[idx])
    }
    quantile(boot_vals, c(0.025, 0.975), na.rm = TRUE)
  }
  
  # APC via GLS AR(1)
  apc_gls <- function(vals, ppy = 12) {
    vals <- vals[!is.na(vals)]
    if (length(vals) < 3 || any(vals <= 0)) {
      return(c(apc = NA, lower = NA, upper = NA))
    }
    tt <- seq_along(vals)
    dat <- data.frame(y = log(vals), t = tt)
    fit <- tryCatch(
      gls(y ~ t, data = dat, correlation = corAR1(form = ~ t)),
      error = function(e) NULL
    )
    if (is.null(fit)) return(c(apc = NA, lower = NA, upper = NA))
    beta <- as.numeric(coef(fit)[2])
    se   <- as.numeric(sqrt(vcov(fit)[2, 2]))
    beta_a <- beta * ppy
    se_a   <- se * ppy
    apc  <- (exp(beta_a) - 1) * 100
    lo   <- (exp(beta_a - 1.96 * se_a) - 1) * 100
    hi   <- (exp(beta_a + 1.96 * se_a) - 1) * 100
    c(apc = apc, lower = lo, upper = hi)
  }
  
  segment_results <- list()
  for (i in seq_len(n_segments)) {
    seg_start <- segment_starts[i]
    seg_end   <- segment_ends[i]
    seg_vals  <- serie[seg_start:seg_end]
    duration  <- seg_end - seg_start + 1
    seg_mean <- mean(seg_vals, na.rm = TRUE)
    seg_var  <- var(seg_vals, na.rm = TRUE)
    seg_n    <- sum(!is.na(seg_vals))
    mean_ci_bounds <- block_boot(seg_vals, function(x) mean(x, na.rm = TRUE), B)
    mean_lower <- mean_ci_bounds[1]
    mean_upper <- mean_ci_bounds[2]
    var_ci_bounds <- block_boot(seg_vals, function(x) var(x, na.rm = TRUE), B)
    var_lower <- var_ci_bounds[1]
    var_upper <- var_ci_bounds[2]
    apc_res <- apc_gls(seg_vals, periods_per_year)
    segment_results[[i]] <- tibble(
      segment = i,
      start_date = timestamps[seg_start],
      end_date = timestamps[seg_end],
      duration_months = duration,
      segment_mean = round(seg_mean, 3),
      mean_lower = round(mean_lower, 3),
      mean_upper = round(mean_upper, 3),
      mean_ci = sprintf("%.3f [%.3f, %.3f]", seg_mean, mean_lower, mean_upper),
      apc = round(apc_res["apc"], 2),
      apc_lower = round(apc_res["lower"], 2),
      apc_upper = round(apc_res["upper"], 2),
      apc_ci = sprintf("%.2f [%.2f, %.2f]", apc_res["apc"], apc_res["lower"], apc_res["upper"]),
      segment_var = round(seg_var, 5),
      var_lower = round(var_lower, 5),
      var_upper = round(var_upper, 5),
      var_ci = sprintf("%.5f [%.5f, %.5f]", seg_var, var_lower, var_upper),
      seg_vals_list = list(seg_vals),
      seg_n = seg_n
    )
  }
  segments_tbl <- bind_rows(segment_results)
  valid_apc <- !is.na(segments_tbl$apc)
  if (any(valid_apc)) {
    aapc <- sum(segments_tbl$apc[valid_apc] * segments_tbl$duration_months[valid_apc]) /
      sum(segments_tbl$duration_months[valid_apc])
  } else {
    aapc <- NA_real_
  }
  
  # Block bootstrap for a difference of statistics between two segments
  block_boot_delta <- function(v1, v2, stat_fn, B = 1000) {
    v1 <- v1[!is.na(v1)]; v2 <- v2[!is.na(v2)]
    m1 <- length(v1); m2 <- length(v2)
    if (m1 < 4 || m2 < 4) return(c(NA, NA))
    L1 <- max(2, floor(m1^(1/3)))
    L2 <- max(2, floor(m2^(1/3)))
    boot_diff <- numeric(B)
    for (b in seq_len(B)) {
      idx1 <- c()
      for (k in seq_len(ceiling(m1 / L1))) {
        s <- sample(1:(m1 - L1 + 1), 1)
        idx1 <- c(idx1, s:(s + L1 - 1))
      }
      idx1 <- idx1[1:m1]
      idx2 <- c()
      for (k in seq_len(ceiling(m2 / L2))) {
        s <- sample(1:(m2 - L2 + 1), 1)
        idx2 <- c(idx2, s:(s + L2 - 1))
      }
      idx2 <- idx2[1:m2]
      boot_diff[b] <- stat_fn(v2[idx2]) - stat_fn(v1[idx1])
    }
    quantile(boot_diff, c(0.025, 0.975), na.rm = TRUE)
  }
  
  delta_results <- list()
  if (n_segments >= 2) {
    for (i in 1:(n_segments - 1)) {
      v1 <- segments_tbl$seg_vals_list[[i]]
      v2 <- segments_tbl$seg_vals_list[[i + 1]]
      delta_mean <- mean(v2, na.rm = TRUE) - mean(v1, na.rm = TRUE)
      dm_ci <- block_boot_delta(v1, v2, function(x) mean(x, na.rm = TRUE), B)
      dm_sig <- ifelse(!is.na(dm_ci[1]) && (dm_ci[1] > 0 | dm_ci[2] < 0), "*", "ns")
      delta_var <- var(v2, na.rm = TRUE) - var(v1, na.rm = TRUE)
      dv_ci <- block_boot_delta(v1, v2, function(x) var(x, na.rm = TRUE), B)
      dv_sig <- ifelse(!is.na(dv_ci[1]) && (dv_ci[1] > 0 | dv_ci[2] < 0), "*", "ns")
      delta_results[[i]] <- tibble(
        transition = paste0("Seg", i, " -> Seg", i + 1),
        change_point = timestamps[breakpoints[i]],
        delta_mean = round(delta_mean, 3),
        delta_mean_lower = round(dm_ci[1], 3),
        delta_mean_upper = round(dm_ci[2], 3),
        delta_mean_ci = sprintf("%.3f [%.3f, %.3f]", delta_mean, dm_ci[1], dm_ci[2]),
        delta_mean_sig = dm_sig,
        delta_apc = round(segments_tbl$apc[i + 1] - segments_tbl$apc[i], 2),
        delta_var = round(delta_var, 5),
        delta_var_lower = round(dv_ci[1], 5),
        delta_var_upper = round(dv_ci[2], 5),
        delta_var_ci = sprintf("%.5f [%.5f, %.5f]", delta_var, dv_ci[1], dv_ci[2]),
        delta_var_sig = dv_sig
      )
    }
  }
  delta_tbl <- bind_rows(delta_results)
  segments_tbl <- segments_tbl %>% select(-seg_vals_list, -seg_n)
  list(
    segments = segments_tbl,
    aapc = round(aapc, 2),
    deltas = delta_tbl
  )
}

# =========================================================
# Series heatmap coloured by the final fuzzy score.
# Smooth vertical bands (interpolated score) with the series
# on top. X axis at round years; Y axis with fixed steps.
# =========================================================

plot_series_fuzzy_heatmap <- function(serie, timestamps, fuzzy_score,
                                      threshold_value = NULL,
                                      palette = c("white", "yellow", "red"),
                                      n_interp = 3000,
                                      show_axis_titles = FALSE,
                                      x_break_years = "auto",   # "auto", 5 or 10
                                      auto_threshold_years = 20,
                                      y_n_breaks = 1) {
  stopifnot(length(serie) == length(fuzzy_score),
            length(serie) == length(timestamps))
  df_line <- tibble(
    timestamp = timestamps,
    value     = as.numeric(serie),
    score     = as.numeric(fuzzy_score)
  ) %>%
    dplyr::filter(!is.na(value), !is.na(score))
  if (nrow(df_line) == 0) stop("Empty data frame after removing NAs.")
  limiar <- threshold_value
  
  # Fine time grid for the smooth horizontal heatmap
  t_num  <- as.numeric(df_line$timestamp)
  grid_t <- seq(min(t_num), max(t_num), length.out = n_interp)
  grid_s <- approx(x = t_num, y = df_line$score, xout = grid_t, rule = 2)$y
  tile_w <- (max(t_num) - min(t_num)) / n_interp
  df_heat <- tibble(
    timestamp = as.Date(grid_t, origin = "1970-01-01"),
    score     = grid_s
  )
  
  # Vertical geometry: bands span the full height
  x_range  <- as.Date(range(df_line$timestamp))
  y_min    <- min(df_line$value)
  y_max    <- max(df_line$value)
  y_center <- (y_min + y_max) / 2
  y_height <- (y_max - y_min) * 1.30
  
  # Automatic X-axis spacing
  span_years <- as.numeric(difftime(x_range[2], x_range[1], units = "days")) / 365.25
  if (identical(x_break_years, "auto")) {
    x_break_years <- if (span_years > auto_threshold_years) 10 else 5
  }
  
  # X breaks anchored at round years (multiples of 5/10)
  yr_min <- as.integer(format(x_range[1], "%Y"))
  yr_max <- as.integer(format(x_range[2], "%Y"))
  first_break <- ceiling(yr_min / x_break_years) * x_break_years
  last_break  <- floor(yr_max  / x_break_years) * x_break_years
  break_years <- seq(first_break, last_break, by = x_break_years)
  x_breaks    <- as.Date(paste0(break_years, "-01-01"))
  
  # Legend with threshold mark
  if (!is.null(limiar)) {
    br  <- c(0, limiar, 1)
    lbl <- c("0", paste0("Threshold = ", round(limiar, 3)), "1")
  } else {
    br  <- c(0, 0.5, 1)
    lbl <- c("0", "0.5", "1")
  }
  x_lab <- if (show_axis_titles) "Year" else NULL
  y_lab <- if (show_axis_titles) "Rate" else NULL
  
  ggplot() +
    geom_tile(data = df_heat,
              aes(x = timestamp, y = y_center, fill = score),
              height = y_height, width = tile_w) +
    geom_line(data = df_line, aes(x = timestamp, y = value),
              color = "#222222", linewidth = 0.5) +
    geom_point(data = df_line, aes(x = timestamp, y = value),
               color = "#222222", size = 0.5) +
    scale_fill_gradientn(
      colors = palette, limits = c(0, 1),
      breaks = br, labels = lbl, name = "Fuzzy score"
    ) +
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%Y",
      limits = x_range,
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
      breaks = seq(floor(y_min), ceiling(y_max), by = 0.1),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(x = x_lab, y = y_lab) +
    guides(
      fill = guide_colorbar(
        direction    = "horizontal",
        barwidth     = unit(5, "cm"),
        barheight    = unit(0.25, "cm"),
        ticks        = TRUE,
        ticks.colour = "black"
      )
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 0, hjust = 0.5),
      legend.position  = "bottom",
      panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "grey60", fill = NA),
      plot.margin      = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# =========================================================
# Data configuration
# =========================================================

config <- list(
  data = list(
    tipo_dado   = "mortalidade",
    tipo_evento = "alcool",
    local       = "Brasil"
  )
)

data_file <- here::here(
  "data", "real", "raw",
  paste0("txs_", config$data$tipo_dado, "_", config$data$tipo_evento, ".csv")
)

# =========================================================
# Load data
# =========================================================

df <- load_and_filter_data(
  data_file,
  local = config$data$local,
  sexo = NULL,
  faixa_etaria = NULL
) %>%
  mutate(
    t = seq_len(n()),
    serie = value
  )

# =========================================================
# Preprocessing
# =========================================================

preproc <- preprocess_series_for_methods(df$serie)
serie_proc <- preproc$serie

cat("Preprocessing | shift_applied:", preproc$applied,
    "| shift:", round(preproc$shift, 6), "\n")

# =========================================================
# Level 1: frozen banks
# =========================================================

cat("\n=== Building frozen banks (Level 1) ===\n")

ae_member_bank <- build_ae_member_bank_frozen(serie_proc)
cf_member_bank <- build_cf_member_bank_frozen(serie_proc)

ae_ensemble <- build_ae_ensemble_from_frozen(ae_member_bank)
cf_ensemble <- build_cf_ensemble_from_frozen(cf_member_bank)

static_bank <- build_static_frozen_bank(serie_proc)

# =========================================================
# Level 1: freeze AE and CF at the MFEF+JP(F,12,0.95) config
# =========================================================

cfg_detect <- list(
  time_tolerance = REAL_TIME_TOLERANCE,
  threshold      = REAL_THRESHOLD,
  threshold_type = REAL_THRESHOLD_TYPE,
  use_nms        = REAL_USE_NMS
)

ae_det <- run_detect(ae_ensemble, serie_proc, cfg_detect)
cf_det <- run_detect(cf_ensemble, serie_proc, cfg_detect)

ae_frozen <- hcp_frozen_detection(ae_det)
cf_frozen <- hcp_frozen_detection(cf_det)

# =========================================================
# Level 2: MFEF+JP(F,12,0.95)
# =========================================================

cat("\n=== Building Level 2: MFEF+JP(F,12,0.95) ===\n")

ensemble_mfef_jp <- build_level2_mfef_jp(ae_frozen, cf_frozen, static_bank)
ensemble_mfef_jp <- daltoolbox::fit(ensemble_mfef_jp, serie_proc)

detection_mfef_jp <- run_detect(ensemble_mfef_jp, serie_proc, cfg_detect)

# =========================================================
# Results: MFEF+JP
# =========================================================

pred <- as.logical(detection_mfef_jp$event)
pred[is.na(pred)] <- FALSE
detected_positions <- which(pred)
detected_timestamps <- df$timestamp[pred]

cat("\n=== MFEF+JP(F,12,0.95) DETECTIONS ===\n")
cat("Number of change-points:", sum(pred), "\n")
cat("Detected timestamps:\n")
print(detected_timestamps)

fuzzy_score <- attr(detection_mfef_jp, "score")
threshold_value <- attr(detection_mfef_jp, "threshold")

cat("\nThreshold value (0.95 percentile of scores > 0):", round(threshold_value, 3), "\n")

# =========================================================
# Joinpoint detections within the ensemble
# =========================================================

jp_dates <- extract_joinpoint_dates(detection_mfef_jp, df$timestamp)

cat("\n=== JOINPOINT(F) DETECTIONS (within the ensemble) ===\n")
cat("Number of Joinpoint detections:", length(jp_dates), "\n")
cat("Joinpoint timestamps:\n")
print(jp_dates)

# =========================================================
# Sub-threshold consensus peaks (qualitative)
# =========================================================

subthreshold_peaks <- find_subthreshold_peaks(
  score = fuzzy_score,
  timestamps = df$timestamp,
  threshold_value = threshold_value,
  min_peak_height = 0.2,
  min_distance = REAL_TIME_TOLERANCE
)

cat("\n=== SUB-THRESHOLD CONSENSUS PEAKS ===\n")
cat("(candidate change regions with partial consensus; qualitative use)\n")
print(subthreshold_peaks)

# =========================================================
# Simple delta-metrics (magnitude) for MFEF+JP detections
# =========================================================

delta_metrics <- compute_delta_metrics(
  serie = df$serie,
  detected_positions = detected_positions,
  timestamps = df$timestamp
)

cat("\n=== SIMPLE DELTA-METRICS (MFEF+JP) ===\n")
print(delta_metrics)

# =========================================================
# Full segment metrics: MFEF+JP
# =========================================================

cat("\n=====================================\n")
cat("SEGMENT METRICS: MFEF+JP(F,12,0.95)\n")
cat("=====================================\n")

metrics_mfef <- compute_segment_metrics(
  serie = df$serie,
  change_positions = detected_positions,
  timestamps = df$timestamp,
  periods_per_year = 12
)

cat("\n--- Segments (MFEF+JP) ---\n")
print(metrics_mfef$segments, width = Inf)

cat("\n--- AAPC (MFEF+JP) ---\n")
cat("AAPC:", metrics_mfef$aapc, "%\n")

cat("\n--- Delta-metrics between segments (MFEF+JP) ---\n")
print(metrics_mfef$deltas, width = Inf)

# =========================================================
# Full segment metrics: Joinpoint(F)
# =========================================================

cat("\n=====================================\n")
cat("SEGMENT METRICS: Joinpoint(F)\n")
cat("=====================================\n")

jp_positions <- which(df$timestamp %in% jp_dates)

metrics_jp <- compute_segment_metrics(
  serie = df$serie,
  change_positions = jp_positions,
  timestamps = df$timestamp,
  periods_per_year = 12
)

cat("\n--- Segments (Joinpoint) ---\n")
print(metrics_jp$segments, width = Inf)

cat("\n--- AAPC (Joinpoint) ---\n")
cat("AAPC:", metrics_jp$aapc, "%\n")

cat("\n--- Delta-metrics between segments (Joinpoint) ---\n")
print(metrics_jp$deltas, width = Inf)

# =========================================================
# Standard plots
# =========================================================

harbinger::har_ensemble_plot_models(
  detection = detection_mfef_jp,
  serie = serie_proc,
  time_idx = df$timestamp
)

harbinger::har_ensemble_plot(
  detection = detection_mfef_jp,
  serie = serie_proc,
  time_idx = df$timestamp
)

# Automatic: chooses 5 or 10 depending on the series length
p_heat <- plot_series_fuzzy_heatmap(
  serie           = df$serie,
  timestamps      = df$timestamp,
  fuzzy_score     = fuzzy_score,
  threshold_value = threshold_value,
  x_break_years = 10,
  y_n_breaks = 4
)
print(p_heat)