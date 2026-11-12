# =========================================================
# 04-analyze-real.R
# Real-data application using Brazilian DATASUS indicators.
#
# The script applies MFEF+JP under the fixed shortlisted
# configuration MFEF+JP(F,12,0.95), extracts the internal
# Joinpoint detections, estimates segment-level statistics,
# and produces the ensemble and fuzzy-score figures.
#
# Input:
#   data/real/raw/txs_<tipo_dado>_<tipo_evento>.csv
#
# Outputs:
#   RDS result artifacts in results/real/;
#   PDF figures in figures/real/;
#   console summaries.
#
# Reproducibility:
#   R dependencies are managed through renv.
#   Python dependencies are resolved through reticulate.
# =========================================================

# =========================================================
# EXECUTION ENVIRONMENT
# =========================================================

execution_system <- Sys.info()[["sysname"]]

if (identical(execution_system, "Darwin")) {
  Sys.setenv(
    KMP_DUPLICATE_LIB_OK = "TRUE"
  )
  
  warning(
    paste0(
      "macOS OpenMP compatibility mode enabled. ",
      "The analysis should also be reproduced on Linux ",
      "without this workaround."
    ),
    call. = FALSE
  )
}

Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

# =========================================================
# PYTHON REQUIREMENTS
# =========================================================

library(
  reticulate
)

reticulate::py_require(
  packages = c(
    "numpy==2.5.4",
    "pandas==3.1.1",
    "scikit-learn==1.9.1",
    "torch==2.15.0"
  ),
  python_version = "==3.12.*",
  action = "add"
)

python_configuration <- reticulate::py_config()

required_python_modules <- c(
  "numpy",
  "pandas",
  "sklearn",
  "torch"
)

python_module_status <- setNames(
  vapply(
    required_python_modules,
    reticulate::py_module_available,
    logical(1)
  ),
  required_python_modules
)

if (any(!python_module_status)) {
  stop(
    paste0(
      "Missing Python module(s): ",
      paste(
        names(
          python_module_status
        )[!python_module_status],
        collapse = ", "
      ),
      ". Restart R and run this script from the first line."
    ),
    call. = FALSE
  )
}

# PyTorch must be imported before R packages that load OpenMP.
reticulate::py_run_string(
  "
import platform
import numpy as np
import pandas as pd
import sklearn
import torch

torch.set_num_threads(1)

print('Python architecture:', platform.machine())
print('NumPy version:', np.__version__)
print('pandas version:', pd.__version__)
print('scikit-learn version:', sklearn.__version__)
print('PyTorch version:', torch.__version__)
print('PyTorch threads:', torch.get_num_threads())
print('MPS available:', torch.backends.mps.is_available())
"
)

# =========================================================
# REPRODUCIBILITY
# =========================================================

analysis_seed <- 42L
bootstrap_replicates <- 1000L

set_analysis_seed <- function(
    seed = 42L) {
  
  seed <- as.integer(
    seed
  )
  
  set.seed(
    seed
  )
  
  reticulate::py_run_string(
    "
import os
import random
import numpy as np
import torch

def set_analysis_seed(seed=42):
    seed = int(seed)

    random.seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)

    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
        torch.cuda.manual_seed_all(seed)

    if hasattr(torch.backends, 'cudnn'):
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = False

    return seed
"
  )
  
  reticulate::py$set_analysis_seed(
    seed
  )
  
  invisible(
    seed
  )
}

set_analysis_seed(
  analysis_seed
)

# =========================================================
# REQUIRED R PACKAGES
# =========================================================

required_r_packages <- c(
  "harbinger",
  "daltoolbox",
  "daltoolboxdp",
  "ocp",
  "tidyverse",
  "patchwork",
  "nlme",
  "here"
)

missing_r_packages <- required_r_packages[
  !vapply(
    required_r_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_r_packages) > 0L) {
  stop(
    paste0(
      "Missing R package(s): ",
      paste(
        missing_r_packages,
        collapse = ", "
      ),
      ". Run renv::restore() before executing this analysis."
    ),
    call. = FALSE
  )
}

if (
  as.character(
    utils::packageVersion(
      "ocp"
    )
  ) != "0.1.1"
) {
  stop(
    paste0(
      "Package 'ocp' 0.1.1 is required, but version ",
      as.character(
        utils::packageVersion(
          "ocp"
        )
      ),
      " is installed."
    ),
    call. = FALSE
  )
}

# =========================================================
# R PACKAGES
# =========================================================

library(harbinger)
library(daltoolbox)
library(daltoolboxdp)
library(tidyverse)
library(patchwork)
library(nlme)
library(here)



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
      log_transform = REAL_JP_LOG
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
        delta_var = delta_var,
        delta_var_lower = unname(dv_ci[1]),
        delta_var_upper = unname(dv_ci[2]),
        delta_var_ci = sprintf(
          "%.8f [%.8f, %.8f]",
          delta_var,
          dv_ci[1],
          dv_ci[2]
        ),
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

# =========================================================
# Dynamic axis helpers
# =========================================================

compute_year_breaks <- function(
    timestamps,
    target_breaks = 6L) {
  
  timestamps <- as.Date(
    timestamps
  )
  
  valid_timestamps <- timestamps[
    !is.na(
      timestamps
    )
  ]
  
  if (length(valid_timestamps) == 0L) {
    stop(
      "No valid timestamps were provided.",
      call. = FALSE
    )
  }
  
  start_date <- min(
    valid_timestamps
  )
  
  end_date <- max(
    valid_timestamps
  )
  
  start_year <- as.integer(
    format(
      start_date,
      "%Y"
    )
  )
  
  end_year <- as.integer(
    format(
      end_date,
      "%Y"
    )
  )
  
  span_years <- max(
    1L,
    end_year - start_year
  )
  
  candidate_steps <- c(
    1L,
    2L,
    3L,
    5L,
    10L,
    20L
  )
  
  target_step <- span_years /
    max(
      1L,
      as.integer(
        target_breaks
      ) -
        1L
    )
  
  step_years <- candidate_steps[
    which.min(
      abs(
        candidate_steps -
          target_step
      )
    )
  ]
  
  first_year <- ceiling(
    start_year /
      step_years
  ) *
    step_years
  
  last_year <- floor(
    end_year /
      step_years
  ) *
    step_years
  
  if (first_year <= last_year) {
    break_years <- seq(
      from = first_year,
      to = last_year,
      by = step_years
    )
  } else {
    break_years <- unique(
      c(
        start_year,
        end_year
      )
    )
  }
  
  break_dates <- as.Date(
    paste0(
      break_years,
      "-01-01"
    )
  )
  
  break_dates <- break_dates[
    break_dates >= start_date &
      break_dates <= end_date
  ]
  
  if (length(break_dates) < 2L) {
    break_dates <- unique(
      as.Date(
        c(
          start_date,
          end_date
        )
      )
    )
  }
  
  break_dates
}

compute_value_breaks <- function(
    values,
    target_breaks = 5L) {
  
  values <- as.numeric(
    values
  )
  
  values <- values[
    is.finite(
      values
    )
  ]
  
  if (length(values) == 0L) {
    stop(
      "No finite values were provided.",
      call. = FALSE
    )
  }
  
  value_range <- range(
    values
  )
  
  if (
    isTRUE(
      all.equal(
        value_range[1],
        value_range[2]
      )
    )
  ) {
    margin <- max(
      abs(
        value_range[1]
      ) *
        0.05,
      1e-6
    )
    
    value_range <- c(
      value_range[1] -
        margin,
      value_range[2] +
        margin
    )
  }
  
  breaks <- pretty(
    value_range,
    n = as.integer(
      target_breaks
    )
  )
  
  breaks <- breaks[
    breaks >= value_range[1] &
      breaks <= value_range[2]
  ]
  
  if (length(breaks) < 2L) {
    breaks <- pretty(
      value_range,
      n = max(
        3L,
        as.integer(
          target_breaks
        )
      )
    )
  }
  
  breaks
}

# =========================================================
# Series heatmap coloured by the final fuzzy score
# =========================================================

plot_series_fuzzy_heatmap <- function(
    serie,
    timestamps,
    fuzzy_score,
    threshold_value = NULL,
    palette = c(
      "white",
      "yellow",
      "red"
    ),
    n_interp = 3000L,
    show_axis_titles = FALSE,
    target_x_breaks = 6L,
    target_y_breaks = 5L) {
  
  stopifnot(
    length(serie) == length(fuzzy_score),
    length(serie) == length(timestamps)
  )
  
  df_line <- tibble::tibble(
    timestamp = as.Date(
      timestamps
    ),
    value = as.numeric(
      serie
    ),
    score = as.numeric(
      fuzzy_score
    )
  ) %>%
    dplyr::filter(
      !is.na(
        timestamp
      ),
      !is.na(
        value
      ),
      !is.na(
        score
      ),
      is.finite(
        value
      ),
      is.finite(
        score
      )
    )
  
  if (nrow(df_line) == 0L) {
    stop(
      "Empty data frame after removing invalid values.",
      call. = FALSE
    )
  }
  
  limiar <- threshold_value
  
  # Fine time grid for the smooth horizontal heatmap.
  t_num <- as.numeric(
    df_line$timestamp
  )
  
  grid_t <- seq(
    from = min(
      t_num
    ),
    to = max(
      t_num
    ),
    length.out = as.integer(
      n_interp
    )
  )
  
  grid_s <- stats::approx(
    x = t_num,
    y = df_line$score,
    xout = grid_t,
    rule = 2
  )$y
  
  tile_w <- (
    max(
      t_num
    ) -
      min(
        t_num
      )
  ) /
    as.integer(
      n_interp
    )
  
  df_heat <- tibble::tibble(
    timestamp = as.Date(
      grid_t,
      origin = "1970-01-01"
    ),
    score = grid_s
  )
  
  # Vertical geometry: fuzzy-score bands span the full plot height.
  x_range <- as.Date(
    range(
      df_line$timestamp
    )
  )
  
  y_min <- min(
    df_line$value
  )
  
  y_max <- max(
    df_line$value
  )
  
  if (
    isTRUE(
      all.equal(
        y_min,
        y_max
      )
    )
  ) {
    y_margin <- max(
      abs(
        y_min
      ) *
        0.05,
      1e-6
    )
    
    y_min <- y_min -
      y_margin
    
    y_max <- y_max +
      y_margin
  }
  
  y_center <- (
    y_min +
      y_max
  ) /
    2
  
  y_height <- (
    y_max -
      y_min
  ) *
    1.30
  
  # Axis breaks are calculated separately for each series.
  x_breaks <- compute_year_breaks(
    timestamps = df_line$timestamp,
    target_breaks = target_x_breaks
  )
  
  y_breaks <- compute_value_breaks(
    values = df_line$value,
    target_breaks = target_y_breaks
  )
  
  # Legend with threshold mark.
  if (!is.null(limiar)) {
    br <- c(
      0,
      limiar,
      1
    )
    
    lbl <- c(
      "0",
      paste0(
        "Threshold = ",
        sprintf(
          "%.3f",
          limiar
        )
      ),
      "1"
    )
  } else {
    br <- c(
      0,
      0.5,
      1
    )
    
    lbl <- c(
      "0",
      "0.5",
      "1"
    )
  }
  
  x_lab <- if (
    show_axis_titles
  ) {
    "Year"
  } else {
    NULL
  }
  
  y_lab <- if (
    show_axis_titles
  ) {
    "Rate"
  } else {
    NULL
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = df_heat,
      ggplot2::aes(
        x = timestamp,
        y = y_center,
        fill = score
      ),
      height = y_height,
      width = tile_w
    ) +
    ggplot2::geom_line(
      data = df_line,
      ggplot2::aes(
        x = timestamp,
        y = value
      ),
      color = "#222222",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = df_line,
      ggplot2::aes(
        x = timestamp,
        y = value
      ),
      color = "#222222",
      size = 0.5
    ) +
    ggplot2::scale_fill_gradientn(
      colors = palette,
      limits = c(
        0,
        1
      ),
      breaks = br,
      labels = lbl,
      name = "Fuzzy score"
    ) +
    ggplot2::scale_x_date(
      breaks = x_breaks,
      date_labels = "%Y",
      expand = c(
        0.01,
        0.01
      )
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      expand = c(
        0,
        0
      )
    ) +
    ggplot2::coord_cartesian(
      xlim = x_range,
      ylim = c(
        y_min,
        y_max
      )
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        direction = "horizontal",
        barwidth = grid::unit(
          5,
          "cm"
        ),
        barheight = grid::unit(
          0.25,
          "cm"
        ),
        ticks = TRUE,
        ticks.colour = "black"
      )
    ) +
    ggplot2::theme_bw(
      base_size = 12
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 0,
        hjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(
        hjust = 1
      ),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_line(
        color = "grey88",
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        color = "grey60",
        fill = NA
      ),
      plot.margin = ggplot2::margin(
        t = 10,
        r = 10,
        b = 10,
        l = 10
      )
    )
}

# =========================================================
# Real-data series configuration
# =========================================================

series_config <- list(
  alcohol_mortality = list(
    label = "Alcohol mortality",
    tipo_dado = "mortalidade",
    tipo_evento = "alcool",
    local = "Brasil",
    sexo = NULL,
    faixa_etaria = NULL,
    figure_prefix = "alcohol_mortalidade"
  ),
  alcohol_hospitalization = list(
    label = "Alcohol hospitalization",
    tipo_dado = "internacao",
    tipo_evento = "alcool",
    local = "Brasil",
    sexo = NULL,
    faixa_etaria = NULL,
    figure_prefix = "alcohol_internacao"
  ),
  mood_mortality = list(
    label = "Mood disorder mortality",
    tipo_dado = "mortalidade",
    tipo_evento = "humor",
    local = "Brasil",
    sexo = NULL,
    faixa_etaria = NULL,
    figure_prefix = "mood_mortalidade"
  ),
  mood_hospitalization = list(
    label = "Mood disorder hospitalization",
    tipo_dado = "internacao",
    tipo_evento = "humor",
    local = "Brasil",
    sexo = NULL,
    faixa_etaria = NULL,
    figure_prefix = "mood_internacao"
  )
)

# =========================================================
# OUTPUT DIRECTORIES
# =========================================================

real_results_directory <- here::here(
  "results",
  "real"
)

real_figures_directory <- here::here(
  "figures",
  "real"
)

dir.create(
  real_results_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  real_figures_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# =========================================================
# Analyze one real-data series
# =========================================================

analyze_real_series <- function(series_id, series_cfg) {
  
  cat("\n\n")
  cat("============================================================\n")
  cat("SERIES:", series_cfg$label, "\n")
  cat("============================================================\n")
  
  # Reset random generators for each series.
  set_analysis_seed(
    analysis_seed
  )
  
  # =========================================================
  # Data file
  # =========================================================
  
  data_file <- here::here(
    "data",
    "real",
    "raw",
    paste0(
      "txs_",
      series_cfg$tipo_dado,
      "_",
      series_cfg$tipo_evento,
      ".csv"
    )
  )
  
  if (!file.exists(data_file)) {
    stop(
      paste0(
        "Input file not found: ",
        data_file
      ),
      call. = FALSE
    )
  }
  
  cat(
    "Input file:",
    data_file,
    "\n"
  )
  
  # =========================================================
  # Load data
  # =========================================================
  
  df <- load_and_filter_data(
    data_file = data_file,
    local = series_cfg$local,
    sexo = series_cfg$sexo,
    faixa_etaria = series_cfg$faixa_etaria
  ) %>%
    dplyr::mutate(
      t = dplyr::row_number(),
      serie = value
    )
  
  if (nrow(df) == 0L) {
    stop(
      paste0(
        series_cfg$label,
        ": no observations remained after filtering."
      ),
      call. = FALSE
    )
  }
  
  if (
    anyNA(df$serie) ||
    any(!is.finite(df$serie))
  ) {
    stop(
      paste0(
        series_cfg$label,
        ": the series contains missing or non-finite values."
      ),
      call. = FALSE
    )
  }
  
  cat(
    "Observations:",
    nrow(df),
    "\n"
  )
  
  cat(
    "Period:",
    as.character(min(df$timestamp)),
    "to",
    as.character(max(df$timestamp)),
    "\n"
  )
  
  # =========================================================
  # Preprocessing
  # =========================================================
  
  preproc <- preprocess_series_for_methods(
    df$serie
  )
  
  serie_proc <- preproc$serie
  
  cat(
    "Preprocessing | shift_applied:",
    preproc$applied,
    "| shift:",
    round(preproc$shift, 6),
    "\n"
  )
  
  # =========================================================
  # Level 1: frozen banks
  # =========================================================
  
  cat(
    "\n=== Building frozen banks (Level 1) ===\n"
  )
  
  ae_member_bank <- build_ae_member_bank_frozen(
    serie_proc
  )
  
  cf_member_bank <- build_cf_member_bank_frozen(
    serie_proc
  )
  
  ae_ensemble <- build_ae_ensemble_from_frozen(
    ae_member_bank
  )
  
  cf_ensemble <- build_cf_ensemble_from_frozen(
    cf_member_bank
  )
  
  static_bank <- build_static_frozen_bank(
    serie_proc
  )
  
  # =========================================================
  # Level 1: freeze AE and CF
  # =========================================================
  
  cfg_detect <- list(
    time_tolerance = REAL_TIME_TOLERANCE,
    threshold = REAL_THRESHOLD,
    threshold_type = REAL_THRESHOLD_TYPE,
    use_nms = REAL_USE_NMS
  )
  
  ae_det <- run_detect(
    ae_ensemble,
    serie_proc,
    cfg_detect
  )
  
  cf_det <- run_detect(
    cf_ensemble,
    serie_proc,
    cfg_detect
  )
  
  ae_frozen <- hcp_frozen_detection(
    ae_det
  )
  
  cf_frozen <- hcp_frozen_detection(
    cf_det
  )
  
  # =========================================================
  # Level 2: MFEF+JP(F,12,0.95)
  # =========================================================
  
  cat(
    "\n=== Building Level 2: MFEF+JP(F,12,0.95) ===\n"
  )
  
  ensemble_mfef_jp <- build_level2_mfef_jp(
    ae_frozen,
    cf_frozen,
    static_bank
  )
  
  ensemble_mfef_jp <- daltoolbox::fit(
    ensemble_mfef_jp,
    serie_proc
  )
  
  detection_mfef_jp <- run_detect(
    ensemble_mfef_jp,
    serie_proc,
    cfg_detect
  )
  
  # =========================================================
  # MFEF+JP detections
  # =========================================================
  
  pred <- as.logical(
    detection_mfef_jp$event
  )
  
  pred[is.na(pred)] <- FALSE
  
  detected_positions <- which(
    pred
  )
  
  detected_timestamps <- df$timestamp[
    pred
  ]
  
  fuzzy_score <- attr(
    detection_mfef_jp,
    "score"
  )
  
  threshold_value <- attr(
    detection_mfef_jp,
    "threshold"
  )
  
  cat(
    "\n=== MFEF+JP(F,12,0.95) DETECTIONS ===\n"
  )
  
  cat(
    "Number of change-points:",
    sum(pred),
    "\n"
  )
  
  cat(
    "Detected timestamps:\n"
  )
  
  print(
    detected_timestamps
  )
  
  cat(
    "\nThreshold value:",
    round(threshold_value, 3),
    "\n"
  )
  
  # =========================================================
  # Joinpoint detections
  # =========================================================
  
  jp_dates <- extract_joinpoint_dates(
    detection_mfef_jp,
    df$timestamp
  )
  
  jp_positions <- which(
    df$timestamp %in% jp_dates
  )
  
  cat(
    "\n=== JOINPOINT(F) DETECTIONS ===\n"
  )
  
  cat(
    "Number of Joinpoint detections:",
    length(jp_dates),
    "\n"
  )
  
  cat(
    "Joinpoint timestamps:\n"
  )
  
  print(
    jp_dates
  )
  
  # =========================================================
  # Sub-threshold peaks
  # =========================================================
  
  subthreshold_peaks <- find_subthreshold_peaks(
    score = fuzzy_score,
    timestamps = df$timestamp,
    threshold_value = threshold_value,
    min_peak_height = 0.2,
    min_distance = REAL_TIME_TOLERANCE
  )
  
  cat(
    "\n=== SUB-THRESHOLD CONSENSUS PEAKS ===\n"
  )
  
  print(
    subthreshold_peaks
  )
  
  # =========================================================
  # Simple delta metrics
  # =========================================================
  
  delta_metrics <- compute_delta_metrics(
    serie = df$serie,
    detected_positions = detected_positions,
    timestamps = df$timestamp
  )
  
  cat(
    "\n=== SIMPLE DELTA-METRICS (MFEF+JP) ===\n"
  )
  
  print(
    delta_metrics
  )
  
  # =========================================================
  # Segment metrics: MFEF+JP
  # =========================================================
  
  set.seed(
    analysis_seed
  )
  
  metrics_mfef <- compute_segment_metrics(
    serie = df$serie,
    change_positions = detected_positions,
    timestamps = df$timestamp,
    periods_per_year = 12,
    B = bootstrap_replicates
  )
  
  cat("\n=====================================\n")
  cat("SEGMENT METRICS: MFEF+JP(F,12,0.95)\n")
  cat("=====================================\n")
  
  cat(
    "\n--- Segments (MFEF+JP) ---\n"
  )
  
  print(
    metrics_mfef$segments,
    width = Inf
  )
  
  cat(
    "\n--- AAPC (MFEF+JP) ---\n"
  )
  
  cat(
    "AAPC:",
    metrics_mfef$aapc,
    "%\n"
  )
  
  cat(
    "\n--- Delta-metrics between segments (MFEF+JP) ---\n"
  )
  
  print(
    metrics_mfef$deltas,
    width = Inf
  )
  
  # =========================================================
  # Segment metrics: Joinpoint
  # =========================================================
  
  set.seed(
    analysis_seed
  )
  
  metrics_jp <- compute_segment_metrics(
    serie = df$serie,
    change_positions = jp_positions,
    timestamps = df$timestamp,
    periods_per_year = 12,
    B = bootstrap_replicates
  )
  
  cat("\n=====================================\n")
  cat("SEGMENT METRICS: Joinpoint(F)\n")
  cat("=====================================\n")
  
  cat(
    "\n--- Segments (Joinpoint) ---\n"
  )
  
  print(
    metrics_jp$segments,
    width = Inf
  )
  
  cat(
    "\n--- AAPC (Joinpoint) ---\n"
  )
  
  cat(
    "AAPC:",
    metrics_jp$aapc,
    "%\n"
  )
  
  cat(
    "\n--- Delta-metrics between segments (Joinpoint) ---\n"
  )
  
  print(
    metrics_jp$deltas,
    width = Inf
  )
  
  # =========================================================
  # Plots and figure export
  # =========================================================
  
  detectors_figure_file <- file.path(
    real_figures_directory,
    paste0(
      series_cfg$figure_prefix,
      "_detectors.pdf"
    )
  )
  
  score_figure_file <- file.path(
    real_figures_directory,
    paste0(
      series_cfg$figure_prefix,
      "_score.pdf"
    )
  )
  
  heatmap_figure_file <- file.path(
    real_figures_directory,
    paste0(
      series_cfg$figure_prefix,
      "_heatmap.pdf"
    )
  )
  
  figure_files <- c(
    detectors = detectors_figure_file,
    score = score_figure_file,
    heatmap = heatmap_figure_file
  )
  
  existing_figure_files <- figure_files[
    file.exists(
      figure_files
    )
  ]
  
  if (length(existing_figure_files) > 0L) {
    unlink(
      existing_figure_files
    )
  }
  
  # Detector-level figure
  grDevices::pdf(
    file = detectors_figure_file,
    width = 12,
    height = 9,
    onefile = TRUE,
    useDingbats = FALSE
  )
  
  p_models <- harbinger::har_ensemble_plot_models(
    detection = detection_mfef_jp,
    serie = serie_proc,
    time_idx = df$timestamp
  )
  
  if (
    inherits(
      p_models,
      "ggplot"
    ) ||
    inherits(
      p_models,
      "patchwork"
    )
  ) {
    print(
      p_models
    )
  }
  
  grDevices::dev.off()
  
  # Ensemble score and detected change-points
  grDevices::pdf(
    file = score_figure_file,
    width = 12,
    height = 7,
    onefile = TRUE,
    useDingbats = FALSE
  )
  
  p_ensemble <- harbinger::har_ensemble_plot(
    detection = detection_mfef_jp,
    serie = serie_proc,
    time_idx = df$timestamp
  )
  
  if (
    inherits(
      p_ensemble,
      "ggplot"
    ) ||
    inherits(
      p_ensemble,
      "patchwork"
    )
  ) {
    print(
      p_ensemble
    )
  }
  
  grDevices::dev.off()
  
  # Fuzzy-score heatmap
  p_heat <- plot_series_fuzzy_heatmap(
    serie = df$serie,
    timestamps = df$timestamp,
    fuzzy_score = fuzzy_score,
    threshold_value = threshold_value,
    show_axis_titles = FALSE,
    target_x_breaks = 6L,
    target_y_breaks = 5L
  )
  
  ggplot2::ggsave(
    filename = heatmap_figure_file,
    plot = p_heat,
    device = "pdf",
    width = 12,
    height = 5.5,
    units = "in",
    limitsize = FALSE
  )
  
  # Display figures during interactive execution
  if (
    inherits(
      p_models,
      "ggplot"
    ) ||
    inherits(
      p_models,
      "patchwork"
    )
  ) {
    print(
      p_models
    )
  }
  
  if (
    inherits(
      p_ensemble,
      "ggplot"
    ) ||
    inherits(
      p_ensemble,
      "patchwork"
    )
  ) {
    print(
      p_ensemble
    )
  }
  
  print(
    p_heat
  )
  
  
  figure_status <- file.exists(
    figure_files
  )
  
  if (
    any(
      !figure_status
    )
  ) {
    stop(
      paste0(
        series_cfg$label,
        ": figure export failed for: ",
        paste(
          figure_files[
            !figure_status
          ],
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  figure_sizes <- file.info(
    figure_files
  )$size
  
  if (
    any(
      is.na(
        figure_sizes
      ) |
      figure_sizes <= 0
    )
  ) {
    stop(
      paste0(
        series_cfg$label,
        ": one or more figure files are empty."
      ),
      call. = FALSE
    )
  }
  
  cat(
    "\nFigures saved:\n"
  )
  
  for (
    figure_file in figure_files
  ) {
    cat(
      figure_file,
      "|",
      file.info(
        figure_file
      )$size,
      "bytes\n"
    )
  }
  
  # =========================================================
  # Return series results
  # =========================================================
  
  list(
    series_id = series_id,
    label = series_cfg$label,
    data_file = data_file,
    data_file_md5 = unname(
      tools::md5sum(
        data_file
      )
    ),
    data = df,
    preprocessing = preproc,
    detections = list(
      mfef_positions = detected_positions,
      mfef_dates = detected_timestamps,
      joinpoint_positions = jp_positions,
      joinpoint_dates = jp_dates,
      fuzzy_score = fuzzy_score,
      threshold = threshold_value,
      subthreshold_peaks = subthreshold_peaks
    ),
    metrics = list(
      simple_deltas = delta_metrics,
      mfef = metrics_mfef,
      joinpoint = metrics_jp
    ),
    figures = list(
      models = p_models,
      ensemble = p_ensemble,
      heatmap = p_heat,
      files = as.list(
        figure_files
      )
    )
  )
}

# =========================================================
# Run all four series
# =========================================================

real_analysis_results <- vector(
  mode = "list",
  length = length(series_config)
)

names(real_analysis_results) <- names(
  series_config
)

for (series_id in names(series_config)) {
  
  real_analysis_results[[series_id]] <- analyze_real_series(
    series_id = series_id,
    series_cfg = series_config[[series_id]]
  )
  
  gc()
}

# =========================================================
# Derived real-data tables
# =========================================================

real_detections <- purrr::imap_dfr(
  real_analysis_results,
  function(
    result,
    series_id) {
    
    tibble::tibble(
      series_id = series_id,
      series = result$label,
      threshold = result$detections$threshold,
      mfef_detections = paste(
        format(
          result$detections$mfef_dates,
          "%Y-%m"
        ),
        collapse = ", "
      ),
      joinpoint_detections = paste(
        format(
          result$detections$joinpoint_dates,
          "%Y-%m"
        ),
        collapse = ", "
      )
    )
  }
)

real_mfef_segments <- purrr::imap_dfr(
  real_analysis_results,
  function(
    result,
    series_id) {
    
    result$metrics$mfef$segments %>%
      dplyr::mutate(
        series_id = series_id,
        series = result$label,
        aapc = result$metrics$mfef$aapc,
        .before = 1
      )
  }
)

real_mfef_deltas <- purrr::imap_dfr(
  real_analysis_results,
  function(
    result,
    series_id) {
    
    result$metrics$mfef$deltas %>%
      dplyr::mutate(
        series_id = series_id,
        series = result$label,
        .before = 1
      )
  }
)

real_input_manifest <- purrr::imap_dfr(
  real_analysis_results,
  function(
    result,
    series_id) {
    
    tibble::tibble(
      series_id = series_id,
      series = result$label,
      data_file = result$data_file,
      md5 = result$data_file_md5,
      observations = nrow(
        result$data
      ),
      start_date = min(
        result$data$timestamp
      ),
      end_date = max(
        result$data$timestamp
      )
    )
  }
)

# =========================================================
# Final summary
# =========================================================

real_analysis_summary <- purrr::imap_dfr(
  real_analysis_results,
  function(result, series_id) {
    tibble::tibble(
      series_id = series_id,
      series = result$label,
      observations = nrow(result$data),
      start_date = min(result$data$timestamp),
      end_date = max(result$data$timestamp),
      threshold = result$detections$threshold,
      mfef_detections = paste(
        result$detections$mfef_dates,
        collapse = ", "
      ),
      joinpoint_detections = paste(
        result$detections$joinpoint_dates,
        collapse = ", "
      ),
      mfef_aapc = result$metrics$mfef$aapc,
      joinpoint_aapc = result$metrics$joinpoint$aapc
    )
  }
)

cat("\n\n")
cat("============================================================\n")
cat("FINAL SUMMARY\n")
cat("============================================================\n")

print(
  real_analysis_summary,
  width = Inf
)

cat("\nAll four real-data series completed successfully.\n")

# =========================================================
# SAVE REAL-DATA RESULT ARTIFACTS
# =========================================================

real_analysis_results_file <- file.path(
  real_results_directory,
  "real_analysis_results.rds"
)

real_analysis_summary_file <- file.path(
  real_results_directory,
  "real_analysis_summary.rds"
)

real_detections_file <- file.path(
  real_results_directory,
  "real_detections.rds"
)

real_mfef_segments_file <- file.path(
  real_results_directory,
  "real_mfef_segments.rds"
)

real_mfef_deltas_file <- file.path(
  real_results_directory,
  "real_mfef_deltas.rds"
)

real_input_manifest_file <- file.path(
  real_results_directory,
  "real_input_manifest.rds"
)

saveRDS(
  real_analysis_results,
  real_analysis_results_file
)

saveRDS(
  real_analysis_summary,
  real_analysis_summary_file
)

saveRDS(
  real_detections,
  real_detections_file
)

saveRDS(
  real_mfef_segments,
  real_mfef_segments_file
)

saveRDS(
  real_mfef_deltas,
  real_mfef_deltas_file
)

saveRDS(
  real_input_manifest,
  real_input_manifest_file
)

cat("\n")
cat("============================================================\n")
cat("REAL-DATA ARTIFACTS\n")
cat("============================================================\n")

cat(
  real_analysis_results_file,
  "\n"
)

cat(
  real_analysis_summary_file,
  "\n"
)

cat(
  real_detections_file,
  "\n"
)

cat(
  real_mfef_segments_file,
  "\n"
)

cat(
  real_mfef_deltas_file,
  "\n"
)

cat(
  real_input_manifest_file,
  "\n"
)

# =========================================================
# VERIFY REAL-DATA ARTIFACTS
# =========================================================

expected_result_files <- c(
  real_analysis_results_file,
  real_analysis_summary_file,
  real_detections_file,
  real_mfef_segments_file,
  real_mfef_deltas_file,
  real_input_manifest_file
)

expected_figure_files <- unlist(
  purrr::map(
    real_analysis_results,
    function(result) {
      unlist(
        result$figures$files,
        use.names = FALSE
      )
    }
  ),
  use.names = FALSE
)

missing_result_files <- expected_result_files[
  !file.exists(
    expected_result_files
  )
]

missing_figure_files <- expected_figure_files[
  !file.exists(
    expected_figure_files
  )
]

if (length(missing_result_files) > 0L) {
  stop(
    paste0(
      "Missing result artifact(s): ",
      paste(
        missing_result_files,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (length(missing_figure_files) > 0L) {
  stop(
    paste0(
      "Missing figure artifact(s): ",
      paste(
        missing_figure_files,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

real_artifact_files <- c(
  expected_result_files,
  expected_figure_files
)

real_artifact_metadata <- file.info(
  real_artifact_files
)

empty_real_artifact_files <- real_artifact_files[
  is.na(
    real_artifact_metadata$size
  ) |
    real_artifact_metadata$size <= 0
]

if (length(empty_real_artifact_files) > 0L) {
  stop(
    paste0(
      "Empty real-data artifact(s): ",
      paste(
        empty_real_artifact_files,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

real_artifact_inventory <- tibble::tibble(
  file = basename(
    real_artifact_files
  ),
  
  path =
    real_artifact_files,
  
  size_bytes =
    real_artifact_metadata$size,
  
  modified =
    real_artifact_metadata$mtime
) %>%
  arrange(
    file
  )

cat("\n")
cat("============================================================\n")
cat("REAL-DATA ARTIFACT INVENTORY\n")
cat("============================================================\n")

print(
  real_artifact_inventory,
  n = Inf,
  width = Inf
)

cat(
  "\nNumber of real-data artifacts:",
  nrow(
    real_artifact_inventory
  ),
  "\n"
)

cat(
  "All real-data artifacts were saved successfully.\n"
)




