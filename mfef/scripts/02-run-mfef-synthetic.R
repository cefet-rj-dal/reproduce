# =========================================================
# 02-run-mfef-synthetic.R
# Batch runner for the MFEF families and the Joinpoint baseline
# over the synthetic series. Computes soft/hard detection metrics
# and writes a per-series, per-method, per-configuration results file.
#
# Input : data/synthetic/processed/synthetic_series.rds
# Output: outputs/synthetic/detections/<batch>.csv
# =========================================================

library(tidyverse)
library(here)
library(rJavaEnv)
library(daltoolbox)
library(daltoolboxdp)
library(harbinger)

# =========================================================
# Seed
# =========================================================
source(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/examples/seed.R"))
set_example_seed(42)

# =========================================================
# Dependency checks
# =========================================================
if (!exists("har_eval_soft", mode = "function")) {
  stop(
    "har_eval_soft() not found. Ensure it is available through harbinger.",
    call. = FALSE
  )
}

if (!exists("har_eval", mode = "function")) {
  stop(
    "har_eval() not found. Ensure it is available through harbinger.",
    call. = FALSE
  )
}

# =========================================================
# Paths
# =========================================================
synthetic_series_file <- here::here("data", "synthetic", "processed", "synthetic_series.rds")

if (!file.exists(synthetic_series_file)) {
  stop(
    paste0(
      "File not found: ", synthetic_series_file,
      "\nRun 01-prep-synthetic.R first."
    )
  )
}

synthetic_series <- readRDS(synthetic_series_file) %>%
  ungroup() %>%
  as_tibble()

cat("\nLoaded synthetic_series:\n")
cat(synthetic_series_file, "\n")
cat("Total number of series:", nrow(synthetic_series), "\n")

# =========================================================
# User controls
# =========================================================

# TRUE  = run batch
# FALSE = run visual diagnostics
run_batch <- TRUE

plot_diagnostics <- FALSE
show_diagnostic_metrics <- FALSE
open_diagnostic_metrics_view <- FALSE
open_viewer_after_run <- FALSE

# Batch range (by serie_id)
batch_start_serie_id <- 838
batch_end_serie_id <- 838

# Visual diagnostics target
plot_target_serie_id <- 838

plot_joinpoint_log_true <- TRUE
plot_joinpoint_log_false <- TRUE

plot_mfef_time_tolerance_grid <- c(3, 6, 12)
plot_mfef_threshold_grid <- c(0.90, 0.95)

# Families to run
run_joinpoint_baseline <- TRUE
run_mfef_base <- TRUE
run_mfef_jp_false <- TRUE
run_mfef_jp_true <- TRUE

# MFEF grid
detection_time_tolerance_grid <- c(3, 6, 12)
detection_threshold_grid <- c(0.90, 0.95)
detection_threshold_type <- "percentile"
detection_use_nms <- TRUE

# SoftED
softed_window_size <- 6

# =========================================================
# Execution rules
# =========================================================
if (run_batch) {
  plot_diagnostics <- FALSE
  show_diagnostic_metrics <- FALSE
  open_diagnostic_metrics_view <- FALSE
  open_viewer_after_run <- FALSE
}

# =========================================================
# Detector parameters
# =========================================================
bocpd_hazard <- 100
bocpd_dist <- "gaussian"
bocpd_threshold <- 0.5
bocpd_min_distance <- 12
bocpd_burn_in <- 6

binseg_Q <- 20

joinpoint_min_between <- 6
joinpoint_min_end <- 6
joinpoint_k_max <- 7

# =========================================================
# Output file
# =========================================================
threshold_grid_label <- paste0(
  "thrgrid_",
  paste(gsub("\\.", "p", format(detection_threshold_grid, trim = TRUE)), collapse = "_")
)

results_folder <- here::here("outputs", "synthetic", "detections")

dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)

results_csv_file <- file.path(
  results_folder,
  paste0(
    "batch_mfef_joinpoint_hybrid_soft_hard_",
    "id_", batch_start_serie_id, "_", batch_end_serie_id,
    "_sw_", softed_window_size,
    "_", threshold_grid_label,
    ".csv"
  )
)

# =========================================================
# Helpers
# =========================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Detection wrapper: applies a detection configuration to a fitted model.
run_detection <- function(model, serie, cfg) {
  harbinger::detect(
    model,
    serie,
    time_tolerance = cfg$time_tolerance,
    threshold      = cfg$threshold,
    threshold_type = cfg$threshold_type,
    use_nms        = cfg$use_nms
  )
}

format_elapsed <- function(seconds) {
  seconds <- as.numeric(seconds)
  h <- floor(seconds / 3600)
  m <- floor((seconds %% 3600) / 60)
  s <- seconds %% 60
  sprintf("%02dh:%02dm:%05.2fs", h, m, s)
}

safe_view <- function(x, title = "View") {
  View(x)
}

print_harbinger_plot <- function(p) {
  if (is.null(p)) return(invisible(NULL))
  
  if (inherits(p, "ggplot")) {
    print(p)
    return(invisible(NULL))
  }
  
  if (is.list(p)) {
    purrr::walk(p, print_harbinger_plot)
    return(invisible(NULL))
  }
  
  print(p)
  invisible(NULL)
}

build_synthetic_pool <- function(synthetic_series) {
  synthetic_series %>%
    ungroup() %>%
    as_tibble() %>%
    arrange(serie_id)
}

extract_series_metadata <- function(selected_row) {
  tibble(
    serie_id = selected_row$serie_id,
    serie_id_nome = selected_row$serie_id_nome
  )
}

get_active_ensemble_families <- function(
    run_mfef_base,
    run_mfef_jp_false,
    run_mfef_jp_true
) {
  out <- c()
  if (run_mfef_base) out <- c(out, "MFEF")
  if (run_mfef_jp_false) out <- c(out, "MFEF_JP_FALSE")
  if (run_mfef_jp_true) out <- c(out, "MFEF_JP_TRUE")
  out
}

# =========================================================
# Frozen detector helper
# =========================================================

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
  
  if (is.null(serie)) {
    stop("No data was provided", call. = FALSE)
  }
  
  if (length(obj$events) != length(serie)) {
    stop(
      paste0(
        "Frozen detection length (", length(obj$events),
        ") differs from series length (", length(serie), ")."
      ),
      call. = FALSE
    )
  }
  
  obj <- obj$har_store_refs(obj, serie)
  obj
}

detect.hcp_frozen_detection <- function(obj, serie, ...) {
  
  if (is.null(serie)) {
    stop("No data provided", call. = FALSE)
  }
  
  if (length(obj$events) != length(serie)) {
    stop(
      paste0(
        "Frozen detection length (", length(obj$events),
        ") differs from series length (", length(serie), ")."
      ),
      call. = FALSE
    )
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

# =========================================================
# Generic helper: fit, detect and freeze a single detector
# =========================================================

freeze_once <- function(model, serie, cfg = list()) {
  model     <- fit(model, serie)
  detection <- run_detection(model, serie, cfg)
  hcp_frozen_detection(detection)
}

# =========================================================
# Series preprocessing (once per series).
# Ensures strictly positive values by shifting the series when
# needed, so Joinpoint with log_transform = TRUE is supported.
# =========================================================

preprocess_series_for_methods <- function(serie) {
  
  if (!any(serie <= 0, na.rm = TRUE)) {
    return(list(
      serie   = serie,
      shift   = 0,
      applied = FALSE
    ))
  }
  
  # epsilon proportional to the smallest positive value
  pos_vals <- serie[serie > 0]
  pos_vals <- pos_vals[!is.na(pos_vals)]
  
  epsilon <- if (length(pos_vals) > 0) {
    max(min(pos_vals, na.rm = TRUE) / 100, 1e-6)
  } else {
    1e-6
  }
  
  min_val <- min(serie, na.rm = TRUE)
  
  # shift so the minimum becomes epsilon
  shift   <- epsilon - min_val
  serie_p <- serie + shift
  
  list(
    serie   = serie_p,
    shift   = shift,
    applied = TRUE
  )
}

# =========================================================
# Frozen bank of AE ensemble members (per series)
# =========================================================

build_ae_member_bank_frozen <- function(serie) {
  list(
    WAYPOINT_ED = freeze_once(
      hcp_waypoint(
        input_size = 12, encode_size = 4,
        warmup = 60, retrain_size = 30, buffer_size = 40,
        k_factor = 0.35, h_low = 3.5, h_high = 6, prob_tau = 0.997,
        epochs_init = 100, epochs_retrain = 100,
        encoderclass = autoenc_ed
      ),
      serie
    ),
    WAYPOINT_LSTM = freeze_once(
      hcp_waypoint(
        input_size = 12, encode_size = 4,
        warmup = 60, retrain_size = 30, buffer_size = 40,
        k_factor = 0.35, h_low = 3.5, h_high = 6, prob_tau = 0.997,
        epochs_init = 100, epochs_retrain = 100,
        encoderclass = autoenc_lstm_ed
      ),
      serie
    ),
    WAYPOINT_CONV = freeze_once(
      hcp_waypoint(
        input_size = 12, encode_size = 4,
        warmup = 60, retrain_size = 30, buffer_size = 40,
        k_factor = 0.35, h_low = 3.5, h_high = 6, prob_tau = 0.997,
        epochs_init = 100, epochs_retrain = 100,
        encoderclass = autoenc_conv_ed
      ),
      serie
    )
  )
}

# =========================================================
# Frozen bank of CF ensemble members (per series)
# =========================================================

build_cf_member_bank_frozen <- function(serie) {
  list(
    CF_ARIMA = freeze_once(hcp_cf_arima(), serie),
    CF_LR    = freeze_once(hcp_cf_lr(),    serie),
    CF_GARCH = freeze_once(hcp_garch(),    serie),
    CF_ETS   = freeze_once(hcp_cf_ets(),   serie)
  )
}

# =========================================================
# Frozen bank of classical detectors and Joinpoint (per series)
# =========================================================

build_static_frozen_bank <- function(
    serie,
    binseg_Q,
    bocpd_hazard, bocpd_dist, bocpd_threshold,
    bocpd_min_distance, bocpd_burn_in,
    joinpoint_min_between, joinpoint_min_end, joinpoint_k_max
) {
  
  bank <- list()
  
  bank$BINSEG <- freeze_once(hcp_binseg(Q = binseg_Q), serie)
  bank$PELT   <- freeze_once(hcp_pelt(), serie)
  bank$GFT    <- freeze_once(hcp_gft(), serie)
  bank$SCP    <- freeze_once(hcp_scp(), serie)
  
  bank$BOCPD <- freeze_once(
    hcp_bocpd(
      hazard = bocpd_hazard,
      dist = bocpd_dist,
      threshold = bocpd_threshold,
      min_distance = bocpd_min_distance,
      burn_in = bocpd_burn_in
    ),
    serie
  )
  
  if (exists("hcp_joinpoint", mode = "function")) {
    
    bank$JOINPOINT_FALSE <- freeze_once(
      hcp_joinpoint(
        min_between = joinpoint_min_between,
        min_end = joinpoint_min_end,
        k_max = joinpoint_k_max,
        log_transform = FALSE
      ),
      serie
    )
    
    if (!any(serie <= 0, na.rm = TRUE)) {
      bank$JOINPOINT_TRUE <- freeze_once(
        hcp_joinpoint(
          min_between = joinpoint_min_between,
          min_end = joinpoint_min_end,
          k_max = joinpoint_k_max,
          log_transform = TRUE
        ),
        serie
      )
    } else {
      bank$JOINPOINT_TRUE <- NULL
    }
    
  } else {
    bank$JOINPOINT_FALSE <- NULL
    bank$JOINPOINT_TRUE  <- NULL
  }
  
  bank
}

# =========================================================
# AE/CF ensembles assembled from frozen members
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

# =========================================================
# Level-2 ensemble built from frozen components
# =========================================================

build_level2_ensemble <- function(
    family_name,
    ae_ensemble_frozen_config,  # frozen AE ensemble (per config)
    cf_ensemble_frozen_config,  # frozen CF ensemble (per config)
    static_bank                 # frozen classical detectors and Joinpoint (per series)
) {
  
  models <- list(
    BINSEG = static_bank$BINSEG,
    PELT   = static_bank$PELT,
    GFT    = static_bank$GFT,
    SCP    = static_bank$SCP,
    BOCPD  = static_bank$BOCPD,
    ENSEMBLE_AE = ae_ensemble_frozen_config,
    ENSEMBLE_CF = cf_ensemble_frozen_config
  )
  
  if (family_name == "MFEF_JP_FALSE" && !is.null(static_bank$JOINPOINT_FALSE)) {
    models$JOINPOINT <- static_bank$JOINPOINT_FALSE
  }
  
  if (family_name == "MFEF_JP_TRUE" && !is.null(static_bank$JOINPOINT_TRUE)) {
    models$JOINPOINT <- static_bank$JOINPOINT_TRUE
  }
  
  do.call(har_ensemble_fuzzy, models)
}

# =========================================================
# Metrics
# =========================================================

summarise_detection_metrics <- function(
    method,
    detection,
    dataset,
    df_example,
    sw_size,
    time_tolerance = NA_real_,
    threshold = NA_real_,
    model_detail = NA_character_,
    log_transform = NA
) {
  
  pred_event <- as.logical(detection$event)
  pred_event[is.na(pred_event)] <- FALSE
  
  truth_event <- as.logical(dataset$event)
  truth_event[is.na(truth_event)] <- FALSE
  
  true_cps_vec <- df_example$tempo[truth_event]
  detected_cps_vec <- df_example$tempo[pred_event]
  
  soft_eval <- evaluate(
    har_eval_soft(sw_size = sw_size),
    pred_event,
    truth_event
  )
  
  hard_eval <- evaluate(
    har_eval(),
    pred_event,
    truth_event
  )
  
  hard_eval <- lapply(hard_eval, function(x) {
    if (is.numeric(x)) {
      x[is.nan(x) | is.infinite(x)] <- NA_real_
    }
    x
  })
  
  tibble(
    method = method,
    model_detail = model_detail,
    time_tolerance = time_tolerance,
    threshold = threshold,
    log_transform = log_transform,
    
    true_cps = paste(true_cps_vec, collapse = "; "),
    detected_cps = paste(detected_cps_vec, collapse = "; "),
    n_true = length(true_cps_vec),
    n_detected = length(detected_cps_vec),
    
    soft_F1 = soft_eval$F1,
    soft_precision = soft_eval$precision,
    soft_recall = soft_eval$recall,
    soft_accuracy = soft_eval$accuracy,
    soft_specificity = soft_eval$specificity,
    
    hard_F1 = hard_eval$F1,
    hard_precision = hard_eval$precision,
    hard_recall = hard_eval$recall,
    hard_accuracy = hard_eval$accuracy,
    hard_specificity = hard_eval$specificity
  )
}

make_empty_result_row <- function(
    metadata,
    method,
    model_detail,
    time_tolerance = NA_real_,
    threshold = NA_real_,
    log_transform = NA,
    true_cps = "",
    n_true = NA_integer_,
    status = "error",
    error_message = NA_character_
) {
  bind_cols(
    metadata,
    tibble(
      method = method,
      model_detail = model_detail,
      time_tolerance = time_tolerance,
      threshold = threshold,
      log_transform = log_transform,
      true_cps = true_cps,
      detected_cps = NA_character_,
      n_true = n_true,
      n_detected = NA_integer_,
      
      soft_F1 = NA_real_,
      soft_precision = NA_real_,
      soft_recall = NA_real_,
      soft_accuracy = NA_real_,
      soft_specificity = NA_real_,
      
      hard_F1 = NA_real_,
      hard_precision = NA_real_,
      hard_recall = NA_real_,
      hard_accuracy = NA_real_,
      hard_specificity = NA_real_,
      
      status = status,
      error_message = error_message
    )
  )
}

# =========================================================
# Single Joinpoint baseline run
# =========================================================

run_joinpoint_baseline_one <- function(
    metadata,
    dataset,
    df_example,
    softed_window_size,
    current_log_transform,
    current_detail,
    static_bank
) {
  
  true_cps_txt <- paste(df_example$tempo[df_example$is_true_cp == 1], collapse = "; ")
  n_true_val   <- sum(df_example$is_true_cp == 1)
  
  joinpoint_frozen <- if (current_log_transform) {
    static_bank$JOINPOINT_TRUE
  } else {
    static_bank$JOINPOINT_FALSE
  }
  
  if (is.null(joinpoint_frozen)) {
    return(list(
      row = make_empty_result_row(
        metadata = metadata,
        method = "Joinpoint",
        model_detail = current_detail,
        log_transform = current_log_transform,
        true_cps = true_cps_txt,
        n_true = n_true_val,
        status = "skipped_log_transform_nonpositive_series"
      ),
      run = NULL
    ))
  }
  
  out <- tryCatch({
    
    joinpoint_detection <- run_detection(joinpoint_frozen, dataset$serie, list())
    
    row <- bind_cols(
      metadata,
      summarise_detection_metrics(
        method = "Joinpoint",
        model_detail = current_detail,
        detection = joinpoint_detection,
        dataset = dataset,
        df_example = df_example,
        sw_size = softed_window_size,
        time_tolerance = NA_real_,
        threshold = NA_real_,
        log_transform = current_log_transform
      ) %>%
        mutate(
          status = "ok",
          error_message = NA_character_
        )
    )
    
    pred <- as.logical(joinpoint_detection$event)
    pred[is.na(pred)] <- FALSE
    detected_cps <- df_example$tempo[pred]
    
    list(
      row = row,
      run = list(
        type = "Joinpoint",
        model = joinpoint_frozen,
        detection = joinpoint_detection,
        detected_cps = detected_cps,
        label = current_detail
      )
    )
    
  }, error = function(e) {
    list(
      row = make_empty_result_row(
        metadata = metadata,
        method = "Joinpoint",
        model_detail = current_detail,
        log_transform = current_log_transform,
        true_cps = true_cps_txt,
        n_true = n_true_val,
        status = "error",
        error_message = conditionMessage(e)
      ),
      run = NULL
    )
  })
  
  out
}

# =========================================================
# Single ensemble family run (receives frozen AE/CF per config)
# =========================================================

run_ensemble_family_one <- function(
    metadata,
    family_name,
    dataset,
    df_example,
    softed_window_size,
    current_time_tolerance,
    current_threshold,
    detection_threshold_type,
    detection_use_nms,
    ae_ensemble_frozen_config,
    cf_ensemble_frozen_config,
    static_bank,
    joinpoint_available
) {
  
  model_detail_val <- paste0(
    "tt_", current_time_tolerance,
    "_thr_", format(current_threshold, trim = TRUE)
  )
  
  true_cps_txt <- paste(df_example$tempo[df_example$is_true_cp == 1], collapse = "; ")
  n_true_val   <- sum(df_example$is_true_cp == 1)
  
  family_log_transform <- ifelse(
    family_name == "MFEF_JP_TRUE", TRUE,
    ifelse(family_name == "MFEF_JP_FALSE", FALSE, NA)
  )
  
  if (!joinpoint_available && family_name %in% c("MFEF_JP_FALSE", "MFEF_JP_TRUE")) {
    return(list(
      row = make_empty_result_row(
        metadata = metadata,
        method = family_name,
        model_detail = model_detail_val,
        time_tolerance = current_time_tolerance,
        threshold = current_threshold,
        log_transform = family_log_transform,
        true_cps = true_cps_txt,
        n_true = n_true_val,
        status = "hcp_joinpoint_not_available"
      ),
      run = NULL
    ))
  }
  
  if (family_name == "MFEF_JP_TRUE" && is.null(static_bank$JOINPOINT_TRUE)) {
    return(list(
      row = make_empty_result_row(
        metadata = metadata,
        method = family_name,
        model_detail = model_detail_val,
        time_tolerance = current_time_tolerance,
        threshold = current_threshold,
        log_transform = TRUE,
        true_cps = true_cps_txt,
        n_true = n_true_val,
        status = "skipped_log_transform_nonpositive_series"
      ),
      run = NULL
    ))
  }
  
  out <- tryCatch({
    
    detection_config <- list(
      time_tolerance = current_time_tolerance,
      threshold = current_threshold,
      threshold_type = detection_threshold_type,
      use_nms = detection_use_nms
    )
    
    level2_ensemble <- build_level2_ensemble(
      family_name = family_name,
      ae_ensemble_frozen_config = ae_ensemble_frozen_config,
      cf_ensemble_frozen_config = cf_ensemble_frozen_config,
      static_bank = static_bank
    )
    
    level2_ensemble  <- fit(level2_ensemble, dataset$serie)
    level2_detection <- run_detection(level2_ensemble, dataset$serie, detection_config)
    
    row <- bind_cols(
      metadata,
      summarise_detection_metrics(
        method = family_name,
        model_detail = model_detail_val,
        detection = level2_detection,
        dataset = dataset,
        df_example = df_example,
        sw_size = softed_window_size,
        time_tolerance = current_time_tolerance,
        threshold = current_threshold,
        log_transform = family_log_transform
      ) %>%
        mutate(
          status = "ok",
          error_message = NA_character_
        )
    )
    
    pred <- as.logical(level2_detection$event)
    pred[is.na(pred)] <- FALSE
    detected_cps <- df_example$tempo[pred]
    
    list(
      row = row,
      run = list(
        type = "ensemble",
        family_name = family_name,
        threshold = current_threshold,
        time_tolerance = current_time_tolerance,
        model = level2_ensemble,
        detection = level2_detection,
        detected_cps = detected_cps
      )
    )
    
  }, error = function(e) {
    list(
      row = make_empty_result_row(
        metadata = metadata,
        method = family_name,
        model_detail = model_detail_val,
        time_tolerance = current_time_tolerance,
        threshold = current_threshold,
        log_transform = family_log_transform,
        true_cps = true_cps_txt,
        n_true = n_true_val,
        status = "error",
        error_message = conditionMessage(e)
      ),
      run = NULL
    )
  })
  
  out
}

# =========================================================
# Core: one series -> result rows
# =========================================================

run_one_series_comparison <- function(
    selected_row,
    detection_time_tolerance_grid,
    detection_threshold_grid,
    softed_window_size,
    detection_threshold_type,
    detection_use_nms,
    run_joinpoint_baseline,
    run_mfef_base,
    run_mfef_jp_false,
    run_mfef_jp_true,
    bocpd_hazard,
    bocpd_dist,
    bocpd_threshold,
    bocpd_min_distance,
    bocpd_burn_in,
    binseg_Q,
    joinpoint_min_between,
    joinpoint_min_end,
    joinpoint_k_max
) {
  
  metadata   <- extract_series_metadata(selected_row)
  df_example <- selected_row[["data"]][[1]]
  
  preproc <- preprocess_series_for_methods(df_example$valor)
  
  dataset <- list(
    serie = preproc$serie,
    event = as.logical(df_example$is_true_cp)
  )
  
  metadata <- metadata %>%
    mutate(
      shift_applied = preproc$applied,
      shift_value   = preproc$shift
    )
  
  cat(
    "Series preprocessing",
    "| shift_applied:", preproc$applied,
    "| shift:", round(preproc$shift, 6),
    "\n"
  )
  
  results <- list()
  
  joinpoint_available <- exists("hcp_joinpoint", mode = "function")
  
  # Frozen banks (per series)
  ae_member_bank <- build_ae_member_bank_frozen(dataset$serie)
  cf_member_bank <- build_cf_member_bank_frozen(dataset$serie)
  
  ae_ensemble_from_frozen <- build_ae_ensemble_from_frozen(ae_member_bank)
  cf_ensemble_from_frozen <- build_cf_ensemble_from_frozen(cf_member_bank)
  
  static_bank <- build_static_frozen_bank(
    serie = dataset$serie,
    binseg_Q = binseg_Q,
    bocpd_hazard = bocpd_hazard,
    bocpd_dist = bocpd_dist,
    bocpd_threshold = bocpd_threshold,
    bocpd_min_distance = bocpd_min_distance,
    bocpd_burn_in = bocpd_burn_in,
    joinpoint_min_between = joinpoint_min_between,
    joinpoint_min_end = joinpoint_min_end,
    joinpoint_k_max = joinpoint_k_max
  )
  
  # Joinpoint baselines
  if (run_joinpoint_baseline) {
    
    joinpoint_variants <- tibble(
      model_detail = c("log_transform_TRUE", "log_transform_FALSE"),
      log_transform = c(TRUE, FALSE)
    )
    
    for (i in seq_len(nrow(joinpoint_variants))) {
      
      current_detail <- joinpoint_variants$model_detail[[i]]
      current_log_transform <- joinpoint_variants$log_transform[[i]]
      
      jp_res <- run_joinpoint_baseline_one(
        metadata = metadata,
        dataset = dataset,
        df_example = df_example,
        softed_window_size = softed_window_size,
        current_log_transform = current_log_transform,
        current_detail = current_detail,
        static_bank = static_bank
      )
      
      results[[paste0("Joinpoint_", current_detail)]] <- jp_res$row
    }
  }
  
  # Ensemble families
  ensemble_families <- get_active_ensemble_families(
    run_mfef_base = run_mfef_base,
    run_mfef_jp_false = run_mfef_jp_false,
    run_mfef_jp_true = run_mfef_jp_true
  )
  
  # Cache frozen AE/CF per config (computed once per config)
  ae_cf_by_config <- list()
  
  for (current_threshold in detection_threshold_grid) {
    for (current_time_tolerance in detection_time_tolerance_grid) {
      
      detection_config <- list(
        time_tolerance = current_time_tolerance,
        threshold = current_threshold,
        threshold_type = detection_threshold_type,
        use_nms = detection_use_nms
      )
      
      key <- paste0("thr_", current_threshold, "_tt_", current_time_tolerance)
      
      ae_det_cfg <- run_detection(ae_ensemble_from_frozen, dataset$serie, detection_config)
      cf_det_cfg <- run_detection(cf_ensemble_from_frozen, dataset$serie, detection_config)
      
      ae_cf_by_config[[key]] <- list(
        ae_frozen = hcp_frozen_detection(ae_det_cfg),
        cf_frozen = hcp_frozen_detection(cf_det_cfg)
      )
    }
  }
  
  for (family_name in ensemble_families) {
    for (current_threshold in detection_threshold_grid) {
      for (current_time_tolerance in detection_time_tolerance_grid) {
        
        key <- paste0("thr_", current_threshold, "_tt_", current_time_tolerance)
        ae_frozen_config <- ae_cf_by_config[[key]]$ae_frozen
        cf_frozen_config <- ae_cf_by_config[[key]]$cf_frozen
        
        fam_res <- run_ensemble_family_one(
          metadata = metadata,
          family_name = family_name,
          dataset = dataset,
          df_example = df_example,
          softed_window_size = softed_window_size,
          current_time_tolerance = current_time_tolerance,
          current_threshold = current_threshold,
          detection_threshold_type = detection_threshold_type,
          detection_use_nms = detection_use_nms,
          ae_ensemble_frozen_config = ae_frozen_config,
          cf_ensemble_frozen_config = cf_frozen_config,
          static_bank = static_bank,
          joinpoint_available = joinpoint_available
        )
        
        results[[paste0(family_name, "_", current_time_tolerance, "_", current_threshold)]] <- fam_res$row
      }
    }
  }
  
  bind_rows(results)
}

# =========================================================
# Diagnostics: one series -> metrics + plot runs
# =========================================================

run_one_series_for_plots <- function(
    selected_row,
    mfef_time_tolerance_grid,
    mfef_threshold_grid,
    softed_window_size,
    detection_threshold_type,
    detection_use_nms,
    run_joinpoint_baseline,
    run_mfef_base,
    run_mfef_jp_false,
    run_mfef_jp_true,
    bocpd_hazard,
    bocpd_dist,
    bocpd_threshold,
    bocpd_min_distance,
    bocpd_burn_in,
    binseg_Q,
    joinpoint_min_between,
    joinpoint_min_end,
    joinpoint_k_max,
    plot_joinpoint_log_true = TRUE,
    plot_joinpoint_log_false = TRUE
) {
  
  metadata   <- extract_series_metadata(selected_row)
  df_example <- selected_row[["data"]][[1]]
  
  preproc <- preprocess_series_for_methods(df_example$valor)
  
  dataset <- list(
    serie = preproc$serie,
    event = as.logical(df_example$is_true_cp)
  )
  
  cat(
    "Series preprocessing",
    "| shift_applied:", preproc$applied,
    "| shift:", round(preproc$shift, 6),
    "\n"
  )
  
  true_cps <- df_example$tempo[df_example$is_true_cp == 1]
  
  series_label <- paste0(
    "ID ", selected_row$serie_id,
    " | ", selected_row$serie_id_nome
  )
  
  series_subtitle_base <- paste0(
    "true cps: ", paste(true_cps, collapse = "; "),
    " | SoftED sw: ", softed_window_size
  )
  
  cat("\n=========================================================\n")
  cat("Preparing robust visual diagnostics (freeze active)\n")
  cat("=========================================================\n")
  
  joinpoint_available <- exists("hcp_joinpoint", mode = "function")
  
  # Frozen banks (per series)
  ae_member_bank <- build_ae_member_bank_frozen(dataset$serie)
  cf_member_bank <- build_cf_member_bank_frozen(dataset$serie)
  
  ae_ensemble_from_frozen <- build_ae_ensemble_from_frozen(ae_member_bank)
  cf_ensemble_from_frozen <- build_cf_ensemble_from_frozen(cf_member_bank)
  
  static_bank <- build_static_frozen_bank(
    serie = dataset$serie,
    binseg_Q = binseg_Q,
    bocpd_hazard = bocpd_hazard,
    bocpd_dist = bocpd_dist,
    bocpd_threshold = bocpd_threshold,
    bocpd_min_distance = bocpd_min_distance,
    bocpd_burn_in = bocpd_burn_in,
    joinpoint_min_between = joinpoint_min_between,
    joinpoint_min_end = joinpoint_min_end,
    joinpoint_k_max = joinpoint_k_max
  )
  
  diagnostic_metrics <- list()
  family_runs        <- list()
  joinpoint_runs     <- list()
  
  families <- get_active_ensemble_families(
    run_mfef_base = run_mfef_base,
    run_mfef_jp_false = run_mfef_jp_false,
    run_mfef_jp_true = run_mfef_jp_true
  )
  
  # Cache frozen AE/CF per config (computed once per config)
  ae_cf_by_config <- list()
  
  for (current_threshold in mfef_threshold_grid) {
    for (current_time_tolerance in mfef_time_tolerance_grid) {
      
      detection_config <- list(
        time_tolerance = current_time_tolerance,
        threshold = current_threshold,
        threshold_type = detection_threshold_type,
        use_nms = detection_use_nms
      )
      
      key <- paste0("thr_", current_threshold, "_tt_", current_time_tolerance)
      
      ae_det_cfg <- run_detection(ae_ensemble_from_frozen, dataset$serie, detection_config)
      cf_det_cfg <- run_detection(cf_ensemble_from_frozen, dataset$serie, detection_config)
      
      ae_cf_by_config[[key]] <- list(
        ae_frozen = hcp_frozen_detection(ae_det_cfg),
        cf_frozen = hcp_frozen_detection(cf_det_cfg)
      )
    }
  }
  
  # Family loop (using frozen components)
  for (family_name in families) {
    for (current_threshold in mfef_threshold_grid) {
      for (current_time_tolerance in mfef_time_tolerance_grid) {
        
        run_key <- paste0(
          family_name,
          "_thr_", format(current_threshold, trim = TRUE),
          "_tt_", current_time_tolerance
        )
        
        key <- paste0("thr_", current_threshold, "_tt_", current_time_tolerance)
        ae_frozen_config <- ae_cf_by_config[[key]]$ae_frozen
        cf_frozen_config <- ae_cf_by_config[[key]]$cf_frozen
        
        fam_res <- run_ensemble_family_one(
          metadata = metadata,
          family_name = family_name,
          dataset = dataset,
          df_example = df_example,
          softed_window_size = softed_window_size,
          current_time_tolerance = current_time_tolerance,
          current_threshold = current_threshold,
          detection_threshold_type = detection_threshold_type,
          detection_use_nms = detection_use_nms,
          ae_ensemble_frozen_config = ae_frozen_config,
          cf_ensemble_frozen_config = cf_frozen_config,
          static_bank = static_bank,
          joinpoint_available = joinpoint_available
        )
        
        diagnostic_metrics[[run_key]] <- fam_res$row
        
        if (!is.null(fam_res$run)) {
          family_runs[[run_key]] <- fam_res$run
        }
      }
    }
  }
  
  # Joinpoint baselines
  if (run_joinpoint_baseline) {
    
    joinpoint_variants <- tibble(
      model_detail = c("log_transform_TRUE", "log_transform_FALSE"),
      log_transform = c(TRUE, FALSE),
      should_run   = c(plot_joinpoint_log_true, plot_joinpoint_log_false)
    )
    
    for (i in seq_len(nrow(joinpoint_variants))) {
      
      current_detail        <- joinpoint_variants$model_detail[[i]]
      current_log_transform <- joinpoint_variants$log_transform[[i]]
      current_should_run    <- joinpoint_variants$should_run[[i]]
      
      if (!current_should_run) next
      
      run_key <- paste0("Joinpoint_", current_detail)
      
      jp_res <- run_joinpoint_baseline_one(
        metadata = metadata,
        dataset = dataset,
        df_example = df_example,
        softed_window_size = softed_window_size,
        current_log_transform = current_log_transform,
        current_detail = current_detail,
        static_bank = static_bank
      )
      
      diagnostic_metrics[[run_key]] <- jp_res$row
      
      if (!is.null(jp_res$run)) {
        joinpoint_runs[[current_detail]] <- jp_res$run
      }
    }
  }
  
  diagnostic_metrics <- bind_rows(diagnostic_metrics) %>%
    arrange(method, threshold, time_tolerance, model_detail)
  
  list(
    df_example = df_example,
    dataset = dataset,
    true_cps = true_cps,
    series_label = series_label,
    series_subtitle_base = series_subtitle_base,
    family_runs = family_runs,
    joinpoint_runs = joinpoint_runs,
    diagnostic_metrics = diagnostic_metrics
  )
}

plot_series_diagnostics <- function(plot_run) {
  
  dataset <- plot_run$dataset
  df_example <- plot_run$df_example
  
  # Ensemble families
  if (length(plot_run$family_runs) > 0) {
    for (nm in names(plot_run$family_runs)) {
      
      fam <- plot_run$family_runs[[nm]]
      
      cat("\n=========================================================\n")
      cat(
        "Plotting", fam$family_name,
        "| threshold =", fam$threshold,
        "| time_tolerance =", fam$time_tolerance,
        "\n"
      )
      cat("=========================================================\n")
      
      cat("\nDetected cps:\n")
      print(fam$detected_cps)
      
      tryCatch({
        p_models <- harbinger::har_ensemble_plot_models(
          detection = fam$detection,
          serie = dataset$serie,
          time_idx = df_example$tempo
        )
        print_harbinger_plot(p_models)
      }, error = function(e) {
        cat("\n[plot models error]", fam$family_name, "-", conditionMessage(e), "\n")
      })
      
      tryCatch({
        p_ensemble <- harbinger::har_ensemble_plot(
          detection = fam$detection,
          serie = dataset$serie,
          time_idx = df_example$tempo
        )
        print_harbinger_plot(p_ensemble)
      }, error = function(e) {
        cat("\n[plot ensemble error]", fam$family_name, "-", conditionMessage(e), "\n")
      })
      
      tryCatch({
        p_event <- harbinger::har_plot(
          fam$model,
          dataset$serie,
          fam$detection,
          dataset$event
        )
        
        print(
          p_event +
            labs(
              title = paste0(
                plot_run$series_label,
                " | ", fam$family_name,
                " | detection vs ground truth"
              ),
              subtitle = paste0(
                plot_run$series_subtitle_base,
                " | threshold: ", fam$threshold,
                " | time_tolerance: ", fam$time_tolerance,
                " | detected: ",
                paste(fam$detected_cps, collapse = "; ")
              ),
              x = "Time",
              y = "Value"
            )
        )
      }, error = function(e) {
        cat("\n[har_plot error]", fam$family_name, "-", conditionMessage(e), "\n")
      })
    }
  }
  
  # Joinpoint
  if (length(plot_run$joinpoint_runs) > 0) {
    for (nm in names(plot_run$joinpoint_runs)) {
      
      jp <- plot_run$joinpoint_runs[[nm]]
      
      cat("\n=========================================================\n")
      cat("Joinpoint plot |", nm, "\n")
      cat("=========================================================\n")
      
      cat("\nJoinpoint detected cps:\n")
      print(jp$detected_cps)
      
      tryCatch({
        p_joinpoint_event <- harbinger::har_plot(
          jp$model,
          dataset$serie,
          jp$detection,
          dataset$event
        )
        
        print(
          p_joinpoint_event +
            labs(
              title = paste0(
                plot_run$series_label,
                " | Joinpoint | detection vs ground truth"
              ),
              subtitle = paste0(
                plot_run$series_subtitle_base,
                " | ", nm,
                " | Joinpoint detected: ",
                paste(jp$detected_cps, collapse = "; ")
              ),
              x = "Time",
              y = "Value"
            )
        )
      }, error = function(e) {
        cat("\n[har_plot Joinpoint error]", nm, "-", conditionMessage(e), "\n")
      })
    }
  }
}

# =========================================================
# Build pool
# =========================================================
synthetic_pool <- build_synthetic_pool(synthetic_series)

n_available <- nrow(synthetic_pool)

cat("\nSeries pool for processing:\n")
cat("Number of available series:", n_available, "\n")

if (n_available == 0) {
  stop("No series available in the pool.")
}

# =========================================================
# Run batch
# =========================================================
if (run_batch) {
  
  batch_global_begin <- Sys.time()
  
  cat("\nBatch selected by serie_id:\n")
  cat("batch_start_serie_id:", batch_start_serie_id, "\n")
  cat("batch_end_serie_id:", batch_end_serie_id, "\n")
  cat("Batch CSV:", results_csv_file, "\n")
  
  if (batch_start_serie_id > batch_end_serie_id) {
    stop("batch_start_serie_id cannot be greater than batch_end_serie_id.")
  }
  
  batch_tbl <- synthetic_pool %>%
    filter(
      serie_id >= batch_start_serie_id,
      serie_id <= batch_end_serie_id
    )
  
  if (nrow(batch_tbl) == 0) {
    stop("No series found in the given serie_id range.")
  }
  
  cat("\nSeries selected in the batch:", nrow(batch_tbl), "\n")
  cat("First serie_id:", batch_tbl$serie_id[[1]], "\n")
  cat("Last serie_id:", batch_tbl$serie_id[[nrow(batch_tbl)]], "\n")
  
  all_results <- vector("list", nrow(batch_tbl))
  
  for (i in seq_len(nrow(batch_tbl))) {
    
    selected_row <- batch_tbl[i, , drop = FALSE]
    series_begin <- Sys.time()
    
    cat("\n=========================================================\n")
    cat(
      "Processing serie_id =",
      selected_row$serie_id,
      "|",
      i,
      "of",
      nrow(batch_tbl),
      "\n"
    )
    cat("Start:", format(series_begin, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("=========================================================\n")
    
    all_results[[i]] <- tryCatch({
      
      run_one_series_comparison(
        selected_row = selected_row,
        detection_time_tolerance_grid = detection_time_tolerance_grid,
        detection_threshold_grid = detection_threshold_grid,
        softed_window_size = softed_window_size,
        detection_threshold_type = detection_threshold_type,
        detection_use_nms = detection_use_nms,
        run_joinpoint_baseline = run_joinpoint_baseline,
        run_mfef_base = run_mfef_base,
        run_mfef_jp_false = run_mfef_jp_false,
        run_mfef_jp_true = run_mfef_jp_true,
        bocpd_hazard = bocpd_hazard,
        bocpd_dist = bocpd_dist,
        bocpd_threshold = bocpd_threshold,
        bocpd_min_distance = bocpd_min_distance,
        bocpd_burn_in = bocpd_burn_in,
        binseg_Q = binseg_Q,
        joinpoint_min_between = joinpoint_min_between,
        joinpoint_min_end = joinpoint_min_end,
        joinpoint_k_max = joinpoint_k_max
      )
      
    }, error = function(e) {
      
      metadata <- extract_series_metadata(selected_row)
      df_tmp <- selected_row[["data"]][[1]]
      
      make_empty_result_row(
        metadata = metadata,
        method = NA_character_,
        model_detail = NA_character_,
        time_tolerance = NA_real_,
        threshold = NA_real_,
        log_transform = NA,
        true_cps = paste(df_tmp$tempo[df_tmp$is_true_cp == 1], collapse = "; "),
        n_true = sum(df_tmp$is_true_cp == 1),
        status = "series_error",
        error_message = conditionMessage(e)
      )
    })
    
    series_end <- Sys.time()
    
    cat("End:", format(series_end, "%Y-%m-%d %H:%M:%S"), "\n")
    cat(
      "Elapsed serie_id", selected_row$serie_id, ":",
      format_elapsed(difftime(series_end, series_begin, units = "secs")),
      "\n"
    )
    
    if (i %% 10 == 0) {
      invisible(gc(verbose = FALSE))
    }
  }
  
  all_results <- bind_rows(all_results) %>%
    arrange(
      serie_id,
      method,
      threshold,
      model_detail,
      time_tolerance
    )
  
  save_batch_begin <- Sys.time()
  write_csv(all_results, results_csv_file)
  save_batch_end <- Sys.time()
  
  batch_global_end <- Sys.time()
  
  cat("\nBatch file saved:\n")
  cat("Batch CSV:", results_csv_file, "\n")
  cat(
    "Time saving batch CSV:",
    format_elapsed(difftime(save_batch_end, save_batch_begin, units = "secs")),
    "\n"
  )
  
  cat(
    "\nTotal batch time:",
    format_elapsed(difftime(batch_global_end, batch_global_begin, units = "secs")),
    "\n"
  )
  
  if (open_viewer_after_run) {
    safe_view(all_results, title = "all_results")
  }
}

# =========================================================
# Optional diagnostic plots for one series
# =========================================================
if (plot_diagnostics) {
  
  plot_selected_row <- synthetic_pool %>%
    filter(serie_id == plot_target_serie_id)
  
  if (nrow(plot_selected_row) == 0) {
    stop("Series for plotting not found in synthetic_pool.")
  }
  
  if (nrow(plot_selected_row) > 1) {
    stop("More than one series found for plotting. Check plot_target_serie_id.")
  }
  
  cat("\n=========================================================\n")
  cat("Running visual diagnostics\n")
  cat("serie_id:", plot_selected_row$serie_id, "\n")
  cat("Active families:", paste(
    get_active_ensemble_families(
      run_mfef_base = run_mfef_base,
      run_mfef_jp_false = run_mfef_jp_false,
      run_mfef_jp_true = run_mfef_jp_true
    ),
    collapse = "; "
  ), "\n")
  cat("tt grid:", paste(plot_mfef_time_tolerance_grid, collapse = "; "), "\n")
  cat("thr grid:", paste(plot_mfef_threshold_grid, collapse = "; "), "\n")
  cat("Joinpoint log TRUE:", plot_joinpoint_log_true, "\n")
  cat("Joinpoint log FALSE:", plot_joinpoint_log_false, "\n")
  cat("=========================================================\n")
  
  plot_run <- run_one_series_for_plots(
    selected_row = plot_selected_row,
    mfef_time_tolerance_grid = plot_mfef_time_tolerance_grid,
    mfef_threshold_grid = plot_mfef_threshold_grid,
    softed_window_size = softed_window_size,
    detection_threshold_type = detection_threshold_type,
    detection_use_nms = detection_use_nms,
    run_joinpoint_baseline = run_joinpoint_baseline,
    run_mfef_base = run_mfef_base,
    run_mfef_jp_false = run_mfef_jp_false,
    run_mfef_jp_true = run_mfef_jp_true,
    bocpd_hazard = bocpd_hazard,
    bocpd_dist = bocpd_dist,
    bocpd_threshold = bocpd_threshold,
    bocpd_min_distance = bocpd_min_distance,
    bocpd_burn_in = bocpd_burn_in,
    binseg_Q = binseg_Q,
    joinpoint_min_between = joinpoint_min_between,
    joinpoint_min_end = joinpoint_min_end,
    joinpoint_k_max = joinpoint_k_max,
    plot_joinpoint_log_true = plot_joinpoint_log_true,
    plot_joinpoint_log_false = plot_joinpoint_log_false
  )
  
  if (show_diagnostic_metrics) {
    
    cat("\n=========================================================\n")
    cat("Comparative diagnostic metrics\n")
    cat("=========================================================\n")
    
    diagnostic_metrics_view <<- plot_run$diagnostic_metrics %>%
      arrange(
        method,
        threshold,
        time_tolerance,
        model_detail
      )
    
    print(diagnostic_metrics_view, n = Inf, width = Inf)
    
    if (open_diagnostic_metrics_view) {
      View(diagnostic_metrics_view)
    }
  }
  
  plot_series_diagnostics(plot_run)
}