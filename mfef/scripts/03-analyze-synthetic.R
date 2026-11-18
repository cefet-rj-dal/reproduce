# =========================================================
# 03-analyze-synthetic.R
# Statistical analysis and figures for the official synthetic
# detection results.
#
# Input:
#   outputs/synthetic/detections/*.csv
#
# Outputs:
#   validated analysis objects and summary tables in
#   results/synthetic/;
#   PDF figures in figures/synthetic/.
# =========================================================

library(tidyverse)
library(here)

# =========================================================
# OUTPUT DIRECTORIES
# =========================================================

synthetic_results_directory <- here::here(
  "results",
  "synthetic"
)

synthetic_figures_directory <- here::here(
  "figures",
  "synthetic"
)

dir.create(
  synthetic_results_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  synthetic_figures_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

# =========================================================
# GLOBAL OPTIONS
# =========================================================

run_bootstrap <- TRUE
n_boot <- 1000L

if (
  !is.logical(run_bootstrap) ||
  length(run_bootstrap) != 1L ||
  is.na(run_bootstrap)
) {
  stop(
    "run_bootstrap must be one non-missing logical value.",
    call. = FALSE
  )
}

if (
  run_bootstrap &&
  (
    !is.numeric(n_boot) ||
    length(n_boot) != 1L ||
    is.na(n_boot) ||
    n_boot < 1 ||
    n_boot != as.integer(n_boot)
  )
) {
  stop(
    "n_boot must be one positive integer.",
    call. = FALSE
  )
}

# =========================================================
# PLOT SETTINGS
# =========================================================

wong_palette <- c(
  "Joinpoint" = "#D55E00",
  "MFEF" = "#009E73",
  "MFEF+JP" = "#0072B2",
  "None" = "#999999"
)

theme_pub <- function(base_size = 10) {
  theme_classic(
    base_size = base_size
  ) +
    theme(
      axis.title = element_text(
        size = base_size
      ),
      
      axis.text = element_text(
        size = base_size - 1,
        color = "black"
      ),
      
      legend.title = element_text(
        size = base_size
      ),
      
      legend.text = element_text(
        size = base_size - 1
      ),
      
      legend.position = "right",
      
      legend.key.size = grid::unit(
        0.4,
        "cm"
      ),
      
      strip.background = element_blank(),
      
      strip.text = element_text(
        size = base_size,
        face = "bold"
      ),
      
      panel.grid.major = element_line(
        color = "grey92",
        linewidth = 0.3
      ),
      
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(
        5,
        10,
        5,
        5
      )
    )
}

# =========================================================
# INPUT DATA
# =========================================================

detections_dir <- here::here(
  "outputs",
  "synthetic",
  "detections"
)

detection_batch_files <- c(
  file.path(
    detections_dir,
    paste0(
      "batch_mfef_joinpoint_hybrid_soft_hard_",
      "id_0_850_sw_6_thrgrid_0p90_0p95.csv"
    )
  ),
  
  file.path(
    detections_dir,
    paste0(
      "batch_mfef_joinpoint_hybrid_soft_hard_",
      "id_851_1679_sw_6_thrgrid_0p90_0p95.csv"
    )
  )
)

missing_input_files <- detection_batch_files[
  !file.exists(
    detection_batch_files
  )
]

if (length(missing_input_files) > 0L) {
  stop(
    paste0(
      "Missing input file(s):\n",
      paste(
        missing_input_files,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

read_detection_batch <- function(file_path) {
  readr::read_csv(
    file_path,
    col_types = readr::cols(
      .default = readr::col_guess(),
      error_message = readr::col_character()
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
}

detection_batches <- purrr::map(
  detection_batch_files,
  read_detection_batch
)

names(detection_batches) <- paste0(
  "batch_",
  seq_along(
    detection_batches
  )
)

input_parsing_problems <- purrr::imap_dfr(
  detection_batches,
  function(batch_data, batch_name) {
    readr::problems(
      batch_data
    ) %>%
      mutate(
        batch_id = batch_name,
        .before = 1
      )
  }
)

if (nrow(input_parsing_problems) > 0L) {
  print(
    input_parsing_problems,
    n = Inf,
    width = Inf
  )
  
  stop(
    "Input parsing problems remain after explicit column typing.",
    call. = FALSE
  )
}

detection_results <- purrr::imap_dfr(
  detection_batches,
  function(batch_data, batch_name) {
    batch_data %>%
      mutate(
        batch_id = batch_name,
        .before = 1
      )
  }
)

if (nrow(detection_results) == 0L) {
  stop(
    "The combined detection dataset is empty.",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("INPUT DATA\n")
cat("=====================================\n")

for (
  batch_name in
  names(detection_batches)
) {
  cat(
    batch_name,
    "rows:",
    nrow(
      detection_batches[[
        batch_name
      ]]
    ),
    "\n"
  )
}

cat(
  "Combined rows:",
  nrow(detection_results),
  "\n"
)

cat(
  "Combined columns:",
  ncol(detection_results),
  "\n"
)

cat("Parsing problems: 0\n")

rm(
  detection_batches
)

# =========================================================
# Q1 - EXCLUDED SERIES
# =========================================================

expected_failed_series <- c(
  243L,
  363L,
  837L,
  1113L,
  1191L,
  1231L,
  1509L,
  1641L,
  1653L
)

error_series <- detection_results %>%
  filter(
    status != "ok" |
      !is.na(error_message)
  ) %>%
  distinct(
    serie_id,
    serie_id_nome,
    status,
    error_message
  ) %>%
  arrange(
    serie_id
  )

if (
  !setequal(
    error_series$serie_id,
    expected_failed_series
  )
) {
  stop(
    paste0(
      "The recorded error series do not match ",
      "the predefined exclusion list."
    ),
    call. = FALSE
  )
}

if (
  n_distinct(
    error_series$serie_id
  ) !=
  length(
    expected_failed_series
  )
) {
  stop(
    "The excluded-series table contains duplicated series.",
    call. = FALSE
  )
}

analysis_results <- detection_results %>%
  filter(
    status == "ok",
    !serie_id %in%
      expected_failed_series
  )

if (
  anyNA(
    analysis_results$status
  ) ||
  any(
    analysis_results$status != "ok"
  ) ||
  any(
    !is.na(
      analysis_results$error_message
    )
  )
) {
  stop(
    "The analytical dataset contains unresolved execution errors.",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("Q1 - EXCLUDED SERIES\n")
cat("=====================================\n")

cat(
  "Excluded series:",
  n_distinct(
    error_series$serie_id
  ),
  "\n"
)

print(
  error_series %>%
    select(
      serie_id,
      serie_id_nome,
      error_message
    ),
  n = Inf,
  width = Inf
)

# =========================================================
# Q2 - ANALYTICAL DATASET AND F1 STANDARDIZATION
# =========================================================

n_valid_series <- n_distinct(
  analysis_results$serie_id
)

n_valid_rows <- nrow(
  analysis_results
)

if (n_valid_series != 1671L) {
  stop(
    paste0(
      "Expected 1,671 valid series, but found ",
      n_valid_series,
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Standardize undefined F1 values
# ---------------------------------------------------------
# Undefined F1 values are set to zero only when a series has
# reference change-points and both precision and recall are zero.
# Raw F1 values are retained for traceability.

unexpected_hard_f1_missing <- analysis_results %>%
  filter(
    n_true > 0,
    is.na(hard_F1),
    !(
      !is.na(hard_precision) &
        !is.na(hard_recall) &
        hard_precision == 0 &
        hard_recall == 0
    )
  )

unexpected_soft_f1_missing <- analysis_results %>%
  filter(
    n_true > 0,
    is.na(soft_F1),
    !(
      !is.na(soft_precision) &
        !is.na(soft_recall) &
        soft_precision == 0 &
        soft_recall == 0
    )
  )

if (
  nrow(unexpected_hard_f1_missing) > 0L ||
  nrow(unexpected_soft_f1_missing) > 0L
) {
  stop(
    paste0(
      "Unexpected missing F1 values were found in series ",
      "with reference change-points."
    ),
    call. = FALSE
  )
}

analysis_results <- analysis_results %>%
  mutate(
    hard_F1_raw =
      hard_F1,
    
    soft_F1_raw =
      soft_F1,
    
    hard_F1 = case_when(
      n_true > 0 &
        is.na(hard_F1_raw) &
        hard_precision == 0 &
        hard_recall == 0 ~
        0,
      
      TRUE ~
        hard_F1_raw
    ),
    
    soft_F1 = case_when(
      n_true > 0 &
        is.na(soft_F1_raw) &
        soft_precision == 0 &
        soft_recall == 0 ~
        0,
      
      TRUE ~
        soft_F1_raw
    )
  )

f1_standardization_summary <- analysis_results %>%
  summarise(
    n_hard_standardized = sum(
      n_true > 0 &
        is.na(hard_F1_raw) &
        hard_F1 == 0
    ),
    
    n_soft_standardized = sum(
      n_true > 0 &
        is.na(soft_F1_raw) &
        soft_F1 == 0
    ),
    
    n_hard_missing_with_cp = sum(
      n_true > 0 &
        is.na(hard_F1)
    ),
    
    n_soft_missing_with_cp = sum(
      n_true > 0 &
        is.na(soft_F1)
    ),
    
    n_hard_missing_null = sum(
      n_true == 0 &
        is.na(hard_F1)
    ),
    
    n_soft_missing_null = sum(
      n_true == 0 &
        is.na(soft_F1)
    )
  )

if (
  f1_standardization_summary$
  n_hard_missing_with_cp > 0L ||
  f1_standardization_summary$
  n_soft_missing_with_cp > 0L
) {
  stop(
    paste0(
      "F1 standardization left unresolved values ",
      "in series with reference change-points."
    ),
    call. = FALSE
  )
}

if (
  any(
    analysis_results$hard_F1 < 0 |
    analysis_results$hard_F1 > 1,
    na.rm = TRUE
  ) ||
  any(
    analysis_results$soft_F1 < 0 |
    analysis_results$soft_F1 > 1,
    na.rm = TRUE
  )
) {
  stop(
    "Standardized F1 values must remain within [0, 1].",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("Q2 - ANALYTICAL DATASET AND F1 STANDARDIZATION\n")
cat("=====================================\n")
cat("Valid series:", n_valid_series, "\n")
cat("Valid rows:", n_valid_rows, "\n")

print(
  f1_standardization_summary,
  width = Inf
)

# =========================================================
# Q3 - ANALYTICAL VARIABLES AND CONFIGURATION CATALOG
# =========================================================

build_method_label <- function(
    method,
    log_transform,
    time_tolerance,
    threshold) {
  
  supported_methods <- c(
    "Joinpoint",
    "MFEF",
    "MFEF_JP_TRUE",
    "MFEF_JP_FALSE"
  )
  
  if (anyNA(method)) {
    stop(
      "Missing method values were found.",
      call. = FALSE
    )
  }
  
  unsupported_methods <- setdiff(
    unique(method),
    supported_methods
  )
  
  if (length(unsupported_methods) > 0L) {
    stop(
      paste0(
        "Unsupported method(s): ",
        paste(
          unsupported_methods,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      method == "Joinpoint" &
      is.na(log_transform)
    )
  ) {
    stop(
      "Joinpoint rows contain missing log_transform values.",
      call. = FALSE
    )
  }
  
  ensemble_rows <- method %in% c(
    "MFEF",
    "MFEF_JP_TRUE",
    "MFEF_JP_FALSE"
  )
  
  if (
    any(
      ensemble_rows &
      (
        is.na(time_tolerance) |
        is.na(threshold)
      )
    )
  ) {
    stop(
      paste0(
        "MFEF and MFEF+JP rows require complete ",
        "time_tolerance and threshold values."
      ),
      call. = FALSE
    )
  }
  
  case_when(
    method == "Joinpoint" ~
      paste0(
        "Joinpoint(",
        if_else(
          log_transform,
          "T",
          "F"
        ),
        ")"
      ),
    
    method == "MFEF" ~
      paste0(
        "MFEF(",
        time_tolerance,
        ",",
        threshold,
        ")"
      ),
    
    method == "MFEF_JP_TRUE" ~
      paste0(
        "MFEF+JP(T,",
        time_tolerance,
        ",",
        threshold,
        ")"
      ),
    
    method == "MFEF_JP_FALSE" ~
      paste0(
        "MFEF+JP(F,",
        time_tolerance,
        ",",
        threshold,
        ")"
      )
  )
}

analysis_results <- analysis_results %>%
  mutate(
    regime = case_when(
      str_detect(
        serie_id_nome,
        "^mean_"
      ) ~
        "mean",
      
      str_detect(
        serie_id_nome,
        "^trend_"
      ) ~
        "trend",
      
      str_detect(
        serie_id_nome,
        "^variance_"
      ) ~
        "variance",
      
      str_detect(
        serie_id_nome,
        "^null_"
      ) ~
        "null",
      
      TRUE ~
        NA_character_
    ),
    
    data_type = case_when(
      str_detect(
        serie_id_nome,
        "_hospitalization_"
      ) ~
        "hospitalization",
      
      str_detect(
        serie_id_nome,
        "_mortality_"
      ) ~
        "mortality",
      
      TRUE ~
        NA_character_
    ),
    
    cp_position = str_extract(
      serie_id_nome,
      "(?<=_)\\d+q(?=_)"
    ),
    
    magnitude = case_when(
      str_detect(
        serie_id_nome,
        "_high_high_"
      ) ~
        "high_high",
      
      str_detect(
        serie_id_nome,
        "_high_low_"
      ) ~
        "high_low",
      
      str_detect(
        serie_id_nome,
        "_low_high_"
      ) ~
        "low_high",
      
      str_detect(
        serie_id_nome,
        "_low_low_"
      ) ~
        "low_low",
      
      TRUE ~
        NA_character_
    ),
    
    ar1 = case_when(
      str_detect(
        serie_id_nome,
        "_with_ar1_"
      ) ~
        "with_ar1",
      
      str_detect(
        serie_id_nome,
        "_no_ar1_"
      ) ~
        "no_ar1",
      
      TRUE ~
        NA_character_
    ),
    
    method_config = case_when(
      method == "Joinpoint" ~
        paste0(
          method,
          "_log_",
          if_else(
            log_transform,
            "TRUE",
            "FALSE"
          )
        ),
      
      method %in% c(
        "MFEF",
        "MFEF_JP_TRUE",
        "MFEF_JP_FALSE"
      ) ~
        paste0(
          method,
          "_tt_",
          time_tolerance,
          "_thr_",
          threshold
        ),
      
      TRUE ~
        NA_character_
    ),
    
    method_label = build_method_label(
      method,
      log_transform,
      time_tolerance,
      threshold
    ),
    
    method_family = case_when(
      method == "Joinpoint" ~
        "Joinpoint",
      
      method == "MFEF" ~
        "MFEF",
      
      method %in% c(
        "MFEF_JP_TRUE",
        "MFEF_JP_FALSE"
      ) ~
        "MFEF+JP",
      
      TRUE ~
        NA_character_
    )
  )

# ---------------------------------------------------------
# Validate extracted analytical variables
# ---------------------------------------------------------

required_analytical_variables <- c(
  "regime",
  "data_type",
  "method_config",
  "method_label",
  "method_family",
  "ar1"
)

missing_required_variables <- required_analytical_variables[
  purrr::map_lgl(
    analysis_results[
      required_analytical_variables
    ],
    ~ anyNA(.x)
  )
]

if (length(missing_required_variables) > 0L) {
  stop(
    paste0(
      "Missing values were produced in: ",
      paste(
        missing_required_variables,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

if (
  any(
    analysis_results$n_true > 0 &
    is.na(
      analysis_results$cp_position
    )
  ) ||
  any(
    analysis_results$n_true > 0 &
    is.na(
      analysis_results$magnitude
    )
  )
) {
  stop(
    paste0(
      "Series with reference change-points require complete ",
      "cp_position and magnitude values."
    ),
    call. = FALSE
  )
}

if (
  any(
    analysis_results$n_true == 0 &
    !is.na(
      analysis_results$cp_position
    )
  )
) {
  stop(
    "Null series must not contain a change-point position.",
    call. = FALSE
  )
}

analysis_results <- analysis_results %>%
  mutate(
    regime = factor(
      regime,
      levels = c(
        "mean",
        "trend",
        "variance",
        "null"
      )
    ),
    
    data_type = factor(
      data_type,
      levels = c(
        "hospitalization",
        "mortality"
      )
    ),
    
    magnitude = factor(
      magnitude,
      levels = c(
        "low_low",
        "low_high",
        "high_low",
        "high_high"
      )
    ),
    
    ar1 = factor(
      ar1,
      levels = c(
        "no_ar1",
        "with_ar1"
      )
    ),
    
    method_family = factor(
      method_family,
      levels = c(
        "Joinpoint",
        "MFEF",
        "MFEF+JP"
      )
    )
  )

if (
  anyNA(analysis_results$regime) ||
  anyNA(analysis_results$data_type) ||
  anyNA(analysis_results$ar1) ||
  anyNA(analysis_results$method_family)
) {
  stop(
    "Factor conversion produced missing analytical values.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Validate the configuration-series design
# ---------------------------------------------------------

configuration_series_check <- analysis_results %>%
  count(
    serie_id,
    method_label,
    name = "n_rows"
  )

configuration_count_by_series <- configuration_series_check %>%
  count(
    serie_id,
    name = "n_configs"
  )

if (
  any(
    configuration_series_check$
    n_rows != 1L
  ) ||
  any(
    configuration_count_by_series$
    n_configs != 20L
  )
) {
  stop(
    paste0(
      "The analytical dataset must contain one result per ",
      "series and configuration, with 20 configurations per series."
    ),
    call. = FALSE
  )
}

if (
  nrow(
    configuration_count_by_series
  ) != n_valid_series
) {
  stop(
    "The configuration-series design does not cover all valid series.",
    call. = FALSE
  )
}

config_catalog <- analysis_results %>%
  distinct(
    method_label,
    method_family
  ) %>%
  arrange(
    method_family,
    method_label
  )

if (
  nrow(config_catalog) != 20L ||
  anyDuplicated(
    config_catalog$method_label
  ) > 0L
) {
  stop(
    paste0(
      "Expected 20 unique configurations, but found ",
      nrow(config_catalog),
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Create the primary analytical subsets
# ---------------------------------------------------------

series_with_cp <- analysis_results %>%
  filter(
    n_true > 0
  )

series_without_cp <- analysis_results %>%
  filter(
    n_true == 0
  )

n_series_with_cp <- n_distinct(
  series_with_cp$serie_id
)

n_series_without_cp <- n_distinct(
  series_without_cp$serie_id
)

if (
  n_series_with_cp != 1431L ||
  n_series_without_cp != 240L ||
  n_series_with_cp +
  n_series_without_cp !=
  n_valid_series
) {
  stop(
    paste0(
      "The analytical subsets must contain 1,431 series with ",
      "reference change-points and 240 null series."
    ),
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("Q3 - ANALYTICAL VARIABLES AND CONFIGURATION CATALOG\n")
cat("=====================================\n")
cat("Unique configurations:", nrow(config_catalog), "\n")
cat("Configurations per series: 20\n")
cat("Series with reference change-points:", n_series_with_cp, "\n")
cat("Null series:", n_series_without_cp, "\n")

print(
  config_catalog,
  n = Inf,
  width = Inf
)

cat("\nCore analytical objects retained in:\n")
cat("  analysis_results\n")
cat("  config_catalog\n")
cat("  series_with_cp\n")
cat("  series_without_cp\n")

# =========================================================
# HELPER FUNCTIONS
# =========================================================

validate_metric <- function(
    df,
    metric_col) {
  
  if (!metric_col %in% names(df)) {
    stop(
      paste0(
        "Metric column not found: ",
        metric_col,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (!is.numeric(df[[metric_col]])) {
    stop(
      paste0(
        "Metric column must be numeric: ",
        metric_col,
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

count_series_without_positive_score <- function(
    df,
    metric_col) {
  
  validate_metric(
    df,
    metric_col
  )
  
  df %>%
    group_by(
      serie_id
    ) %>%
    summarise(
      has_positive_score = any(
        !is.na(.data[[metric_col]]) &
          .data[[metric_col]] > 0
      ),
      .groups = "drop"
    ) %>%
    summarise(
      n_series = sum(
        !has_positive_score
      )
    ) %>%
    pull(
      n_series
    )
}

find_winners <- function(
    df,
    metric_col) {
  
  validate_metric(
    df,
    metric_col
  )
  
  df %>%
    group_by(
      serie_id
    ) %>%
    filter(
      any(
        !is.na(.data[[metric_col]]) &
          .data[[metric_col]] > 0
      )
    ) %>%
    mutate(
      best_metric = max(
        .data[[metric_col]],
        na.rm = TRUE
      )
    ) %>%
    filter(
      !is.na(.data[[metric_col]]),
      .data[[metric_col]] > 0,
      .data[[metric_col]] == best_metric
    ) %>%
    ungroup()
}

bootstrap_median_ci <- function(
    x,
    n_boot,
    alpha = 0.05) {
  
  x <- x[
    !is.na(x) &
      x > 0
  ]
  
  if (length(x) == 0L) {
    return(
      c(
        point = NA_real_,
        lower = NA_real_,
        upper = NA_real_
      )
    )
  }
  
  bootstrap_values <- replicate(
    n_boot,
    median(
      sample(
        x,
        size = length(x),
        replace = TRUE
      )
    )
  )
  
  c(
    point = median(x),
    
    lower = quantile(
      bootstrap_values,
      probs = alpha / 2,
      names = FALSE,
      type = 7
    ),
    
    upper = quantile(
      bootstrap_values,
      probs = 1 - alpha / 2,
      names = FALSE,
      type = 7
    )
  )
}

# =========================================================
# Q4 - OVERALL WINNER ASSIGNMENTS UNDER HARD F1
# =========================================================
# A winner assignment is recorded whenever a configuration
# attains the highest positive hard F1 on a series. Tied
# configurations each receive one assignment.

n_series_overall <- n_distinct(
  series_with_cp$serie_id
)

if (
  n_series_overall !=
  n_series_with_cp
) {
  stop(
    "The hard-F1 analysis does not cover all series with change-points.",
    call. = FALSE
  )
}

n_no_positive_hard <- count_series_without_positive_score(
  df = series_with_cp,
  metric_col = "hard_F1"
)

n_positive_hard <-
  n_series_overall -
  n_no_positive_hard

hard_winners <- find_winners(
  df = series_with_cp,
  metric_col = "hard_F1"
)

hard_winner_ranking <- config_catalog %>%
  left_join(
    hard_winners %>%
      count(
        method_label,
        name = "n_wins"
      ),
    by = "method_label"
  ) %>%
  mutate(
    n_wins = replace_na(
      n_wins,
      0L
    )
  ) %>%
  arrange(
    desc(
      n_wins
    ),
    method_label
  )

hard_winner_counts_by_series <- hard_winners %>%
  count(
    serie_id,
    name = "n_winners"
  )

n_tied_series_hard <- sum(
  hard_winner_counts_by_series$
    n_winners > 1L
)

n_unique_winner_series_hard <- sum(
  hard_winner_counts_by_series$
    n_winners == 1L
)

n_winner_assignments_hard <- nrow(
  hard_winners
)

if (
  nrow(hard_winner_ranking) != 20L ||
  anyNA(hard_winner_ranking$method_family) ||
  any(hard_winner_ranking$n_wins < 0L)
) {
  stop(
    "The hard-F1 winner ranking is incomplete or invalid.",
    call. = FALSE
  )
}

if (
  nrow(hard_winner_counts_by_series) !=
  n_positive_hard
) {
  stop(
    paste0(
      "The hard-F1 winner results do not match the number ",
      "of positively scored series."
    ),
    call. = FALSE
  )
}

if (
  n_unique_winner_series_hard +
  n_tied_series_hard !=
  n_positive_hard
) {
  stop(
    paste0(
      "Unique- and tied-winner counts do not match the number ",
      "of positively scored series under hard F1."
    ),
    call. = FALSE
  )
}

if (
  sum(
    hard_winner_ranking$n_wins
  ) !=
  n_winner_assignments_hard
) {
  stop(
    "The hard-F1 winner-assignment totals are inconsistent.",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("Q4 - OVERALL WINNER ASSIGNMENTS UNDER HARD F1\n")
cat("=====================================\n")
cat("Series evaluated:", n_series_overall, "\n")

cat(
  "Series with positive hard F1:",
  n_positive_hard,
  sprintf(
    " (%.1f%%)\n",
    100 *
      n_positive_hard /
      n_series_overall
  )
)

cat(
  "Series without positive hard F1:",
  n_no_positive_hard,
  sprintf(
    " (%.1f%%)\n",
    100 *
      n_no_positive_hard /
      n_series_overall
  )
)

cat(
  "Series with a unique winner:",
  n_unique_winner_series_hard,
  "\n"
)

cat(
  "Series with tied winners:",
  n_tied_series_hard,
  "\n"
)

cat(
  "Winner assignments:",
  n_winner_assignments_hard,
  "\n"
)

print(
  hard_winner_ranking,
  n = Inf,
  width = Inf
)

# =========================================================
# Q5 - OVERALL WINNER ASSIGNMENTS UNDER SOFT F1
# =========================================================
# A winner assignment is recorded whenever a configuration
# attains the highest positive soft F1 on a series. Tied
# configurations each receive one assignment.

n_no_positive_soft <- count_series_without_positive_score(
  df = series_with_cp,
  metric_col = "soft_F1"
)

n_positive_soft <-
  n_series_overall -
  n_no_positive_soft

soft_winners <- find_winners(
  df = series_with_cp,
  metric_col = "soft_F1"
)

soft_winner_ranking <- config_catalog %>%
  left_join(
    soft_winners %>%
      count(
        method_label,
        name = "n_wins"
      ),
    by = "method_label"
  ) %>%
  mutate(
    n_wins = replace_na(
      n_wins,
      0L
    )
  ) %>%
  arrange(
    desc(
      n_wins
    ),
    method_label
  )

soft_winner_counts_by_series <- soft_winners %>%
  count(
    serie_id,
    name = "n_winners"
  )

n_tied_series_soft <- sum(
  soft_winner_counts_by_series$
    n_winners > 1L
)

n_unique_winner_series_soft <- sum(
  soft_winner_counts_by_series$
    n_winners == 1L
)

n_winner_assignments_soft <- nrow(
  soft_winners
)

if (
  nrow(soft_winner_ranking) != 20L ||
  anyNA(soft_winner_ranking$method_family) ||
  any(soft_winner_ranking$n_wins < 0L)
) {
  stop(
    "The soft-F1 winner ranking is incomplete or invalid.",
    call. = FALSE
  )
}

if (
  nrow(soft_winner_counts_by_series) !=
  n_positive_soft
) {
  stop(
    paste0(
      "The soft-F1 winner results do not match the number ",
      "of positively scored series."
    ),
    call. = FALSE
  )
}

if (
  n_unique_winner_series_soft +
  n_tied_series_soft !=
  n_positive_soft
) {
  stop(
    paste0(
      "Unique- and tied-winner counts do not match the number ",
      "of positively scored series under soft F1."
    ),
    call. = FALSE
  )
}

if (
  sum(
    soft_winner_ranking$n_wins
  ) !=
  n_winner_assignments_soft
) {
  stop(
    "The soft-F1 winner-assignment totals are inconsistent.",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("Q5 - OVERALL WINNER ASSIGNMENTS UNDER SOFT F1\n")
cat("=====================================\n")
cat("Series evaluated:", n_series_overall, "\n")

cat(
  "Series with positive soft F1:",
  n_positive_soft,
  sprintf(
    " (%.1f%%)\n",
    100 *
      n_positive_soft /
      n_series_overall
  )
)

cat(
  "Series without positive soft F1:",
  n_no_positive_soft,
  sprintf(
    " (%.1f%%)\n",
    100 *
      n_no_positive_soft /
      n_series_overall
  )
)

cat(
  "Series with a unique winner:",
  n_unique_winner_series_soft,
  "\n"
)

cat(
  "Series with tied winners:",
  n_tied_series_soft,
  "\n"
)

cat(
  "Winner assignments:",
  n_winner_assignments_soft,
  "\n"
)

print(
  soft_winner_ranking,
  n = Inf,
  width = Inf
)

# =========================================================
# Q6 - CONSOLIDATED HARD- AND SOFT-F1 SUMMARY
# =========================================================
# Positive-score coverage, winner assignments, and conditional
# F1 are summarized separately for hard and soft matching.

positive_score_coverage <- series_with_cp %>%
  group_by(
    method_label,
    method_family
  ) %>%
  summarise(
    n_positive_hard = sum(
      !is.na(hard_F1) &
        hard_F1 > 0
    ),
    
    n_positive_soft = sum(
      !is.na(soft_F1) &
        soft_F1 > 0
    ),
    
    .groups = "drop"
  )

conditional_quality <- series_with_cp %>%
  group_by(
    method_label
  ) %>%
  summarise(
    mean_hard_positive = if (
      any(
        !is.na(hard_F1) &
        hard_F1 > 0
      )
    ) {
      mean(
        hard_F1[
          !is.na(hard_F1) &
            hard_F1 > 0
        ]
      )
    } else {
      NA_real_
    },
    
    median_hard_positive = if (
      any(
        !is.na(hard_F1) &
        hard_F1 > 0
      )
    ) {
      median(
        hard_F1[
          !is.na(hard_F1) &
            hard_F1 > 0
        ]
      )
    } else {
      NA_real_
    },
    
    mean_soft_positive = if (
      any(
        !is.na(soft_F1) &
        soft_F1 > 0
      )
    ) {
      mean(
        soft_F1[
          !is.na(soft_F1) &
            soft_F1 > 0
        ]
      )
    } else {
      NA_real_
    },
    
    median_soft_positive = if (
      any(
        !is.na(soft_F1) &
        soft_F1 > 0
      )
    ) {
      median(
        soft_F1[
          !is.na(soft_F1) &
            soft_F1 > 0
        ]
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

hard_winner_quality <- hard_winners %>%
  group_by(
    method_label
  ) %>%
  summarise(
    mean_hard_when_winner = mean(
      hard_F1
    ),
    
    median_hard_when_winner = median(
      hard_F1
    ),
    
    .groups = "drop"
  )

soft_winner_quality <- soft_winners %>%
  group_by(
    method_label
  ) %>%
  summarise(
    mean_soft_when_winner = mean(
      soft_F1
    ),
    
    median_soft_when_winner = median(
      soft_F1
    ),
    
    .groups = "drop"
  )

consolidated_performance_summary <- config_catalog %>%
  left_join(
    hard_winner_ranking %>%
      select(
        method_label,
        n_wins_hard =
          n_wins
      ),
    by = "method_label"
  ) %>%
  left_join(
    soft_winner_ranking %>%
      select(
        method_label,
        n_wins_soft =
          n_wins
      ),
    by = "method_label"
  ) %>%
  left_join(
    positive_score_coverage,
    by = c(
      "method_label",
      "method_family"
    )
  ) %>%
  left_join(
    conditional_quality,
    by = "method_label"
  ) %>%
  left_join(
    hard_winner_quality,
    by = "method_label"
  ) %>%
  left_join(
    soft_winner_quality,
    by = "method_label"
  ) %>%
  mutate(
    n_wins_hard = replace_na(
      n_wins_hard,
      0L
    ),
    
    n_wins_soft = replace_na(
      n_wins_soft,
      0L
    ),
    
    winner_rate_given_positive_hard = round(
      100 *
        n_wins_hard /
        n_positive_hard,
      1
    ),
    
    winner_rate_given_positive_soft = round(
      100 *
        n_wins_soft /
        n_positive_soft,
      1
    ),
    
    winner_count_rank_hard = rank(
      -n_wins_hard,
      ties.method = "min"
    ),
    
    winner_count_rank_soft = rank(
      -n_wins_soft,
      ties.method = "min"
    ),
    
    winner_count_rank_change =
      winner_count_rank_soft -
      winner_count_rank_hard
  ) %>%
  arrange(
    winner_count_rank_hard,
    method_label
  )

if (
  nrow(consolidated_performance_summary) != 20L ||
  anyNA(
    consolidated_performance_summary$
    method_family
  ) ||
  anyNA(
    consolidated_performance_summary$
    n_positive_hard
  ) ||
  anyNA(
    consolidated_performance_summary$
    n_positive_soft
  )
) {
  stop(
    "The consolidated hard- and soft-F1 summary is incomplete.",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("Q6 - CONSOLIDATED HARD- AND SOFT-F1 SUMMARY\n")
cat("=====================================\n")

print(
  consolidated_performance_summary,
  n = Inf,
  width = Inf
)

# =========================================================
# Q7 - BOOTSTRAP INTERVALS FOR CONDITIONAL F1
# =========================================================
# Positively scored series are resampled within configuration.
# Percentile 95% intervals describe median conditional F1.

conditional_f1_bootstrap_seed <- 2025L

if (run_bootstrap) {
  
  set.seed(
    conditional_f1_bootstrap_seed
  )
  
  conditional_hard_intervals <- series_with_cp %>%
    group_by(
      method_label
    ) %>%
    summarise(
      bootstrap_result = list(
        bootstrap_median_ci(
          x = hard_F1,
          n_boot = n_boot,
          alpha = 0.05
        )
      ),
      .groups = "drop"
    ) %>%
    transmute(
      method_label,
      
      conditional_hard_median = map_dbl(
        bootstrap_result,
        ~ .x[["point"]]
      ),
      
      conditional_hard_lower = map_dbl(
        bootstrap_result,
        ~ .x[["lower"]]
      ),
      
      conditional_hard_upper = map_dbl(
        bootstrap_result,
        ~ .x[["upper"]]
      )
    )
  
  conditional_soft_intervals <- series_with_cp %>%
    group_by(
      method_label
    ) %>%
    summarise(
      bootstrap_result = list(
        bootstrap_median_ci(
          x = soft_F1,
          n_boot = n_boot,
          alpha = 0.05
        )
      ),
      .groups = "drop"
    ) %>%
    transmute(
      method_label,
      
      conditional_soft_median = map_dbl(
        bootstrap_result,
        ~ .x[["point"]]
      ),
      
      conditional_soft_lower = map_dbl(
        bootstrap_result,
        ~ .x[["lower"]]
      ),
      
      conditional_soft_upper = map_dbl(
        bootstrap_result,
        ~ .x[["upper"]]
      )
    )
  
  consolidated_performance_table <- consolidated_performance_summary %>%
    left_join(
      conditional_hard_intervals,
      by = "method_label"
    ) %>%
    left_join(
      conditional_soft_intervals,
      by = "method_label"
    ) %>%
    mutate(
      conditional_hard_label = sprintf(
        "%.2f [%.2f, %.2f]",
        conditional_hard_median,
        conditional_hard_lower,
        conditional_hard_upper
      ),
      
      conditional_soft_label = sprintf(
        "%.2f [%.2f, %.2f]",
        conditional_soft_median,
        conditional_soft_lower,
        conditional_soft_upper
      )
    )
  
  if (
    nrow(consolidated_performance_table) != 20L ||
    anyNA(
      consolidated_performance_table$
      conditional_hard_median
    ) ||
    anyNA(
      consolidated_performance_table$
      conditional_soft_median
    )
  ) {
    stop(
      "Conditional-F1 bootstrap intervals are incomplete.",
      call. = FALSE
    )
  }
  
  cat("\n=====================================\n")
  cat("Q7 - BOOTSTRAP INTERVALS FOR CONDITIONAL F1\n")
  cat("=====================================\n")
  cat("Bootstrap replicates:", n_boot, "\n")
  cat("Bootstrap seed:", conditional_f1_bootstrap_seed, "\n")
  
  print(
    consolidated_performance_table %>%
      select(
        method_label,
        method_family,
        winner_count_rank_hard,
        n_positive_hard,
        n_wins_hard,
        conditional_hard_label,
        winner_count_rank_soft,
        n_positive_soft,
        n_wins_soft,
        conditional_soft_label
      ) %>%
      arrange(
        winner_count_rank_hard,
        method_label
      ),
    n = Inf,
    width = Inf
  )
  
} else {
  
  conditional_hard_intervals <- NULL
  conditional_soft_intervals <- NULL
  
  consolidated_performance_table <-
    consolidated_performance_summary
  
  cat("\n=====================================\n")
  cat("Q7 - BOOTSTRAP INTERVALS FOR CONDITIONAL F1\n")
  cat("=====================================\n")
  cat("Skipped: run_bootstrap = FALSE\n")
}

cat("\nResults retained in:\n")
cat("  hard_winners\n")
cat("  soft_winners\n")
cat("  hard_winner_ranking\n")
cat("  soft_winner_ranking\n")
cat("  positive_score_coverage\n")
cat("  conditional_quality\n")
cat("  consolidated_performance_summary\n")
cat("  consolidated_performance_table\n")


# =========================================================
# Q8 - HARD- AND SOFT-F1 WINNER ASSIGNMENTS
# =========================================================
# Winner assignments are compared across matching criteria.
# The diagonal indicates equal numbers of assignments.

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'ggrepel' is required. ",
      "Install it with renv::install('ggrepel')."
    ),
    call. = FALSE
  )
}

winner_assignment_comparison <- consolidated_performance_summary %>%
  select(
    method_label,
    method_family,
    n_wins_hard,
    n_wins_soft
  )

if (
  nrow(winner_assignment_comparison) != 20L ||
  anyNA(
    winner_assignment_comparison$
    method_family
  ) ||
  any(
    winner_assignment_comparison$
    n_wins_hard < 0L
  ) ||
  any(
    winner_assignment_comparison$
    n_wins_soft < 0L
  )
) {
  stop(
    "The hard- and soft-F1 winner comparison is invalid.",
    call. = FALSE
  )
}

winner_assignment_limit <- max(
  c(
    winner_assignment_comparison$
      n_wins_hard,
    
    winner_assignment_comparison$
      n_wins_soft
  ),
  na.rm = TRUE
) * 1.08

p_wins_hard_vs_soft <- winner_assignment_comparison %>%
  ggplot(
    aes(
      x = n_wins_hard,
      y = n_wins_soft,
      color = method_family
    )
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.4
  ) +
  geom_point(
    size = 3,
    alpha = 0.9
  ) +
  ggrepel::geom_text_repel(
    aes(
      label = method_label
    ),
    size = 3.1,
    box.padding = 0.50,
    point.padding = 0.35,
    min.segment.length = 0,
    segment.color = "grey65",
    segment.size = 0.30,
    max.overlaps = Inf,
    force = 2.5,
    force_pull = 0.5,
    max.time = 5,
    seed = 2025,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = wong_palette,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(
      n = 5
    ),
    limits = c(
      0,
      winner_assignment_limit
    ),
    expand = expansion(
      mult = c(
        0.01,
        0.03
      )
    )
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(
      n = 5
    ),
    limits = c(
      0,
      winner_assignment_limit
    ),
    expand = expansion(
      mult = c(
        0.01,
        0.03
      )
    )
  ) +
  coord_fixed(
    ratio = 1,
    clip = "off"
  ) +
  labs(
    x =
      "Winner assignments under hard F1",
    
    y =
      "Winner assignments under soft F1",
    
    color =
      "Family"
  ) +
  theme_pub(
    base_size = 12
  ) +
  theme(
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10.5,
      color = "black"
    ),
    
    legend.title = element_text(
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    legend.position = "right",
    
    plot.margin = margin(
      t = 18,
      r = 50,
      b = 12,
      l = 12
    )
  )

cat("\n=====================================\n")
cat("Q8 - HARD- AND SOFT-F1 WINNER ASSIGNMENTS\n")
cat("=====================================\n")

print(
  winner_assignment_comparison,
  n = Inf,
  width = Inf
)

print(
  p_wins_hard_vs_soft
)

cat("\nResults retained in:\n")
cat("  winner_assignment_comparison\n")
cat("  p_wins_hard_vs_soft\n")

# =========================================================
# Q9 - HARD PRECISION AND RECALL STRUCTURE
# =========================================================
# Under positive hard F1, recall is binary and equals one.
# Hard F1 is therefore monotonic in precision within the
# positively scored observations.

precision_winners_hard <- find_winners(
  df = series_with_cp,
  metric_col = "hard_precision"
)

recall_winners_hard <- find_winners(
  df = series_with_cp,
  metric_col = "hard_recall"
)

hard_f1_winner_keys <- hard_winners %>%
  distinct(
    serie_id,
    method_label
  )

hard_precision_winner_keys <- precision_winners_hard %>%
  distinct(
    serie_id,
    method_label
  )

hard_recall_winner_keys <- recall_winners_hard %>%
  distinct(
    serie_id,
    method_label
  )

positive_hard_keys <- series_with_cp %>%
  filter(
    !is.na(hard_F1),
    hard_F1 > 0
  ) %>%
  distinct(
    serie_id,
    method_label
  )

precision_matches_hard_f1 <- setequal(
  hard_f1_winner_keys,
  hard_precision_winner_keys
)

recall_matches_positive_hard <- setequal(
  hard_recall_winner_keys,
  positive_hard_keys
)

if (
  !precision_matches_hard_f1 ||
  !recall_matches_positive_hard
) {
  stop(
    paste0(
      "The expected hard precision, recall, and F1 ",
      "relationships were not observed."
    ),
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("Q9 - HARD PRECISION AND RECALL STRUCTURE\n")
cat("=====================================\n")

cat(
  "Precision winner set equals hard-F1 winner set:",
  precision_matches_hard_f1,
  "\n"
)

cat(
  "Recall winner set equals positive-hard-F1 set:",
  recall_matches_positive_hard,
  "\n"
)

# =========================================================
# Q10 - DESCRIPTIVE PAIRWISE MEAN F1 DIFFERENCES
# =========================================================
# Mean F1 differences are calculated on series where both
# configurations obtain F1 > 0. These comparisons describe
# direction and magnitude but do not represent significance.

pairwise_positive_score_comparison <- function(
    df,
    config_a,
    config_b,
    metric_col) {
  
  validate_metric(
    df,
    metric_col
  )
  
  paired_data <- df %>%
    filter(
      method_label %in% c(
        config_a,
        config_b
      )
    ) %>%
    select(
      serie_id,
      method_label,
      metric_value =
        all_of(metric_col)
    ) %>%
    pivot_wider(
      names_from =
        method_label,
      values_from =
        metric_value
    )
  
  metric_a <- paired_data[[config_a]]
  metric_b <- paired_data[[config_b]]
  
  if (
    is.null(metric_a) ||
    is.null(metric_b)
  ) {
    stop(
      paste0(
        "Incomplete pairwise data for ",
        config_a,
        " versus ",
        config_b,
        "."
      ),
      call. = FALSE
    )
  }
  
  shared_positive <- tibble(
    metric_a =
      metric_a,
    metric_b =
      metric_b
  ) %>%
    filter(
      !is.na(metric_a),
      !is.na(metric_b),
      metric_a > 0,
      metric_b > 0
    )
  
  n_common <- nrow(
    shared_positive
  )
  
  if (n_common == 0L) {
    return(
      tibble(
        config_a =
          config_a,
        
        config_b =
          config_b,
        
        metric =
          metric_col,
        
        n_common =
          0L,
        
        mean_a =
          NA_real_,
        
        mean_b =
          NA_real_,
        
        mean_difference =
          NA_real_
      )
    )
  }
  
  tibble(
    config_a =
      config_a,
    
    config_b =
      config_b,
    
    metric =
      metric_col,
    
    n_common =
      n_common,
    
    mean_a = mean(
      shared_positive$metric_a
    ),
    
    mean_b = mean(
      shared_positive$metric_b
    ),
    
    mean_difference = mean(
      shared_positive$metric_a -
        shared_positive$metric_b
    )
  )
}

config_levels <- config_catalog %>%
  pull(
    method_label
  )

if (
  length(config_levels) != 20L ||
  anyDuplicated(config_levels) > 0L
) {
  stop(
    "The pairwise analysis requires 20 unique configurations.",
    call. = FALSE
  )
}

ordered_config_pairs <- tidyr::expand_grid(
  config_a =
    config_levels,
  
  config_b =
    config_levels
)

if (nrow(ordered_config_pairs) != 400L) {
  stop(
    "Expected 400 ordered configuration pairs.",
    call. = FALSE
  )
}

pairwise_hard <- ordered_config_pairs %>%
  pmap_dfr(
    function(
    config_a,
    config_b) {
      
      pairwise_positive_score_comparison(
        df = series_with_cp,
        config_a = config_a,
        config_b = config_b,
        metric_col = "hard_F1"
      )
    }
  )

pairwise_soft <- ordered_config_pairs %>%
  pmap_dfr(
    function(
    config_a,
    config_b) {
      
      pairwise_positive_score_comparison(
        df = series_with_cp,
        config_a = config_a,
        config_b = config_b,
        metric_col = "soft_F1"
      )
    }
  )

if (
  nrow(pairwise_hard) != 400L ||
  nrow(pairwise_soft) != 400L
) {
  stop(
    "The pairwise F1 analyses have unexpected dimensions.",
    call. = FALSE
  )
}

pairwise_fill_limit <- max(
  abs(
    c(
      pairwise_hard$
        mean_difference,
      
      pairwise_soft$
        mean_difference
    )
  ),
  na.rm = TRUE
)

pairwise_fill_limit <- max(
  0.1,
  ceiling(
    pairwise_fill_limit *
      10
  ) / 10
)

plot_pairwise_heatmap <- function(
    pairwise_data,
    fill_title) {
  
  pairwise_data %>%
    mutate(
      config_a = factor(
        config_a,
        levels = config_levels
      ),
      
      config_b = factor(
        config_b,
        levels = config_levels
      ),
      
      difference_label = if_else(
        is.na(mean_difference),
        "",
        sprintf(
          "%.2f",
          mean_difference
        )
      )
    ) %>%
    ggplot(
      aes(
        x = config_b,
        y = config_a,
        fill = mean_difference
      )
    ) +
    geom_tile(
      color = "white",
      linewidth = 0.2
    ) +
    geom_text(
      aes(
        label =
          difference_label
      ),
      size = 2.7
    ) +
    scale_x_discrete(
      limits =
        config_levels
    ) +
    scale_y_discrete(
      limits =
        rev(config_levels)
    ) +
    scale_fill_gradient2(
      low = "#D55E00",
      mid = "white",
      high = "#0072B2",
      midpoint = 0,
      limits = c(
        -pairwise_fill_limit,
        pairwise_fill_limit
      ),
      oob = scales::squish,
      na.value = "grey92",
      name = fill_title
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_pub(
      base_size = 11
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 10
      ),
      
      axis.text.y = element_text(
        size = 10
      ),
      
      legend.position = "top",
      
      legend.title = element_text(
        size = 10
      ),
      
      legend.text = element_text(
        size = 9
      ),
      
      legend.key.width = grid::unit(
        2.8,
        "cm"
      ),
      
      legend.key.height = grid::unit(
        0.3,
        "cm"
      ),
      
      legend.margin = margin(
        t = 0,
        r = 0,
        b = -5,
        l = 0
      ),
      
      panel.grid =
        element_blank()
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "left",
        title.vjust = 0.9,
        title.hjust = 0.5
      )
    )
}

p_pairwise_heatmap_hard <- plot_pairwise_heatmap(
  pairwise_data =
    pairwise_hard,
  fill_title =
    "Mean hard-F1 difference"
)

cat("\n=====================================\n")
cat("Q10 - DESCRIPTIVE PAIRWISE MEAN F1 DIFFERENCES\n")
cat("=====================================\n")

print(
  p_pairwise_heatmap_hard
)

cat("\nResults retained in:\n")
cat("  pairwise_hard\n")
cat("  pairwise_soft\n")
cat("  p_pairwise_heatmap_hard\n")

# =========================================================
# Q11 - DESCRIPTIVE PAIRWISE MEAN-ADVANTAGE RANKING
# =========================================================
# The mean-advantage rate is calculated over comparisons with
# available shared-positive-score observations.

build_pairwise_ranking <- function(
    pairwise_data) {
  
  required_columns <- c(
    "config_a",
    "config_b",
    "mean_difference"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(pairwise_data)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Missing pairwise column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  ranking <- pairwise_data %>%
    filter(
      config_a != config_b
    ) %>%
    mutate(
      has_mean_advantage =
        !is.na(mean_difference) &
        mean_difference > 0
    ) %>%
    group_by(
      config_a
    ) %>%
    summarise(
      n_possible_matchups =
        n(),
      
      n_valid_matchups = sum(
        !is.na(mean_difference)
      ),
      
      n_unavailable_matchups =
        n_possible_matchups -
        n_valid_matchups,
      
      n_mean_advantages = sum(
        has_mean_advantage,
        na.rm = TRUE
      ),
      
      mean_advantage_rate = if (
        n_valid_matchups > 0L
      ) {
        round(
          100 *
            n_mean_advantages /
            n_valid_matchups,
          1
        )
      } else {
        NA_real_
      },
      
      average_mean_difference = if (
        n_valid_matchups > 0L
      ) {
        round(
          mean(
            mean_difference,
            na.rm = TRUE
          ),
          3
        )
      } else {
        NA_real_
      },
      
      .groups = "drop"
    ) %>%
    arrange(
      desc(
        mean_advantage_rate
      ),
      desc(
        average_mean_difference
      ),
      config_a
    )
  
  if (
    nrow(ranking) != 20L ||
    any(
      ranking$
      n_possible_matchups != 19L
    )
  ) {
    stop(
      "The descriptive pairwise ranking is incomplete.",
      call. = FALSE
    )
  }
  
  ranking
}

pairwise_ranking_hard <- build_pairwise_ranking(
  pairwise_data =
    pairwise_hard
)

pairwise_ranking_soft <- build_pairwise_ranking(
  pairwise_data =
    pairwise_soft
)

cat("\n=====================================\n")
cat("Q11 - DESCRIPTIVE PAIRWISE MEAN-ADVANTAGE RANKING\n")
cat("=====================================\n")

cat("\n--- Hard F1 ---\n")

print(
  pairwise_ranking_hard,
  n = Inf,
  width = Inf
)

cat("\n--- Soft F1 ---\n")

print(
  pairwise_ranking_soft,
  n = Inf,
  width = Inf
)

cat("\nResults retained in:\n")
cat("  pairwise_ranking_hard\n")
cat("  pairwise_ranking_soft\n")

# =========================================================
# Q12 - FAMILY WINNER SHARE BY REGIME
# =========================================================
# Each series contributes a total weight of one. When winning
# configurations belong to multiple families, the series weight
# is divided equally among those families. Series without a
# positive F1 score are assigned to the None category.

build_family_winner_share <- function(
    df,
    metric_col,
    metric_label) {
  
  validate_metric(
    df,
    metric_col
  )
  
  regime_levels <- c(
    "mean",
    "trend",
    "variance"
  )
  
  family_levels <- c(
    "Joinpoint",
    "MFEF",
    "MFEF+JP",
    "None"
  )
  
  analysis_data <- df %>%
    transmute(
      regime =
        as.character(regime),
      
      serie_id,
      
      method_family =
        as.character(method_family),
      
      metric_value =
        .data[[metric_col]]
    ) %>%
    filter(
      regime %in%
        regime_levels
    ) %>%
    mutate(
      metric_value = replace_na(
        metric_value,
        0
      )
    )
  
  series_summary <- analysis_data %>%
    group_by(
      regime,
      serie_id
    ) %>%
    summarise(
      best_metric = max(
        metric_value
      ),
      .groups = "drop"
    )
  
  family_winners <- analysis_data %>%
    inner_join(
      series_summary,
      by = c(
        "regime",
        "serie_id"
      )
    ) %>%
    filter(
      best_metric > 0,
      metric_value ==
        best_metric
    ) %>%
    distinct(
      regime,
      serie_id,
      method_family
    ) %>%
    group_by(
      regime,
      serie_id
    ) %>%
    mutate(
      series_weight =
        1 / n()
    ) %>%
    ungroup()
  
  no_positive_score <- series_summary %>%
    filter(
      best_metric <= 0
    ) %>%
    transmute(
      regime,
      serie_id,
      method_family =
        "None",
      series_weight =
        1
    )
  
  weighted_assignments <- bind_rows(
    family_winners %>%
      select(
        regime,
        serie_id,
        method_family,
        series_weight
      ),
    no_positive_score
  )
  
  regime_totals <- analysis_data %>%
    distinct(
      regime,
      serie_id
    ) %>%
    count(
      regime,
      name = "n_series"
    )
  
  result <- weighted_assignments %>%
    group_by(
      regime,
      method_family
    ) %>%
    summarise(
      weighted_series = sum(
        series_weight
      ),
      .groups = "drop"
    ) %>%
    complete(
      regime =
        regime_levels,
      
      method_family =
        family_levels,
      
      fill = list(
        weighted_series = 0
      )
    ) %>%
    left_join(
      regime_totals,
      by = "regime"
    ) %>%
    mutate(
      pct =
        100 *
        weighted_series /
        n_series,
      
      metric =
        metric_label,
      
      regime = factor(
        regime,
        levels =
          regime_levels
      ),
      
      method_family = factor(
        method_family,
        levels =
          family_levels
      )
    ) %>%
    arrange(
      regime,
      method_family
    )
  
  if (
    nrow(result) != 12L ||
    anyNA(result$n_series) ||
    anyNA(result$pct)
  ) {
    stop(
      paste0(
        "Invalid family winner shares for ",
        metric_label,
        "."
      ),
      call. = FALSE
    )
  }
  
  result
}

family_winner_share_hard <- build_family_winner_share(
  df = series_with_cp,
  metric_col = "hard_F1",
  metric_label = "Hard F1"
)

family_winner_share_soft <- build_family_winner_share(
  df = series_with_cp,
  metric_col = "soft_F1",
  metric_label = "Soft F1"
)

family_winner_share <- bind_rows(
  family_winner_share_hard,
  family_winner_share_soft
)

family_share_totals <- family_winner_share %>%
  group_by(
    metric,
    regime
  ) %>%
  summarise(
    total_pct = sum(
      pct
    ),
    
    total_weighted_series = sum(
      weighted_series
    ),
    
    n_series = first(
      n_series
    ),
    
    .groups = "drop"
  )

if (
  anyNA(
    family_share_totals$
    total_pct
  ) ||
  any(
    abs(
      family_share_totals$
      total_pct -
      100
    ) > 1e-8
  ) ||
  any(
    abs(
      family_share_totals$
      total_weighted_series -
      family_share_totals$
      n_series
    ) > 1e-8
  )
) {
  stop(
    "Family winner shares do not sum to the regime totals.",
    call. = FALSE
  )
}

family_winner_share_plot_data <- family_winner_share %>%
  mutate(
    pct_label = if_else(
      pct >= 1,
      sprintf(
        "%.2f%%",
        pct
      ),
      ""
    )
  )

p_family_winner_share <- family_winner_share_plot_data %>%
  ggplot(
    aes(
      x = regime,
      y = pct,
      fill = method_family
    )
  ) +
  geom_col(
    width = 0.9
  ) +
  geom_text(
    aes(
      label = pct_label
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 3.2,
    color = "black"
  ) +
  facet_wrap(
    ~ metric
  ) +
  scale_fill_manual(
    values = wong_palette,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      100,
      by = 25
    ),
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    ),
    clip = "off"
  ) +
  labs(
    x = "Regime",
    
    y =
      "Percentage of series (%)",
    
    fill =
      "Family"
  ) +
  theme_pub(
    base_size = 11
  ) +
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 10
    ),
    
    legend.title = element_text(
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

cat("\n=====================================\n")
cat("Q12 - FAMILY WINNER SHARE BY REGIME\n")
cat("=====================================\n")

print(
  family_winner_share %>%
    select(
      metric,
      regime,
      method_family,
      weighted_series,
      n_series,
      pct
    ),
  n = Inf,
  width = Inf
)

print(
  p_family_winner_share
)

cat("\nResults retained in:\n")
cat("  family_winner_share\n")
cat("  family_share_totals\n")
cat("  p_family_winner_share\n")

# =========================================================
# Q13 - TIES AMONG HARD-F1 WINNERS
# =========================================================
# Ties occur when multiple configurations receive the highest
# positive hard F1 on the same series.

tied_winner_series_hard <- hard_winner_counts_by_series %>%
  filter(
    n_winners > 1L
  )

hard_winner_count_distribution <- hard_winner_counts_by_series %>%
  count(
    n_winners,
    name = "n_series"
  ) %>%
  arrange(
    n_winners
  )

# ---------------------------------------------------------
# Validate tie counts
# ---------------------------------------------------------

if (
  nrow(tied_winner_series_hard) !=
  n_tied_series_hard
) {
  stop(
    paste0(
      "The hard-F1 tie count does not match ",
      "the overall winner summary."
    ),
    call. = FALSE
  )
}

if (
  sum(
    hard_winner_count_distribution$
    n_series
  ) !=
  n_positive_hard
) {
  stop(
    paste0(
      "The winner-count distribution does not match ",
      "the number of positively scored series."
    ),
    call. = FALSE
  )
}

if (
  any(
    hard_winner_count_distribution$
    n_winners < 1L
  ) ||
  any(
    hard_winner_count_distribution$
    n_series < 1L
  )
) {
  stop(
    "The hard-F1 winner-count distribution is invalid.",
    call. = FALSE
  )
}

n_unique_winner_series_from_distribution <-
  hard_winner_count_distribution %>%
  filter(
    n_winners == 1L
  ) %>%
  summarise(
    n_series = sum(
      n_series
    )
  ) %>%
  pull(
    n_series
  )

n_tied_series_from_distribution <-
  hard_winner_count_distribution %>%
  filter(
    n_winners > 1L
  ) %>%
  summarise(
    n_series = sum(
      n_series
    )
  ) %>%
  pull(
    n_series
  )

if (
  length(
    n_unique_winner_series_from_distribution
  ) != 1L ||
  length(
    n_tied_series_from_distribution
  ) != 1L ||
  n_unique_winner_series_from_distribution !=
  n_unique_winner_series_hard ||
  n_tied_series_from_distribution !=
  n_tied_series_hard
) {
  stop(
    paste0(
      "Unique- and tied-winner counts do not match ",
      "the overall winner summary."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q13 - TIES AMONG HARD-F1 WINNERS\n")
cat("=====================================\n")

cat(
  "Series with positive hard F1:",
  n_positive_hard,
  "\n"
)

cat(
  "Series with a unique winner:",
  n_unique_winner_series_hard,
  sprintf(
    " (%.1f%% of positively scored series)\n",
    100 *
      n_unique_winner_series_hard /
      n_positive_hard
  )
)

cat(
  "Series with tied winners:",
  n_tied_series_hard,
  sprintf(
    paste0(
      " (%.1f%% of positively scored series; ",
      "%.1f%% of all series with reference change-points)\n"
    ),
    100 *
      n_tied_series_hard /
      n_positive_hard,
    100 *
      n_tied_series_hard /
      n_series_overall
  )
)

cat("\n--- Number of winners per positively scored series ---\n")

print(
  hard_winner_count_distribution,
  n = Inf,
  width = Inf
)

cat("\nResults retained in:\n")
cat("  tied_winner_series_hard\n")
cat("  hard_winner_count_distribution\n")

# =========================================================
# Q14 - SERIES WITHOUT POSITIVE HARD F1
# =========================================================
# These series receive hard F1 = 0 for every configuration and
# therefore do not contribute a hard-F1 winner assignment.

series_without_positive_hard <- series_with_cp %>%
  group_by(
    serie_id
  ) %>%
  summarise(
    has_positive_hard = any(
      !is.na(hard_F1) &
        hard_F1 > 0
    ),
    .groups = "drop"
  ) %>%
  filter(
    !has_positive_hard
  ) %>%
  pull(
    serie_id
  )

# ---------------------------------------------------------
# Extract benchmark characteristics
# ---------------------------------------------------------

features_without_positive_hard <- series_with_cp %>%
  filter(
    serie_id %in%
      series_without_positive_hard
  ) %>%
  transmute(
    serie_id,
    
    regime =
      as.character(regime),
    
    data_type =
      as.character(data_type),
    
    cp_position,
    
    magnitude =
      as.character(magnitude),
    
    ar1 =
      as.character(ar1)
  ) %>%
  distinct(
    serie_id,
    .keep_all = TRUE
  )

if (
  nrow(features_without_positive_hard) !=
  length(series_without_positive_hard)
) {
  stop(
    paste0(
      "Each series without positive hard F1 must have ",
      "one unique set of benchmark characteristics."
    ),
    call. = FALSE
  )
}

if (
  anyNA(
    features_without_positive_hard$
    regime
  ) ||
  anyNA(
    features_without_positive_hard$
    data_type
  ) ||
  anyNA(
    features_without_positive_hard$
    cp_position
  ) ||
  anyNA(
    features_without_positive_hard$
    magnitude
  ) ||
  anyNA(
    features_without_positive_hard$
    ar1
  )
) {
  stop(
    paste0(
      "Missing benchmark characteristics were found among ",
      "series without positive hard F1."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Summarize difficult benchmark scenarios
# ---------------------------------------------------------

summary_without_positive_hard <- features_without_positive_hard %>%
  count(
    regime,
    magnitude,
    ar1,
    name = "n_series"
  ) %>%
  mutate(
    percentage_of_no_positive_hard = round(
      100 *
        n_series /
        length(
          series_without_positive_hard
        ),
      2
    )
  ) %>%
  arrange(
    desc(
      n_series
    ),
    regime,
    magnitude,
    ar1
  )

# ---------------------------------------------------------
# Validate series counts
# ---------------------------------------------------------

if (
  length(
    series_without_positive_hard
  ) !=
  n_no_positive_hard
) {
  stop(
    paste0(
      "The number of series without positive hard F1 ",
      "does not match the overall winner summary."
    ),
    call. = FALSE
  )
}

if (
  length(
    series_without_positive_hard
  ) +
  n_positive_hard !=
  n_series_overall
) {
  stop(
    paste0(
      "Positive and non-positive hard-F1 series do not sum ",
      "to the number of series with reference change-points."
    ),
    call. = FALSE
  )
}

if (
  sum(
    summary_without_positive_hard$
    n_series
  ) !=
  length(
    series_without_positive_hard
  )
) {
  stop(
    paste0(
      "The scenario summary does not match the number of ",
      "series without positive hard F1."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q14 - SERIES WITHOUT POSITIVE HARD F1\n")
cat("=====================================\n")

cat(
  "Series without positive hard F1:",
  length(
    series_without_positive_hard
  ),
  sprintf(
    " (%.1f%% of all series with reference change-points)\n",
    100 *
      length(
        series_without_positive_hard
      ) /
      n_series_overall
  )
)

cat("\n--- Distribution by regime, magnitude, and AR(1) ---\n")

print(
  summary_without_positive_hard,
  n = Inf,
  width = Inf
)

cat("\nResults retained in:\n")
cat("  series_without_positive_hard\n")
cat("  features_without_positive_hard\n")
cat("  summary_without_positive_hard\n")

# =========================================================
# Q15 - HARD-F1 WINNER ASSIGNMENTS BY REGIME
# =========================================================
# Winner assignments are counted separately by structural
# regime. Series without positive hard F1 receive no assignment.

change_regimes <- c(
  "mean",
  "trend",
  "variance"
)

# ---------------------------------------------------------
# Validate regimes and configurations
# ---------------------------------------------------------

regime_catalog <- series_with_cp %>%
  transmute(
    regime =
      as.character(regime)
  ) %>%
  filter(
    regime %in%
      change_regimes
  ) %>%
  distinct(
    regime
  ) %>%
  arrange(
    match(
      regime,
      change_regimes
    )
  )

if (
  nrow(regime_catalog) !=
  length(change_regimes) ||
  !setequal(
    regime_catalog$regime,
    change_regimes
  )
) {
  stop(
    "The expected mean, trend, and variance regimes were not found.",
    call. = FALSE
  )
}

if (
  nrow(config_catalog) != 20L ||
  anyDuplicated(
    config_catalog$method_label
  ) > 0L
) {
  stop(
    "The configuration catalog must contain 20 unique configurations.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Count winner assignments by configuration and regime
# ---------------------------------------------------------

winner_counts_by_regime <- hard_winners %>%
  transmute(
    regime =
      as.character(regime),
    
    method_label
  ) %>%
  filter(
    regime %in%
      change_regimes
  ) %>%
  count(
    regime,
    method_label,
    name = "n_wins"
  )

winner_assignments_by_regime <- tidyr::crossing(
  regime_catalog,
  config_catalog
) %>%
  left_join(
    winner_counts_by_regime,
    by = c(
      "regime",
      "method_label"
    )
  ) %>%
  mutate(
    n_wins = replace_na(
      n_wins,
      0L
    )
  ) %>%
  arrange(
    match(
      regime,
      change_regimes
    ),
    desc(
      n_wins
    ),
    method_label
  )

if (
  nrow(
    winner_assignments_by_regime
  ) != 60L
) {
  stop(
    paste0(
      "Expected 60 regime-configuration combinations, but found ",
      nrow(
        winner_assignments_by_regime
      ),
      "."
    ),
    call. = FALSE
  )
}

if (
  anyNA(
    winner_assignments_by_regime$
    method_family
  ) ||
  any(
    winner_assignments_by_regime$
    n_wins < 0L
  )
) {
  stop(
    "Invalid winner-assignment results were produced.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Summarize positive hard F1 by regime
# ---------------------------------------------------------

positive_hard_summary_by_regime <- series_with_cp %>%
  transmute(
    regime =
      as.character(regime),
    
    serie_id,
    hard_F1
  ) %>%
  filter(
    regime %in%
      change_regimes
  ) %>%
  group_by(
    regime,
    serie_id
  ) %>%
  summarise(
    has_positive_hard = any(
      !is.na(hard_F1) &
        hard_F1 > 0
    ),
    .groups = "drop"
  ) %>%
  group_by(
    regime
  ) %>%
  summarise(
    n_series =
      n(),
    
    n_positive_hard = sum(
      has_positive_hard
    ),
    
    n_without_positive_hard = sum(
      !has_positive_hard
    ),
    
    pct_without_positive_hard = round(
      100 *
        n_without_positive_hard /
        n_series,
      2
    ),
    
    .groups = "drop"
  )

if (
  nrow(
    positive_hard_summary_by_regime
  ) != 3L ||
  any(
    positive_hard_summary_by_regime$
    n_positive_hard +
    positive_hard_summary_by_regime$
    n_without_positive_hard !=
    positive_hard_summary_by_regime$
    n_series
  ) ||
  sum(
    positive_hard_summary_by_regime$
    n_series
  ) !=
  n_series_overall
) {
  stop(
    "The hard-F1 regime summary is internally inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Summarize winner assignments by regime
# ---------------------------------------------------------

winner_totals_by_regime <- hard_winners %>%
  transmute(
    regime =
      as.character(regime)
  ) %>%
  filter(
    regime %in%
      change_regimes
  ) %>%
  count(
    regime,
    name = "n_winner_assignments"
  )

hard_regime_summary <- positive_hard_summary_by_regime %>%
  left_join(
    winner_totals_by_regime,
    by = "regime"
  ) %>%
  mutate(
    n_winner_assignments = replace_na(
      n_winner_assignments,
      0L
    )
  ) %>%
  arrange(
    match(
      regime,
      change_regimes
    )
  )

if (
  nrow(hard_regime_summary) != 3L ||
  anyNA(
    hard_regime_summary$
    n_winner_assignments
  ) ||
  any(
    hard_regime_summary$
    n_winner_assignments <
    hard_regime_summary$
    n_positive_hard
  ) ||
  sum(
    hard_regime_summary$
    n_winner_assignments
  ) !=
  n_winner_assignments_hard
) {
  stop(
    "The winner-assignment summary is incomplete or inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Validate winner-assignment totals
# ---------------------------------------------------------

winner_total_validation <- winner_assignments_by_regime %>%
  group_by(
    regime
  ) %>%
  summarise(
    n_wins_from_configurations = sum(
      n_wins
    ),
    .groups = "drop"
  ) %>%
  left_join(
    hard_regime_summary %>%
      select(
        regime,
        n_winner_assignments
      ),
    by = "regime"
  )

if (
  anyNA(
    winner_total_validation$
    n_winner_assignments
  ) ||
  any(
    winner_total_validation$
    n_wins_from_configurations !=
    winner_total_validation$
    n_winner_assignments
  )
) {
  stop(
    paste0(
      "Configuration-level winner assignments do not match ",
      "the regime-level totals."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q15 - HARD-F1 WINNER ASSIGNMENTS BY REGIME\n")
cat("=====================================\n")

cat("\n--- Regime summary ---\n")

print(
  hard_regime_summary,
  n = Inf,
  width = Inf
)

cat("\n--- Winner assignments by configuration and regime ---\n")

print(
  winner_assignments_by_regime,
  n = Inf,
  width = Inf
)

cat("\nResults retained in:\n")
cat("  hard_regime_summary\n")
cat("  winner_assignments_by_regime\n")


# =========================================================
# Q16 - DESCRIPTIVE PAIRWISE COMPARISON BY REGIME
# =========================================================
# Pairwise mean F1 differences are calculated on series where
# both configurations obtain F1 > 0. Comparisons without shared
# positive-score series are excluded from the denominator.

build_pairwise_ranking_by_regime <- function(
    df,
    regime_name,
    metric_col) {
  
  validate_metric(
    df,
    metric_col
  )
  
  regime_data <- df %>%
    filter(
      as.character(regime) ==
        regime_name
    )
  
  if (nrow(regime_data) == 0L) {
    stop(
      paste0(
        "No observations were found for regime ",
        regime_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  regime_configurations <- regime_data %>%
    distinct(
      method_label
    ) %>%
    pull(
      method_label
    )
  
  if (
    length(regime_configurations) != 20L ||
    !setequal(
      regime_configurations,
      config_catalog$method_label
    )
  ) {
    stop(
      paste0(
        "The configuration set is incomplete for regime ",
        regime_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  ordered_pairs <- tidyr::expand_grid(
    config_a =
      config_levels,
    
    config_b =
      config_levels
  ) %>%
    filter(
      config_a != config_b
    )
  
  if (nrow(ordered_pairs) != 380L) {
    stop(
      paste0(
        "Expected 380 directed configuration pairs for regime ",
        regime_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  pairwise_results <- ordered_pairs %>%
    pmap_dfr(
      function(
    config_a,
    config_b) {
        
        pairwise_positive_score_comparison(
          df = regime_data,
          config_a = config_a,
          config_b = config_b,
          metric_col = metric_col
        )
      }
    )
  
  if (nrow(pairwise_results) != 380L) {
    stop(
      paste0(
        "The pairwise comparison returned an unexpected number ",
        "of results for regime ",
        regime_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  ranking <- pairwise_results %>%
    filter(
      config_a != config_b
    ) %>%
    mutate(
      has_mean_advantage =
        !is.na(mean_difference) &
        mean_difference > 0
    ) %>%
    group_by(
      config_a
    ) %>%
    summarise(
      regime =
        regime_name,
      
      metric =
        metric_col,
      
      n_possible_matchups =
        n(),
      
      n_valid_matchups = sum(
        !is.na(mean_difference)
      ),
      
      n_mean_advantages = sum(
        has_mean_advantage,
        na.rm = TRUE
      ),
      
      average_mean_difference = if (
        any(
          !is.na(mean_difference)
        )
      ) {
        round(
          mean(
            mean_difference,
            na.rm = TRUE
          ),
          3
        )
      } else {
        NA_real_
      },
      
      .groups = "drop"
    ) %>%
    mutate(
      n_unavailable_matchups =
        n_possible_matchups -
        n_valid_matchups,
      
      mean_advantage_rate = if_else(
        n_valid_matchups > 0L,
        round(
          100 *
            n_mean_advantages /
            n_valid_matchups,
          1
        ),
        NA_real_
      )
    ) %>%
    left_join(
      config_catalog %>%
        rename(
          config_a =
            method_label
        ),
      by = "config_a"
    ) %>%
    select(
      regime,
      metric,
      config_a,
      method_family,
      n_possible_matchups,
      n_valid_matchups,
      n_unavailable_matchups,
      n_mean_advantages,
      mean_advantage_rate,
      average_mean_difference
    ) %>%
    arrange(
      desc(
        mean_advantage_rate
      ),
      desc(
        average_mean_difference
      ),
      config_a
    )
  
  if (
    nrow(ranking) != 20L ||
    any(
      ranking$n_possible_matchups != 19L
    ) ||
    anyNA(
      ranking$method_family
    )
  ) {
    stop(
      paste0(
        "The regime-level ranking is incomplete for ",
        regime_name,
        " under ",
        metric_col,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      ranking$n_valid_matchups +
      ranking$n_unavailable_matchups !=
      ranking$n_possible_matchups
    ) ||
    any(
      ranking$n_mean_advantages >
      ranking$n_valid_matchups
    )
  ) {
    stop(
      paste0(
        "The regime-level matchup counts are inconsistent for ",
        regime_name,
        " under ",
        metric_col,
        "."
      ),
      call. = FALSE
    )
  }
  
  list(
    pairwise_results =
      pairwise_results,
    
    ranking =
      ranking
  )
}

# ---------------------------------------------------------
# Run hard- and soft-F1 comparisons by regime
# ---------------------------------------------------------

pairwise_objects_by_regime_hard <- purrr::map(
  change_regimes,
  ~ build_pairwise_ranking_by_regime(
    df = series_with_cp,
    regime_name = .x,
    metric_col = "hard_F1"
  )
)

names(pairwise_objects_by_regime_hard) <-
  change_regimes

pairwise_objects_by_regime_soft <- purrr::map(
  change_regimes,
  ~ build_pairwise_ranking_by_regime(
    df = series_with_cp,
    regime_name = .x,
    metric_col = "soft_F1"
  )
)

names(pairwise_objects_by_regime_soft) <-
  change_regimes

# ---------------------------------------------------------
# Consolidate rankings and pairwise results
# ---------------------------------------------------------

pairwise_ranking_by_regime_hard <- purrr::map_dfr(
  pairwise_objects_by_regime_hard,
  "ranking"
)

pairwise_ranking_by_regime_soft <- purrr::map_dfr(
  pairwise_objects_by_regime_soft,
  "ranking"
)

pairwise_details_by_regime_hard <- purrr::imap_dfr(
  pairwise_objects_by_regime_hard,
  function(result_object, regime_name) {
    result_object$pairwise_results %>%
      mutate(
        regime =
          regime_name,
        .before = 1
      )
  }
)

pairwise_details_by_regime_soft <- purrr::imap_dfr(
  pairwise_objects_by_regime_soft,
  function(result_object, regime_name) {
    result_object$pairwise_results %>%
      mutate(
        regime =
          regime_name,
        .before = 1
      )
  }
)

if (
  nrow(pairwise_ranking_by_regime_hard) != 60L ||
  nrow(pairwise_ranking_by_regime_soft) != 60L ||
  nrow(pairwise_details_by_regime_hard) != 1140L ||
  nrow(pairwise_details_by_regime_soft) != 1140L
) {
  stop(
    "The regime-level pairwise results have unexpected dimensions.",
    call. = FALSE
  )
}

if (
  any(
    pairwise_ranking_by_regime_hard$
    n_possible_matchups != 19L
  ) ||
  any(
    pairwise_ranking_by_regime_soft$
    n_possible_matchups != 19L
  )
) {
  stop(
    "Each configuration must have 19 possible regime-level matchups.",
    call. = FALSE
  )
}

if (
  !setequal(
    unique(
      pairwise_ranking_by_regime_hard$
      regime
    ),
    change_regimes
  ) ||
  !setequal(
    unique(
      pairwise_ranking_by_regime_soft$
      regime
    ),
    change_regimes
  )
) {
  stop(
    "The regime-level pairwise results do not contain all regimes.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q16 - DESCRIPTIVE PAIRWISE COMPARISON BY REGIME\n")
cat("=====================================\n")

cat("\n--- Hard F1 ---\n")

print(
  pairwise_ranking_by_regime_hard,
  n = Inf,
  width = Inf
)

cat("\n--- Soft F1 ---\n")

print(
  pairwise_ranking_by_regime_soft,
  n = Inf,
  width = Inf
)

cat("\nResults retained in:\n")
cat("  pairwise_ranking_by_regime_hard\n")
cat("  pairwise_ranking_by_regime_soft\n")
cat("  pairwise_details_by_regime_hard\n")
cat("  pairwise_details_by_regime_soft\n")

# =========================================================
# Q17 - FAMILY-LEVEL DESCRIPTIVE SUMMARY
# =========================================================
# Family-level results are descriptive because the method
# families contain different numbers of configurations.
# Configuration-level analyses remain the basis for inference.

expected_family_sizes <- tibble(
  method_family = c(
    "Joinpoint",
    "MFEF",
    "MFEF+JP"
  ),
  
  expected_configurations = c(
    2L,
    6L,
    12L
  )
)

# ---------------------------------------------------------
# Conditional positive-F1 summary
# ---------------------------------------------------------

family_conditional_f1_summary <- series_with_cp %>%
  group_by(
    method_family
  ) %>%
  summarise(
    n_positive_observations_hard = sum(
      !is.na(hard_F1) &
        hard_F1 > 0
    ),
    
    median_hard_positive = if (
      any(
        !is.na(hard_F1) &
        hard_F1 > 0
      )
    ) {
      median(
        hard_F1[
          !is.na(hard_F1) &
            hard_F1 > 0
        ]
      )
    } else {
      NA_real_
    },
    
    n_positive_observations_soft = sum(
      !is.na(soft_F1) &
        soft_F1 > 0
    ),
    
    median_soft_positive = if (
      any(
        !is.na(soft_F1) &
        soft_F1 > 0
      )
    ) {
      median(
        soft_F1[
          !is.na(soft_F1) &
            soft_F1 > 0
        ]
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

# ---------------------------------------------------------
# Positive-score coverage summary
# ---------------------------------------------------------

family_coverage_summary <- positive_score_coverage %>%
  group_by(
    method_family
  ) %>%
  summarise(
    n_configurations =
      n_distinct(method_label),
    
    minimum_positive_hard = min(
      n_positive_hard
    ),
    
    median_positive_hard = median(
      n_positive_hard
    ),
    
    maximum_positive_hard = max(
      n_positive_hard
    ),
    
    minimum_positive_soft = min(
      n_positive_soft
    ),
    
    median_positive_soft = median(
      n_positive_soft
    ),
    
    maximum_positive_soft = max(
      n_positive_soft
    ),
    
    .groups = "drop"
  )

# ---------------------------------------------------------
# Descriptive pairwise summary
# ---------------------------------------------------------

family_pairwise_summary <- pairwise_ranking_hard %>%
  rename(
    method_label =
      config_a
  ) %>%
  left_join(
    config_catalog,
    by = "method_label"
  ) %>%
  group_by(
    method_family
  ) %>%
  summarise(
    n_configurations =
      n_distinct(method_label),
    
    n_available_mean_advantage_rates = sum(
      !is.na(mean_advantage_rate)
    ),
    
    minimum_mean_advantage_rate = if (
      any(
        !is.na(mean_advantage_rate)
      )
    ) {
      min(
        mean_advantage_rate,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    median_mean_advantage_rate = if (
      any(
        !is.na(mean_advantage_rate)
      )
    ) {
      median(
        mean_advantage_rate,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    maximum_mean_advantage_rate = if (
      any(
        !is.na(mean_advantage_rate)
      )
    ) {
      max(
        mean_advantage_rate,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    minimum_average_mean_difference = if (
      any(
        !is.na(average_mean_difference)
      )
    ) {
      min(
        average_mean_difference,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    median_average_mean_difference = if (
      any(
        !is.na(average_mean_difference)
      )
    ) {
      median(
        average_mean_difference,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    maximum_average_mean_difference = if (
      any(
        !is.na(average_mean_difference)
      )
    ) {
      max(
        average_mean_difference,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

# ---------------------------------------------------------
# Consolidated family summary
# ---------------------------------------------------------

family_descriptive_summary <- family_conditional_f1_summary %>%
  left_join(
    family_coverage_summary,
    by = "method_family"
  ) %>%
  arrange(
    method_family
  )

family_pairwise_summary <- family_pairwise_summary %>%
  arrange(
    method_family
  )

# ---------------------------------------------------------
# Validate family-level results
# ---------------------------------------------------------

if (
  nrow(family_descriptive_summary) != 3L ||
  nrow(family_pairwise_summary) != 3L
) {
  stop(
    "Expected descriptive results for three method families.",
    call. = FALSE
  )
}

if (
  anyNA(
    family_descriptive_summary$
    n_configurations
  ) ||
  anyNA(
    family_pairwise_summary$
    n_configurations
  )
) {
  stop(
    "Missing family-level configuration counts were found.",
    call. = FALSE
  )
}

family_size_validation <- family_coverage_summary %>%
  mutate(
    method_family =
      as.character(method_family)
  ) %>%
  left_join(
    expected_family_sizes,
    by = "method_family"
  )

if (
  nrow(family_size_validation) != 3L ||
  anyNA(
    family_size_validation$
    expected_configurations
  ) ||
  any(
    family_size_validation$
    n_configurations !=
    family_size_validation$
    expected_configurations
  )
) {
  stop(
    paste0(
      "The method families must contain 2 Joinpoint, ",
      "6 MFEF, and 12 MFEF+JP configurations."
    ),
    call. = FALSE
  )
}

if (
  !setequal(
    as.character(
      family_descriptive_summary$
      method_family
    ),
    expected_family_sizes$
    method_family
  ) ||
  !setequal(
    as.character(
      family_pairwise_summary$
      method_family
    ),
    expected_family_sizes$
    method_family
  )
) {
  stop(
    "The family summaries do not contain all expected families.",
    call. = FALSE
  )
}

if (
  any(
    family_conditional_f1_summary$
    n_positive_observations_hard < 0L
  ) ||
  any(
    family_conditional_f1_summary$
    n_positive_observations_soft < 0L
  ) ||
  any(
    family_coverage_summary$
    minimum_positive_hard >
    family_coverage_summary$
    median_positive_hard
  ) ||
  any(
    family_coverage_summary$
    median_positive_hard >
    family_coverage_summary$
    maximum_positive_hard
  ) ||
  any(
    family_coverage_summary$
    minimum_positive_soft >
    family_coverage_summary$
    median_positive_soft
  ) ||
  any(
    family_coverage_summary$
    median_positive_soft >
    family_coverage_summary$
    maximum_positive_soft
  )
) {
  stop(
    "The family-level descriptive summaries are internally inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q17 - FAMILY-LEVEL DESCRIPTIVE SUMMARY\n")
cat("=====================================\n")

cat(
  "\n--- Conditional positive F1 and coverage by family ---\n"
)

print(
  family_descriptive_summary,
  n = Inf,
  width = Inf
)

cat("\n--- Descriptive pairwise summary by family ---\n")

print(
  family_pairwise_summary,
  n = Inf,
  width = Inf
)

cat("\nResults retained in:\n")
cat("  family_descriptive_summary\n")
cat("  family_pairwise_summary\n")

# =========================================================
# Q18 - CONDITIONAL F1 DISTRIBUTIONS BY FAMILY AND REGIME
# =========================================================
# Positive F1 values are summarized at the
# configuration-series level. Family-level distributions are
# descriptive because families contain different numbers of
# configurations and observations are repeated across series.

regime_levels <- c(
  "mean",
  "trend",
  "variance"
)

family_levels <- c(
  "Joinpoint",
  "MFEF",
  "MFEF+JP"
)

metric_levels <- c(
  "Hard F1",
  "Soft F1"
)

# ---------------------------------------------------------
# Prepare configuration-series F1 observations
# ---------------------------------------------------------

family_regime_f1_data <- series_with_cp %>%
  transmute(
    regime =
      as.character(regime),
    
    serie_id,
    method_label,
    
    method_family =
      as.character(method_family),
    
    `Hard F1` =
      hard_F1,
    
    `Soft F1` =
      soft_F1
  ) %>%
  filter(
    regime %in%
      regime_levels
  ) %>%
  pivot_longer(
    cols = all_of(
      metric_levels
    ),
    names_to = "metric",
    values_to = "F1"
  ) %>%
  mutate(
    regime = factor(
      regime,
      levels =
        regime_levels
    ),
    
    method_family = factor(
      method_family,
      levels =
        family_levels
    ),
    
    metric = factor(
      metric,
      levels =
        metric_levels
    )
  )

if (
  anyNA(
    family_regime_f1_data$regime
  ) ||
  anyNA(
    family_regime_f1_data$method_family
  ) ||
  anyNA(
    family_regime_f1_data$metric
  )
) {
  stop(
    "Invalid regime, family, or metric values were found.",
    call. = FALSE
  )
}

if (
  any(
    family_regime_f1_data$F1 < 0 |
    family_regime_f1_data$F1 > 1,
    na.rm = TRUE
  )
) {
  stop(
    "F1 values must remain within [0, 1].",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Determine regime and family denominators
# ---------------------------------------------------------

series_per_regime <- family_regime_f1_data %>%
  transmute(
    regime =
      as.character(regime),
    
    serie_id
  ) %>%
  distinct(
    regime,
    serie_id
  ) %>%
  count(
    regime,
    name = "n_series_total"
  )

configurations_per_family <- config_catalog %>%
  transmute(
    method_family =
      as.character(method_family),
    
    method_label
  ) %>%
  distinct(
    method_family,
    method_label
  ) %>%
  count(
    method_family,
    name = "n_configs"
  )

if (
  nrow(series_per_regime) !=
  length(regime_levels) ||
  nrow(configurations_per_family) !=
  length(family_levels)
) {
  stop(
    "Could not determine all regime and family denominators.",
    call. = FALSE
  )
}

if (
  !setequal(
    series_per_regime$regime,
    regime_levels
  ) ||
  !setequal(
    configurations_per_family$method_family,
    family_levels
  )
) {
  stop(
    "The denominators do not match the expected regimes and families.",
    call. = FALSE
  )
}

if (
  sum(
    series_per_regime$n_series_total
  ) !=
  n_series_overall
) {
  stop(
    "The regime-specific series counts do not match the analytical total.",
    call. = FALSE
  )
}

expected_configuration_counts <- tibble(
  method_family = c(
    "Joinpoint",
    "MFEF",
    "MFEF+JP"
  ),
  
  expected_n_configs = c(
    2L,
    6L,
    12L
  )
)

configuration_count_validation <- configurations_per_family %>%
  left_join(
    expected_configuration_counts,
    by = "method_family"
  )

if (
  anyNA(
    configuration_count_validation$
    expected_n_configs
  ) ||
  any(
    configuration_count_validation$
    n_configs !=
    configuration_count_validation$
    expected_n_configs
  )
) {
  stop(
    paste0(
      "The families must contain 2 Joinpoint, 6 MFEF, ",
      "and 12 MFEF+JP configurations."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Summarize positive configuration-series observations
# ---------------------------------------------------------

family_regime_positive_summary <- family_regime_f1_data %>%
  filter(
    !is.na(F1),
    F1 > 0
  ) %>%
  count(
    regime,
    method_family,
    metric,
    name =
      "n_positive_observations"
  ) %>%
  mutate(
    regime =
      as.character(regime),
    
    method_family =
      as.character(method_family),
    
    metric =
      as.character(metric)
  ) %>%
  complete(
    regime =
      regime_levels,
    
    method_family =
      family_levels,
    
    metric =
      metric_levels,
    
    fill = list(
      n_positive_observations = 0L
    )
  ) %>%
  left_join(
    series_per_regime,
    by = "regime"
  ) %>%
  left_join(
    configurations_per_family,
    by = "method_family"
  ) %>%
  mutate(
    n_possible_observations =
      n_series_total *
      n_configs,
    
    positive_observation_rate =
      100 *
      n_positive_observations /
      n_possible_observations,
    
    label = sprintf(
      "n=%d\n(%.1f%%)",
      n_positive_observations,
      positive_observation_rate
    ),
    
    regime = factor(
      regime,
      levels =
        regime_levels
    ),
    
    method_family = factor(
      method_family,
      levels =
        family_levels
    ),
    
    metric = factor(
      metric,
      levels =
        metric_levels
    )
  ) %>%
  arrange(
    metric,
    regime,
    method_family
  )

if (
  nrow(
    family_regime_positive_summary
  ) != 18L
) {
  stop(
    paste0(
      "Expected 18 regime-family-metric combinations, but found ",
      nrow(
        family_regime_positive_summary
      ),
      "."
    ),
    call. = FALSE
  )
}

if (
  anyNA(
    family_regime_positive_summary$
    n_series_total
  ) ||
  anyNA(
    family_regime_positive_summary$
    n_configs
  ) ||
  anyNA(
    family_regime_positive_summary$
    positive_observation_rate
  ) ||
  any(
    family_regime_positive_summary$
    n_possible_observations <= 0L
  ) ||
  any(
    family_regime_positive_summary$
    n_positive_observations >
    family_regime_positive_summary$
    n_possible_observations
  ) ||
  any(
    family_regime_positive_summary$
    positive_observation_rate < 0 |
    family_regime_positive_summary$
    positive_observation_rate > 100
  )
) {
  stop(
    "Invalid family-regime denominators or rates were produced.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Conditional F1 figure
# ---------------------------------------------------------

p_boxplot_family_regime <- family_regime_f1_data %>%
  filter(
    !is.na(F1),
    F1 > 0
  ) %>%
  ggplot(
    aes(
      x = method_family,
      y = F1,
      fill = method_family
    )
  ) +
  geom_boxplot(
    outlier.size = 0.6,
    outlier.alpha = 0.7,
    alpha = 0.75,
    width = 0.65
  ) +
  geom_text(
    data =
      family_regime_positive_summary,
    
    aes(
      x = method_family,
      y = -0.10,
      label = label
    ),
    
    inherit.aes = FALSE,
    size = 3.1,
    lineheight = 0.85
  ) +
  facet_grid(
    metric ~ regime
  ) +
  scale_fill_manual(
    values = wong_palette,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    
    expand = expansion(
      mult = c(
        0,
        0.03
      )
    )
  ) +
  coord_cartesian(
    ylim = c(
      -0.18,
      1.05
    ),
    clip = "off"
  ) +
  labs(
    x = "Family",
    
    y =
      "F1 among positively scored observations",
    
    fill = "Family"
  ) +
  theme_pub(
    base_size = 11
  ) +
  theme(
    legend.position = "none",
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    strip.text = element_text(
      size = 11,
      face = "bold"
    ),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 15,
      l = 10
    )
  )

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q18 - CONDITIONAL F1 DISTRIBUTIONS BY FAMILY AND REGIME\n")
cat("=====================================\n")

cat("\n--- Positive configuration-series observations ---\n")

print(
  family_regime_positive_summary,
  n = Inf,
  width = Inf
)

print(
  p_boxplot_family_regime
)

cat("\nResults retained in:\n")
cat("  family_regime_f1_data\n")
cat("  family_regime_positive_summary\n")
cat("  p_boxplot_family_regime\n")

# =========================================================
# Q19 - PARAMETER-MATCHED MFEF VS MFEF+JP CONTRASTS
# =========================================================
# MFEF is compared with the corresponding MFEF+JP(F) and
# MFEF+JP(T) configurations using identical k and rho values.
# Paired differences are defined as F1(MFEF+JP) - F1(MFEF).
# Positive differences favor the incorporation of Joinpoint.

metric_catalog <- tibble(
  metric_col = c(
    "hard_F1",
    "soft_F1"
  ),
  
  metric_label = c(
    "Hard F1",
    "Soft F1"
  )
)

jp_variant_catalog <- tibble(
  jp_method = c(
    "MFEF_JP_FALSE",
    "MFEF_JP_TRUE"
  ),
  
  jp_variant = c(
    "F",
    "T"
  )
)

# ---------------------------------------------------------
# Prepare one parameter-matched contrast
# ---------------------------------------------------------

prepare_parameter_matched_contrast <- function(
    df,
    regime_name,
    metric_col,
    time_tolerance_value,
    threshold_value,
    jp_method_value,
    jp_variant_value) {
  
  validate_metric(
    df,
    metric_col
  )
  
  mfef_data <- df %>%
    filter(
      as.character(regime) ==
        regime_name,
      method == "MFEF",
      time_tolerance ==
        time_tolerance_value,
      threshold ==
        threshold_value
    ) %>%
    transmute(
      serie_id,
      f1_mfef =
        .data[[metric_col]]
    )
  
  mfef_jp_data <- df %>%
    filter(
      as.character(regime) ==
        regime_name,
      method ==
        jp_method_value,
      time_tolerance ==
        time_tolerance_value,
      threshold ==
        threshold_value
    ) %>%
    transmute(
      serie_id,
      f1_mfef_jp =
        .data[[metric_col]]
    )
  
  if (
    anyDuplicated(
      mfef_data$serie_id
    ) > 0L ||
    anyDuplicated(
      mfef_jp_data$serie_id
    ) > 0L
  ) {
    stop(
      paste0(
        "Duplicated series were found in the parameter-matched ",
        "contrast for regime ",
        regime_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  paired_data <- mfef_data %>%
    inner_join(
      mfef_jp_data,
      by = "serie_id"
    ) %>%
    mutate(
      paired_difference =
        f1_mfef_jp -
        f1_mfef
    )
  
  expected_series <- df %>%
    filter(
      as.character(regime) ==
        regime_name
    ) %>%
    summarise(
      n_series =
        n_distinct(serie_id)
    ) %>%
    pull(
      n_series
    )
  
  if (
    nrow(paired_data) !=
    expected_series
  ) {
    stop(
      paste0(
        "Incomplete parameter-matched contrast for regime=",
        regime_name,
        ", metric=",
        metric_col,
        ", k=",
        time_tolerance_value,
        ", rho=",
        threshold_value,
        ", JP variant=",
        jp_variant_value,
        ". Expected ",
        expected_series,
        " paired series but found ",
        nrow(paired_data),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    anyNA(
      paired_data$f1_mfef
    ) ||
    anyNA(
      paired_data$f1_mfef_jp
    )
  ) {
    stop(
      paste0(
        "Missing standardized F1 values for regime=",
        regime_name,
        ", metric=",
        metric_col,
        ", k=",
        time_tolerance_value,
        ", rho=",
        threshold_value,
        ", JP variant=",
        jp_variant_value,
        "."
      ),
      call. = FALSE
    )
  }
  
  paired_data
}

# ---------------------------------------------------------
# Run one paired Wilcoxon test
# ---------------------------------------------------------

run_parameter_matched_wilcoxon <- function(
    df,
    regime_name,
    metric_col,
    metric_label,
    time_tolerance_value,
    threshold_value,
    jp_method_value,
    jp_variant_value) {
  
  paired_data <- prepare_parameter_matched_contrast(
    df = df,
    regime_name = regime_name,
    metric_col = metric_col,
    time_tolerance_value =
      time_tolerance_value,
    threshold_value =
      threshold_value,
    jp_method_value =
      jp_method_value,
    jp_variant_value =
      jp_variant_value
  )
  
  paired_differences <-
    paired_data$paired_difference
  
  n_pairs <- nrow(
    paired_data
  )
  
  n_improved <- sum(
    paired_differences > 0
  )
  
  n_tied <- sum(
    paired_differences == 0
  )
  
  n_worsened <- sum(
    paired_differences < 0
  )
  
  n_nonzero_differences <- sum(
    paired_differences != 0
  )
  
  if (
    n_improved +
    n_tied +
    n_worsened !=
    n_pairs
  ) {
    stop(
      "Direction counts do not sum to the paired sample size.",
      call. = FALSE
    )
  }
  
  if (n_nonzero_differences > 0L) {
    
    wilcoxon_result <- suppressWarnings(
      wilcox.test(
        paired_data$f1_mfef_jp,
        paired_data$f1_mfef,
        paired = TRUE,
        alternative = "two.sided",
        exact = FALSE,
        correct = TRUE
      )
    )
    
    wilcoxon_statistic <- unname(
      wilcoxon_result$statistic
    )
    
    p_value <-
      wilcoxon_result$p.value
    
  } else {
    
    wilcoxon_statistic <- 0
    p_value <- 1
  }
  
  tibble(
    regime =
      regime_name,
    
    metric =
      metric_label,
    
    metric_col =
      metric_col,
    
    time_tolerance =
      time_tolerance_value,
    
    threshold =
      threshold_value,
    
    jp_variant =
      jp_variant_value,
    
    mfef_configuration = paste0(
      "MFEF(",
      time_tolerance_value,
      ",",
      threshold_value,
      ")"
    ),
    
    mfef_jp_configuration = paste0(
      "MFEF+JP(",
      jp_variant_value,
      ",",
      time_tolerance_value,
      ",",
      threshold_value,
      ")"
    ),
    
    n_pairs =
      n_pairs,
    
    n_improved =
      n_improved,
    
    n_tied =
      n_tied,
    
    n_worsened =
      n_worsened,
    
    n_nonzero_differences =
      n_nonzero_differences,
    
    improvement_rate = round(
      100 *
        n_improved /
        n_pairs,
      1
    ),
    
    tie_rate = round(
      100 *
        n_tied /
        n_pairs,
      1
    ),
    
    worsening_rate = round(
      100 *
        n_worsened /
        n_pairs,
      1
    ),
    
    median_f1_mfef = median(
      paired_data$f1_mfef
    ),
    
    median_f1_mfef_jp = median(
      paired_data$f1_mfef_jp
    ),
    
    mean_paired_difference = mean(
      paired_differences
    ),
    
    median_paired_difference = median(
      paired_differences
    ),
    
    wilcoxon_statistic =
      wilcoxon_statistic,
    
    p_value =
      p_value
  )
}

# ---------------------------------------------------------
# Build the parameter-matched contrast grid
# ---------------------------------------------------------

mfef_time_tolerances <- series_with_cp %>%
  filter(
    method == "MFEF"
  ) %>%
  distinct(
    time_tolerance
  ) %>%
  arrange(
    time_tolerance
  ) %>%
  pull(
    time_tolerance
  )

mfef_thresholds <- series_with_cp %>%
  filter(
    method == "MFEF"
  ) %>%
  distinct(
    threshold
  ) %>%
  arrange(
    threshold
  ) %>%
  pull(
    threshold
  )

if (
  !isTRUE(
    all.equal(
      as.numeric(
        mfef_time_tolerances
      ),
      c(
        3,
        6,
        12
      )
    )
  )
) {
  stop(
    "Expected MFEF time tolerances 3, 6, and 12.",
    call. = FALSE
  )
}

if (
  !isTRUE(
    all.equal(
      as.numeric(
        mfef_thresholds
      ),
      c(
        0.90,
        0.95
      )
    )
  )
) {
  stop(
    "Expected MFEF thresholds 0.90 and 0.95.",
    call. = FALSE
  )
}

contrast_grid <- tidyr::crossing(
  regime =
    regime_levels,
  
  metric_catalog,
  
  time_tolerance =
    mfef_time_tolerances,
  
  threshold =
    mfef_thresholds,
  
  jp_variant_catalog
)

if (nrow(contrast_grid) != 72L) {
  stop(
    paste0(
      "Expected 72 parameter-matched contrasts, but found ",
      nrow(contrast_grid),
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Run all parameter-matched contrasts
# ---------------------------------------------------------

parameter_matched_tests <- contrast_grid %>%
  pmap_dfr(
    function(
    regime,
    metric_col,
    metric_label,
    time_tolerance,
    threshold,
    jp_method,
    jp_variant) {
      
      run_parameter_matched_wilcoxon(
        df = series_with_cp,
        regime_name = regime,
        metric_col = metric_col,
        metric_label = metric_label,
        time_tolerance_value =
          time_tolerance,
        threshold_value =
          threshold,
        jp_method_value =
          jp_method,
        jp_variant_value =
          jp_variant
      )
    }
  ) %>%
  mutate(
    p_bonferroni = p.adjust(
      p_value,
      method = "bonferroni",
      n = 72
    ),
    
    p_holm = p.adjust(
      p_value,
      method = "holm",
      n = 72
    ),
    
    significant_bonferroni_005 =
      p_bonferroni < 0.05,
    
    significant_bonferroni_001 =
      p_bonferroni < 0.01,
    
    significant_holm_005 =
      p_holm < 0.05,
    
    significant_mfef_jp_higher =
      significant_bonferroni_005 &
      mean_paired_difference > 0,
    
    significant_mfef_higher =
      significant_bonferroni_005 &
      mean_paired_difference < 0
  ) %>%
  arrange(
    metric,
    regime,
    time_tolerance,
    threshold,
    jp_variant
  )

if (nrow(parameter_matched_tests) != 72L) {
  stop(
    "The analysis did not return all 72 expected contrasts.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Validate paired sample sizes
# ---------------------------------------------------------

expected_pairs_by_regime <- series_with_cp %>%
  transmute(
    regime =
      as.character(regime),
    
    serie_id
  ) %>%
  filter(
    regime %in%
      regime_levels
  ) %>%
  distinct(
    regime,
    serie_id
  ) %>%
  count(
    regime,
    name = "expected_pairs"
  )

paired_sample_validation <- parameter_matched_tests %>%
  left_join(
    expected_pairs_by_regime,
    by = "regime"
  )

if (
  anyNA(
    paired_sample_validation$
    expected_pairs
  ) ||
  any(
    paired_sample_validation$
    n_pairs !=
    paired_sample_validation$
    expected_pairs
  )
) {
  stop(
    "The contrasts contain an unexpected number of paired series.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Summary by criterion and regime
# ---------------------------------------------------------

parameter_matched_summary <- parameter_matched_tests %>%
  group_by(
    metric,
    regime
  ) %>%
  summarise(
    n_contrasts = n(),
    
    n_significant_bonferroni = sum(
      significant_bonferroni_005
    ),
    
    n_significant_mfef_jp_higher = sum(
      significant_mfef_jp_higher
    ),
    
    n_significant_mfef_higher = sum(
      significant_mfef_higher
    ),
    
    n_significant_holm = sum(
      significant_holm_005
    ),
    
    n_not_significant = sum(
      !significant_bonferroni_005
    ),
    
    median_of_mean_differences = median(
      mean_paired_difference
    ),
    
    minimum_mean_difference = min(
      mean_paired_difference
    ),
    
    maximum_mean_difference = max(
      mean_paired_difference
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    metric,
    regime
  )

if (
  nrow(parameter_matched_summary) != 6L ||
  any(
    parameter_matched_summary$
    n_contrasts != 12L
  ) ||
  any(
    parameter_matched_summary$
    n_significant_bonferroni +
    parameter_matched_summary$
    n_not_significant !=
    parameter_matched_summary$
    n_contrasts
  ) ||
  any(
    parameter_matched_summary$
    n_significant_mfef_jp_higher +
    parameter_matched_summary$
    n_significant_mfef_higher !=
    parameter_matched_summary$
    n_significant_bonferroni
  )
) {
  stop(
    "The parameter-matched summary is internally inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Summary table
# ---------------------------------------------------------

parameter_matched_summary_table <- parameter_matched_summary %>%
  transmute(
    metric,
    regime,
    n_contrasts,
    n_significant_bonferroni,
    n_significant_mfef_jp_higher,
    n_significant_mfef_higher,
    n_significant_holm,
    median_of_mean_differences,
    minimum_mean_difference,
    maximum_mean_difference
  ) %>%
  arrange(
    metric,
    regime
  )

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q19 - PARAMETER-MATCHED MFEF VS MFEF+JP CONTRASTS\n")
cat("=====================================\n")
cat("Paired contrasts: 72\n")
cat("Alternative: two-sided\n")
cat("Primary correction: Bonferroni across 72 contrasts\n")
cat("Sensitivity: Holm correction across 72 contrasts\n")

cat("\n--- Summary by criterion and regime ---\n")

print(
  parameter_matched_summary,
  n = Inf,
  width = Inf
)

cat("\nSummary results retained in:\n")
cat("  parameter_matched_summary_table\n")

cat("\nComplete results retained in:\n")
cat("  parameter_matched_tests\n")
cat("  parameter_matched_summary\n")

# =========================================================
# Q20 - FALSE DETECTIONS IN NULL SERIES
# =========================================================
# FPR is the proportion of null series with at least one
# detected change-point. False-detection burden is summarized
# by the number of detected change-points per null series.

n_null_series <- n_distinct(
  series_without_cp$serie_id
)

if (
  n_null_series !=
  n_series_without_cp ||
  n_null_series != 240L
) {
  stop(
    paste0(
      "Expected 240 null series, but found ",
      n_null_series,
      "."
    ),
    call. = FALSE
  )
}

if (
  anyNA(
    series_without_cp$n_detected
  ) ||
  any(
    series_without_cp$n_detected < 0
  )
) {
  stop(
    paste0(
      "Null-series detection counts must be complete ",
      "and non-negative."
    ),
    call. = FALSE
  )
}

if (
  any(
    series_without_cp$n_detected !=
    as.integer(
      series_without_cp$n_detected
    )
  )
) {
  stop(
    "Null-series detection counts must be integers.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Configuration-level false-detection summary
# ---------------------------------------------------------

null_detection_summary <- series_without_cp %>%
  group_by(
    method_label,
    method_family
  ) %>%
  summarise(
    n_series = n_distinct(
      serie_id
    ),
    
    n_with_detection = sum(
      n_detected >= 1L
    ),
    
    fpr =
      100 *
      n_with_detection /
      n_series,
    
    mean_n_detected = mean(
      n_detected
    ),
    
    sd_n_detected = sd(
      n_detected
    ),
    
    median_n_detected = median(
      n_detected
    ),
    
    q1_n_detected = quantile(
      n_detected,
      probs = 0.25,
      names = FALSE,
      type = 7
    ),
    
    q3_n_detected = quantile(
      n_detected,
      probs = 0.75,
      names = FALSE,
      type = 7
    ),
    
    minimum_n_detected = min(
      n_detected
    ),
    
    maximum_n_detected = max(
      n_detected
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    iqr_n_detected =
      q3_n_detected -
      q1_n_detected
  ) %>%
  arrange(
    fpr,
    mean_n_detected,
    median_n_detected,
    method_label
  )

if (
  nrow(null_detection_summary) != 20L ||
  any(
    null_detection_summary$n_series !=
    n_null_series
  ) ||
  anyNA(
    null_detection_summary$method_family
  )
) {
  stop(
    paste0(
      "Each of the 20 configurations must be evaluated ",
      "on all null series."
    ),
    call. = FALSE
  )
}

if (
  any(
    null_detection_summary$n_with_detection < 0L
  ) ||
  any(
    null_detection_summary$n_with_detection >
    null_detection_summary$n_series
  ) ||
  any(
    null_detection_summary$fpr < 0 |
    null_detection_summary$fpr > 100
  ) ||
  any(
    null_detection_summary$minimum_n_detected >
    null_detection_summary$median_n_detected
  ) ||
  any(
    null_detection_summary$median_n_detected >
    null_detection_summary$maximum_n_detected
  ) ||
  any(
    null_detection_summary$q1_n_detected >
    null_detection_summary$q3_n_detected
  )
) {
  stop(
    "The null-series false-detection summary is inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Overall FPR summary
# ---------------------------------------------------------

null_fpr_summary <- null_detection_summary %>%
  summarise(
    n_configurations = n(),
    
    n_configurations_with_fpr_100 = sum(
      fpr == 100
    ),
    
    minimum_fpr = min(
      fpr
    ),
    
    maximum_fpr = max(
      fpr
    )
  )

if (
  null_fpr_summary$n_configurations != 20L
) {
  stop(
    "The overall null-series summary must contain 20 configurations.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q20 - FALSE DETECTIONS IN NULL SERIES\n")
cat("=====================================\n")
cat("Null series:", n_null_series, "\n")

cat(
  "Configurations with FPR = 100%:",
  null_fpr_summary$
    n_configurations_with_fpr_100,
  "out of",
  null_fpr_summary$
    n_configurations,
  "\n"
)

cat(
  "FPR range:",
  sprintf(
    "%.1f%% to %.1f%%\n",
    null_fpr_summary$minimum_fpr,
    null_fpr_summary$maximum_fpr
  )
)

cat("\n--- Configuration-level false-detection burden ---\n")

print(
  null_detection_summary,
  n = Inf,
  width = Inf
)

cat("\nResults retained in:\n")
cat("  null_detection_summary\n")
cat("  null_fpr_summary\n")

# =========================================================
# Q21 - FRIEDMAN-NEMENYI ANALYSIS OF NULL-SERIES BURDEN
# =========================================================
# Lower n_detected values indicate better performance.
# All null series are retained as blocks in the Friedman test.
# Bootstrap intervals describe mean-rank stability.
# Plot segments represent mean rank +/- CD / 2.

null_rank_bootstrap_seed <- 2025L

# ---------------------------------------------------------
# Prepare the null-series matrix
# ---------------------------------------------------------

null_detection_matrix_data <- series_without_cp %>%
  select(
    serie_id,
    method_label,
    n_detected
  ) %>%
  pivot_wider(
    names_from = method_label,
    values_from = n_detected
  ) %>%
  arrange(
    serie_id
  )

if (
  anyDuplicated(
    null_detection_matrix_data$serie_id
  ) > 0L
) {
  stop(
    "Duplicated null-series identifiers were found.",
    call. = FALSE
  )
}

null_detection_matrix <- null_detection_matrix_data %>%
  select(
    -serie_id
  ) %>%
  as.matrix()

if (
  !is.numeric(
    null_detection_matrix
  ) ||
  anyNA(
    null_detection_matrix
  ) ||
  any(
    null_detection_matrix < 0
  )
) {
  stop(
    paste0(
      "The null-series detection matrix must contain ",
      "complete, non-negative numeric values."
    ),
    call. = FALSE
  )
}

n_null_series_rank <- nrow(
  null_detection_matrix
)

n_null_configurations <- ncol(
  null_detection_matrix
)

if (
  n_null_series_rank !=
  n_null_series
) {
  stop(
    paste0(
      "Expected ",
      n_null_series,
      " null-series rows, but found ",
      n_null_series_rank,
      "."
    ),
    call. = FALSE
  )
}

if (n_null_configurations != 20L) {
  stop(
    paste0(
      "Expected 20 configurations, but found ",
      n_null_configurations,
      "."
    ),
    call. = FALSE
  )
}

if (
  !setequal(
    colnames(
      null_detection_matrix
    ),
    config_catalog$method_label
  )
) {
  stop(
    paste0(
      "The null-series matrix configurations do not match ",
      "the validated configuration catalog."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Classify informative and invariant null series
# ---------------------------------------------------------

informative_null_rows <- apply(
  null_detection_matrix,
  1,
  function(row_values) {
    length(
      unique(
        row_values
      )
    ) > 1L
  }
)

n_informative_null_series <- sum(
  informative_null_rows
)

n_invariant_null_series <-
  n_null_series_rank -
  n_informative_null_series

if (
  n_informative_null_series +
  n_invariant_null_series !=
  n_null_series_rank
) {
  stop(
    "The informative and invariant null-series counts are inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Friedman test and mean ranks
# ---------------------------------------------------------

friedman_null_result <- friedman.test(
  null_detection_matrix
)

null_rank_matrix <- t(
  apply(
    null_detection_matrix,
    1,
    function(row_values) {
      rank(
        row_values,
        ties.method = "average"
      )
    }
  )
)

colnames(null_rank_matrix) <-
  colnames(null_detection_matrix)

if (
  anyNA(
    null_rank_matrix
  ) ||
  nrow(null_rank_matrix) !=
  n_null_series_rank ||
  ncol(null_rank_matrix) !=
  n_null_configurations
) {
  stop(
    "The null-series rank matrix is incomplete or invalid.",
    call. = FALSE
  )
}

mean_null_ranks <- sort(
  colMeans(
    null_rank_matrix
  )
)

# ---------------------------------------------------------
# Nemenyi pairwise comparisons
# ---------------------------------------------------------

null_rank_standard_error <- sqrt(
  n_null_configurations *
    (n_null_configurations + 1) /
    (
      6 *
        n_null_series_rank
    )
)

null_rank_q_critical <- qtukey(
  p = 0.95,
  nmeans = n_null_configurations,
  df = Inf
) / sqrt(2)

null_rank_critical_difference <-
  null_rank_q_critical *
  null_rank_standard_error

null_nemenyi_pairwise <- tidyr::expand_grid(
  configuration_a =
    names(
      mean_null_ranks
    ),
  
  configuration_b =
    names(
      mean_null_ranks
    )
) %>%
  filter(
    configuration_a <
      configuration_b
  ) %>%
  mutate(
    mean_rank_a =
      mean_null_ranks[
        configuration_a
      ],
    
    mean_rank_b =
      mean_null_ranks[
        configuration_b
      ],
    
    absolute_rank_difference = abs(
      mean_rank_a -
        mean_rank_b
    ),
    
    nemenyi_q =
      absolute_rank_difference /
      null_rank_standard_error *
      sqrt(2),
    
    p_value_nemenyi = ptukey(
      q = nemenyi_q,
      nmeans = n_null_configurations,
      df = Inf,
      lower.tail = FALSE
    ),
    
    significant_005 =
      p_value_nemenyi < 0.05,
    
    different_by_cd =
      absolute_rank_difference >
      null_rank_critical_difference,
    
    better_configuration = case_when(
      mean_rank_a < mean_rank_b ~
        configuration_a,
      
      mean_rank_b < mean_rank_a ~
        configuration_b,
      
      TRUE ~
        "Rank tie"
    ),
    
    worse_configuration = case_when(
      mean_rank_a > mean_rank_b ~
        configuration_a,
      
      mean_rank_b > mean_rank_a ~
        configuration_b,
      
      TRUE ~
        "Rank tie"
    )
  ) %>%
  arrange(
    p_value_nemenyi,
    desc(
      absolute_rank_difference
    )
  )

if (nrow(null_nemenyi_pairwise) != 190L) {
  stop(
    paste0(
      "Expected 190 Nemenyi comparisons, but found ",
      nrow(null_nemenyi_pairwise),
      "."
    ),
    call. = FALSE
  )
}

if (
  any(
    null_nemenyi_pairwise$significant_005 !=
    null_nemenyi_pairwise$different_by_cd
  )
) {
  stop(
    paste0(
      "Nemenyi p-values and critical-difference ",
      "decisions are inconsistent."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Bootstrap intervals for mean ranks
# ---------------------------------------------------------
# Percentile intervals are descriptive and are not used for
# Friedman-Nemenyi pairwise inference.

if (run_bootstrap) {
  
  set.seed(
    null_rank_bootstrap_seed
  )
  
  bootstrap_null_ranks <- replicate(
    n_boot,
    {
      sampled_rows <- sample.int(
        n = n_null_series_rank,
        size = n_null_series_rank,
        replace = TRUE
      )
      
      colMeans(
        null_rank_matrix[
          sampled_rows,
          ,
          drop = FALSE
        ]
      )
    }
  )
  
  if (
    nrow(bootstrap_null_ranks) !=
    n_null_configurations ||
    ncol(bootstrap_null_ranks) !=
    n_boot
  ) {
    stop(
      "Unexpected bootstrap rank-matrix dimensions were produced.",
      call. = FALSE
    )
  }
  
  bootstrap_null_lower <- apply(
    bootstrap_null_ranks,
    1,
    quantile,
    probs = 0.025,
    names = FALSE,
    type = 7
  )
  
  bootstrap_null_upper <- apply(
    bootstrap_null_ranks,
    1,
    quantile,
    probs = 0.975,
    names = FALSE,
    type = 7
  )
  
  bootstrap_null_standard_error <- apply(
    bootstrap_null_ranks,
    1,
    sd
  )
  
} else {
  
  bootstrap_null_ranks <- NULL
  
  bootstrap_null_lower <- setNames(
    rep(
      NA_real_,
      n_null_configurations
    ),
    colnames(
      null_rank_matrix
    )
  )
  
  bootstrap_null_upper <- setNames(
    rep(
      NA_real_,
      n_null_configurations
    ),
    colnames(
      null_rank_matrix
    )
  )
  
  bootstrap_null_standard_error <- setNames(
    rep(
      NA_real_,
      n_null_configurations
    ),
    colnames(
      null_rank_matrix
    )
  )
}

# ---------------------------------------------------------
# Configuration-level rank results
# ---------------------------------------------------------

null_rank_table <- tibble(
  method_label =
    names(
      mean_null_ranks
    ),
  
  mean_rank =
    as.numeric(
      mean_null_ranks
    ),
  
  bootstrap_standard_error =
    as.numeric(
      bootstrap_null_standard_error[
        names(
          mean_null_ranks
        )
      ]
    ),
  
  bootstrap_lower =
    as.numeric(
      bootstrap_null_lower[
        names(
          mean_null_ranks
        )
      ]
    ),
  
  bootstrap_upper =
    as.numeric(
      bootstrap_null_upper[
        names(
          mean_null_ranks
        )
      ]
    )
) %>%
  left_join(
    config_catalog,
    by = "method_label"
  ) %>%
  left_join(
    null_detection_summary %>%
      select(
        method_label,
        fpr,
        mean_n_detected,
        sd_n_detected,
        median_n_detected,
        q1_n_detected,
        q3_n_detected,
        minimum_n_detected,
        maximum_n_detected
      ),
    by = "method_label"
  ) %>%
  mutate(
    mean_rank_ci_label = if (
      run_bootstrap
    ) {
      sprintf(
        "%.2f [%.2f, %.2f]",
        mean_rank,
        bootstrap_lower,
        bootstrap_upper
      )
    } else {
      sprintf(
        "%.2f",
        mean_rank
      )
    }
  ) %>%
  arrange(
    mean_rank,
    mean_n_detected,
    method_label
  )

if (nrow(null_rank_table) != 20L) {
  stop(
    paste0(
      "Expected 20 configuration-level rank results, but found ",
      nrow(null_rank_table),
      "."
    ),
    call. = FALSE
  )
}

if (
  anyNA(
    null_rank_table$method_family
  ) ||
  anyNA(
    null_rank_table$mean_n_detected
  )
) {
  stop(
    "Missing configuration metadata were found in the rank results.",
    call. = FALSE
  )
}

if (
  run_bootstrap &&
  (
    anyNA(
      null_rank_table$bootstrap_lower
    ) ||
    anyNA(
      null_rank_table$bootstrap_upper
    ) ||
    any(
      null_rank_table$bootstrap_lower >
      null_rank_table$bootstrap_upper
    )
  )
) {
  stop(
    "Invalid bootstrap intervals were produced for the mean ranks.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Summary table
# ---------------------------------------------------------

null_rank_summary <- tibble(
  n_null_series =
    n_null_series_rank,
  
  n_informative_null_series =
    n_informative_null_series,
  
  n_invariant_null_series =
    n_invariant_null_series,
  
  n_configurations =
    n_null_configurations,
  
  friedman_chi_square =
    unname(
      friedman_null_result$statistic
    ),
  
  friedman_df =
    unname(
      friedman_null_result$parameter
    ),
  
  friedman_p_value =
    friedman_null_result$p.value,
  
  critical_difference =
    null_rank_critical_difference,
  
  n_significant_nemenyi_pairs = sum(
    null_nemenyi_pairwise$
      significant_005
  )
)

null_rank_summary_table <- null_rank_table %>%
  transmute(
    method_label,
    method_family,
    fpr,
    mean_n_detected,
    sd_n_detected,
    median_n_detected,
    q1_n_detected,
    q3_n_detected,
    mean_rank,
    bootstrap_lower,
    bootstrap_upper,
    mean_rank_ci_label
  ) %>%
  arrange(
    mean_rank,
    mean_n_detected,
    method_label
  )

null_nemenyi_summary <- null_nemenyi_pairwise %>%
  summarise(
    n_unique_pairs = n(),
    
    n_significant_pairs = sum(
      significant_005
    ),
    
    n_not_significant_pairs = sum(
      !significant_005
    ),
    
    minimum_significant_rank_difference = if (
      any(
        significant_005
      )
    ) {
      min(
        absolute_rank_difference[
          significant_005
        ]
      )
    } else {
      NA_real_
    },
    
    maximum_significant_rank_difference = if (
      any(
        significant_005
      )
    ) {
      max(
        absolute_rank_difference[
          significant_005
        ]
      )
    } else {
      NA_real_
    }
  )

if (
  null_nemenyi_summary$n_unique_pairs != 190L ||
  null_nemenyi_summary$n_significant_pairs +
  null_nemenyi_summary$n_not_significant_pairs != 190L
) {
  stop(
    "The Nemenyi comparison summary is internally inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Critical-difference reference plot
# ---------------------------------------------------------
# Points represent mean ranks.
# Segments represent mean rank +/- CD / 2.

null_rank_plot_data <- null_rank_table %>%
  mutate(
    lower_cd_reference =
      mean_rank -
      null_rank_critical_difference / 2,
    
    upper_cd_reference =
      mean_rank +
      null_rank_critical_difference / 2
  )

p_null_rank_cd <- ggplot(
  null_rank_plot_data,
  aes(
    x = mean_rank,
    y = reorder(
      method_label,
      -mean_rank
    ),
    color = method_family
  )
) +
  geom_segment(
    aes(
      x = lower_cd_reference,
      xend = upper_cd_reference,
      yend = reorder(
        method_label,
        -mean_rank
      )
    ),
    linewidth = 1.4,
    alpha = 0.35
  ) +
  geom_point(
    size = 3
  ) +
  scale_color_manual(
    values = wong_palette,
    drop = FALSE
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Mean rank for n_detected (lower is better)",
    y = NULL,
    color = "Family"
  ) +
  theme_pub(
    base_size = 11
  ) +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    
    axis.title = element_text(
      size = 11
    ),
    
    axis.text.x = element_text(
      size = 9,
      color = "black"
    ),
    
    axis.text.y = element_text(
      size = 9,
      color = "black"
    ),
    
    legend.title = element_text(
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    )
  )

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q21 - FRIEDMAN-NEMENYI ANALYSIS OF NULL-SERIES BURDEN\n")
cat("=====================================\n")
cat("Null series:", n_null_series_rank, "\n")
cat("Informative series:", n_informative_null_series, "\n")
cat("Invariant series:", n_invariant_null_series, "\n")
cat("Configurations:", n_null_configurations, "\n")

cat(
  "Friedman chi-square:",
  sprintf(
    "%.3f",
    unname(
      friedman_null_result$statistic
    )
  ),
  "\n"
)

cat(
  "Friedman df:",
  unname(
    friedman_null_result$parameter
  ),
  "\n"
)

cat(
  "Friedman p-value:",
  format.pval(
    friedman_null_result$p.value,
    digits = 3,
    eps = 2.2e-16
  ),
  "\n"
)

cat(
  "Nemenyi critical difference:",
  sprintf(
    "%.3f",
    null_rank_critical_difference
  ),
  "\n"
)

cat(
  "Significant Nemenyi pairs:",
  null_rank_summary$
    n_significant_nemenyi_pairs,
  "out of 190\n"
)

if (run_bootstrap) {
  cat("Bootstrap replicates:", n_boot, "\n")
  cat("Bootstrap seed:", null_rank_bootstrap_seed, "\n")
}

cat("\n--- Configuration-level null-series ranking ---\n")

print(
  null_rank_summary_table,
  n = Inf,
  width = Inf
)

cat("\n--- Nemenyi comparison summary ---\n")

print(
  null_nemenyi_summary,
  width = Inf
)

print(
  p_null_rank_cd
)

cat("\nSummary outputs retained in:\n")
cat("  null_rank_summary_table\n")
cat("  p_null_rank_cd\n")

cat("\nComplete results retained in:\n")
cat("  null_rank_summary\n")
cat("  null_rank_table\n")
cat("  null_nemenyi_pairwise\n")
cat("  null_nemenyi_summary\n")
cat("  null_rank_plot_data\n")

# =========================================================
# Q22 - PAIRED WILCOXON TESTS OF FALSE-DETECTION BURDEN
# =========================================================
# Configurations are compared across the same 240 null series.
# Lower n_detected values indicate lower false-detection burden.
# Significance is determined by the paired Wilcoxon test.
# Direction is assigned from the mean paired difference.

null_configuration_levels <- config_catalog %>%
  pull(
    method_label
  )

if (
  length(null_configuration_levels) != 20L ||
  anyDuplicated(null_configuration_levels) > 0L
) {
  stop(
    paste0(
      "Expected 20 unique configurations, but found ",
      length(null_configuration_levels),
      "."
    ),
    call. = FALSE
  )
}

null_configuration_pairs <- combn(
  null_configuration_levels,
  2,
  simplify = FALSE
) %>%
  map_dfr(
    ~ tibble(
      configuration_a = .x[[1]],
      configuration_b = .x[[2]]
    )
  )

if (nrow(null_configuration_pairs) != 190L) {
  stop(
    paste0(
      "Expected 190 unique configuration pairs, but found ",
      nrow(null_configuration_pairs),
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Run one paired comparison
# ---------------------------------------------------------

run_null_detection_wilcoxon <- function(
    df,
    configuration_a,
    configuration_b) {
  
  paired_data <- df %>%
    filter(
      method_label %in% c(
        configuration_a,
        configuration_b
      )
    ) %>%
    select(
      serie_id,
      method_label,
      n_detected
    ) %>%
    pivot_wider(
      names_from = method_label,
      values_from = n_detected
    ) %>%
    arrange(
      serie_id
    )
  
  expected_null_series <- n_distinct(
    df$serie_id
  )
  
  if (
    expected_null_series !=
    n_null_series ||
    expected_null_series != 240L
  ) {
    stop(
      paste0(
        "Expected 240 null series, but found ",
        expected_null_series,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (nrow(paired_data) != expected_null_series) {
    stop(
      paste0(
        "Incomplete null-series pair for ",
        configuration_a,
        " versus ",
        configuration_b,
        ". Expected ",
        expected_null_series,
        " paired series but found ",
        nrow(paired_data),
        "."
      ),
      call. = FALSE
    )
  }
  
  n_detected_a <- paired_data[[
    configuration_a
  ]]
  
  n_detected_b <- paired_data[[
    configuration_b
  ]]
  
  if (
    is.null(n_detected_a) ||
    is.null(n_detected_b) ||
    anyNA(n_detected_a) ||
    anyNA(n_detected_b)
  ) {
    stop(
      paste0(
        "Missing n_detected values for ",
        configuration_a,
        " versus ",
        configuration_b,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(n_detected_a < 0) ||
    any(n_detected_b < 0)
  ) {
    stop(
      paste0(
        "Negative n_detected values for ",
        configuration_a,
        " versus ",
        configuration_b,
        "."
      ),
      call. = FALSE
    )
  }
  
  paired_difference <-
    n_detected_a -
    n_detected_b
  
  n_pairs <- length(
    paired_difference
  )
  
  n_a_lower <- sum(
    paired_difference < 0
  )
  
  n_tied <- sum(
    paired_difference == 0
  )
  
  n_b_lower <- sum(
    paired_difference > 0
  )
  
  n_nonzero_differences <- sum(
    paired_difference != 0
  )
  
  if (
    n_a_lower +
    n_tied +
    n_b_lower !=
    n_pairs
  ) {
    stop(
      paste0(
        "Direction counts are inconsistent for ",
        configuration_a,
        " versus ",
        configuration_b,
        "."
      ),
      call. = FALSE
    )
  }
  
  test_performed <-
    n_nonzero_differences > 0L
  
  if (test_performed) {
    
    wilcoxon_result <- suppressWarnings(
      wilcox.test(
        n_detected_a,
        n_detected_b,
        paired = TRUE,
        alternative = "two.sided",
        exact = FALSE,
        correct = TRUE
      )
    )
    
    wilcoxon_statistic <- unname(
      wilcoxon_result$statistic
    )
    
    p_value <-
      wilcoxon_result$p.value
    
  } else {
    
    wilcoxon_statistic <- NA_real_
    p_value <- NA_real_
  }
  
  tibble(
    configuration_a =
      configuration_a,
    
    configuration_b =
      configuration_b,
    
    n_pairs =
      n_pairs,
    
    n_nonzero_differences =
      n_nonzero_differences,
    
    n_a_lower =
      n_a_lower,
    
    n_tied =
      n_tied,
    
    n_b_lower =
      n_b_lower,
    
    mean_a = mean(
      n_detected_a
    ),
    
    mean_b = mean(
      n_detected_b
    ),
    
    median_a = median(
      n_detected_a
    ),
    
    median_b = median(
      n_detected_b
    ),
    
    mean_paired_difference = mean(
      paired_difference
    ),
    
    median_paired_difference = median(
      paired_difference
    ),
    
    wilcoxon_statistic =
      wilcoxon_statistic,
    
    p_value =
      p_value,
    
    test_performed =
      test_performed
  )
}

# ---------------------------------------------------------
# Run all pairwise comparisons
# ---------------------------------------------------------

pairwise_null_wilcoxon <- null_configuration_pairs %>%
  pmap_dfr(
    function(
    configuration_a,
    configuration_b) {
      
      run_null_detection_wilcoxon(
        df = series_without_cp,
        configuration_a =
          configuration_a,
        configuration_b =
          configuration_b
      )
    }
  )

if (nrow(pairwise_null_wilcoxon) != 190L) {
  stop(
    paste0(
      "Expected 190 pairwise results, but found ",
      nrow(pairwise_null_wilcoxon),
      "."
    ),
    call. = FALSE
  )
}

if (
  any(
    pairwise_null_wilcoxon$n_pairs !=
    n_null_series
  )
) {
  stop(
    "Each null-series comparison must contain 240 paired series.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Multiplicity correction
# ---------------------------------------------------------

pairwise_null_wilcoxon <- pairwise_null_wilcoxon %>%
  mutate(
    p_bonferroni = p.adjust(
      p_value,
      method = "bonferroni",
      n = 190
    ),
    
    p_holm = p.adjust(
      p_value,
      method = "holm",
      n = 190
    ),
    
    significant_bonferroni_005 =
      !is.na(p_bonferroni) &
      p_bonferroni < 0.05,
    
    significant_bonferroni_001 =
      !is.na(p_bonferroni) &
      p_bonferroni < 0.01,
    
    significant_holm_005 =
      !is.na(p_holm) &
      p_holm < 0.05
  ) %>%
  arrange(
    configuration_a,
    configuration_b
  )

# ---------------------------------------------------------
# Expand results into directed comparisons
# ---------------------------------------------------------
# A significant win indicates significantly fewer detections.

directed_null_a <- pairwise_null_wilcoxon %>%
  transmute(
    configuration =
      configuration_a,
    
    competitor =
      configuration_b,
    
    n_pairs,
    n_nonzero_differences,
    
    mean_configuration =
      mean_a,
    
    mean_competitor =
      mean_b,
    
    median_configuration =
      median_a,
    
    median_competitor =
      median_b,
    
    mean_difference =
      mean_paired_difference,
    
    median_difference =
      median_paired_difference,
    
    n_configuration_lower =
      n_a_lower,
    
    n_tied,
    
    n_configuration_higher =
      n_b_lower,
    
    test_performed,
    p_value,
    p_bonferroni,
    p_holm,
    significant_bonferroni_005,
    significant_bonferroni_001,
    significant_holm_005
  )

directed_null_b <- pairwise_null_wilcoxon %>%
  transmute(
    configuration =
      configuration_b,
    
    competitor =
      configuration_a,
    
    n_pairs,
    n_nonzero_differences,
    
    mean_configuration =
      mean_b,
    
    mean_competitor =
      mean_a,
    
    median_configuration =
      median_b,
    
    median_competitor =
      median_a,
    
    mean_difference =
      -mean_paired_difference,
    
    median_difference =
      -median_paired_difference,
    
    n_configuration_lower =
      n_b_lower,
    
    n_tied,
    
    n_configuration_higher =
      n_a_lower,
    
    test_performed,
    p_value,
    p_bonferroni,
    p_holm,
    significant_bonferroni_005,
    significant_bonferroni_001,
    significant_holm_005
  )

directed_null_wilcoxon <- bind_rows(
  directed_null_a,
  directed_null_b
) %>%
  mutate(
    significant_win_bonferroni_005 =
      significant_bonferroni_005 &
      mean_difference < 0,
    
    significant_loss_bonferroni_005 =
      significant_bonferroni_005 &
      mean_difference > 0,
    
    significant_win_bonferroni_001 =
      significant_bonferroni_001 &
      mean_difference < 0,
    
    significant_loss_bonferroni_001 =
      significant_bonferroni_001 &
      mean_difference > 0,
    
    significant_win_holm_005 =
      significant_holm_005 &
      mean_difference < 0,
    
    significant_loss_holm_005 =
      significant_holm_005 &
      mean_difference > 0
  )

if (nrow(directed_null_wilcoxon) != 380L) {
  stop(
    paste0(
      "Expected 380 directed results, but found ",
      nrow(directed_null_wilcoxon),
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Configuration-level significant-win ranking
# ---------------------------------------------------------

null_wilcoxon_ranking <- directed_null_wilcoxon %>%
  group_by(
    configuration
  ) %>%
  summarise(
    n_possible_comparisons =
      n(),
    
    n_testable_comparisons = sum(
      test_performed
    ),
    
    n_unavailable_comparisons =
      n_possible_comparisons -
      n_testable_comparisons,
    
    n_significant_wins_bonferroni_005 = sum(
      significant_win_bonferroni_005
    ),
    
    n_significant_losses_bonferroni_005 = sum(
      significant_loss_bonferroni_005
    ),
    
    n_significant_wins_bonferroni_001 = sum(
      significant_win_bonferroni_001
    ),
    
    n_significant_losses_bonferroni_001 = sum(
      significant_loss_bonferroni_001
    ),
    
    n_significant_wins_holm_005 = sum(
      significant_win_holm_005
    ),
    
    n_significant_losses_holm_005 = sum(
      significant_loss_holm_005
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    significant_win_rate_bonferroni_005 = round(
      100 *
        n_significant_wins_bonferroni_005 /
        n_possible_comparisons,
      1
    ),
    
    significant_win_rate_bonferroni_001 = round(
      100 *
        n_significant_wins_bonferroni_001 /
        n_possible_comparisons,
      1
    ),
    
    significant_win_rate_holm_005 = round(
      100 *
        n_significant_wins_holm_005 /
        n_possible_comparisons,
      1
    )
  ) %>%
  left_join(
    config_catalog %>%
      rename(
        configuration =
          method_label
      ),
    by = "configuration"
  ) %>%
  left_join(
    null_detection_summary %>%
      select(
        method_label,
        mean_n_detected,
        median_n_detected
      ) %>%
      rename(
        configuration =
          method_label
      ),
    by = "configuration"
  ) %>%
  left_join(
    null_rank_table %>%
      select(
        method_label,
        mean_rank,
        mean_rank_ci_label
      ) %>%
      rename(
        configuration =
          method_label
      ),
    by = "configuration"
  ) %>%
  arrange(
    desc(
      n_significant_wins_bonferroni_005
    ),
    n_significant_losses_bonferroni_005,
    mean_rank,
    configuration
  )

if (
  nrow(null_wilcoxon_ranking) != 20L ||
  any(
    null_wilcoxon_ranking$
    n_possible_comparisons != 19L
  )
) {
  stop(
    paste0(
      "Each of the 20 configurations must have ",
      "19 possible comparisons."
    ),
    call. = FALSE
  )
}

if (
  any(
    null_wilcoxon_ranking$
    n_testable_comparisons +
    null_wilcoxon_ranking$
    n_unavailable_comparisons !=
    null_wilcoxon_ranking$
    n_possible_comparisons
  )
) {
  stop(
    "The testable and unavailable comparison counts are inconsistent.",
    call. = FALSE
  )
}

if (
  anyNA(
    null_wilcoxon_ranking$
    method_family
  ) ||
  anyNA(
    null_wilcoxon_ranking$
    mean_n_detected
  ) ||
  anyNA(
    null_wilcoxon_ranking$
    mean_rank
  )
) {
  stop(
    "Missing configuration metadata were found.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Pairwise-test summary
# ---------------------------------------------------------

null_wilcoxon_summary <- pairwise_null_wilcoxon %>%
  summarise(
    n_unique_pairs =
      n(),
    
    n_tests_performed = sum(
      test_performed
    ),
    
    n_unavailable_pairs = sum(
      !test_performed
    ),
    
    n_significant_pairs_bonferroni_005 = sum(
      significant_bonferroni_005
    ),
    
    n_significant_pairs_bonferroni_001 = sum(
      significant_bonferroni_001
    ),
    
    n_significant_pairs_holm_005 = sum(
      significant_holm_005
    ),
    
    minimum_nonzero_differences = min(
      n_nonzero_differences
    ),
    
    median_nonzero_differences = median(
      n_nonzero_differences
    ),
    
    maximum_nonzero_differences = max(
      n_nonzero_differences
    )
  )

if (
  null_wilcoxon_summary$n_unique_pairs != 190L ||
  null_wilcoxon_summary$n_tests_performed +
  null_wilcoxon_summary$n_unavailable_pairs !=
  190L
) {
  stop(
    "The pairwise-test summary is internally inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Validate significant-comparison counts and directions
# ---------------------------------------------------------

null_direction_validation <- null_wilcoxon_ranking %>%
  summarise(
    total_wins_bonferroni_005 = sum(
      n_significant_wins_bonferroni_005
    ),
    
    total_losses_bonferroni_005 = sum(
      n_significant_losses_bonferroni_005
    ),
    
    total_wins_bonferroni_001 = sum(
      n_significant_wins_bonferroni_001
    ),
    
    total_losses_bonferroni_001 = sum(
      n_significant_losses_bonferroni_001
    ),
    
    total_wins_holm_005 = sum(
      n_significant_wins_holm_005
    ),
    
    total_losses_holm_005 = sum(
      n_significant_losses_holm_005
    )
  ) %>%
  bind_cols(
    null_wilcoxon_summary %>%
      select(
        n_significant_pairs_bonferroni_005,
        n_significant_pairs_bonferroni_001,
        n_significant_pairs_holm_005
      )
  )

invalid_null_direction_counts <- null_direction_validation %>%
  filter(
    total_wins_bonferroni_005 !=
      n_significant_pairs_bonferroni_005 |
      
      total_losses_bonferroni_005 !=
      n_significant_pairs_bonferroni_005 |
      
      total_wins_bonferroni_001 !=
      n_significant_pairs_bonferroni_001 |
      
      total_losses_bonferroni_001 !=
      n_significant_pairs_bonferroni_001 |
      
      total_wins_holm_005 !=
      n_significant_pairs_holm_005 |
      
      total_losses_holm_005 !=
      n_significant_pairs_holm_005
  )

if (nrow(invalid_null_direction_counts) > 0L) {
  stop(
    paste0(
      "Significant-win and significant-loss counts are ",
      "inconsistent with the significant-pair counts."
    ),
    call. = FALSE
  )
}

significant_null_zero_mean <- pairwise_null_wilcoxon %>%
  filter(
    significant_bonferroni_005 |
      significant_bonferroni_001 |
      significant_holm_005,
    mean_paired_difference == 0
  )

if (nrow(significant_null_zero_mean) > 0L) {
  stop(
    paste0(
      "Significant comparisons with zero mean paired ",
      "difference were found."
    ),
    call. = FALSE
  )
}

null_direction_consistency <- pairwise_null_wilcoxon %>%
  filter(
    significant_bonferroni_005
  ) %>%
  summarise(
    n_significant_pairs =
      n(),
    
    n_a_lower = sum(
      mean_paired_difference < 0
    ),
    
    n_b_lower = sum(
      mean_paired_difference > 0
    ),
    
    n_zero_direction = sum(
      mean_paired_difference == 0
    )
  )

if (
  null_direction_consistency$n_a_lower +
  null_direction_consistency$n_b_lower !=
  null_direction_consistency$n_significant_pairs ||
  null_direction_consistency$n_zero_direction != 0L
) {
  stop(
    "Inconsistent directions were found for significant pairs.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Configuration summary table
# ---------------------------------------------------------

null_wilcoxon_summary_table <- null_wilcoxon_ranking %>%
  transmute(
    configuration,
    method_family,
    mean_n_detected,
    median_n_detected,
    mean_rank,
    mean_rank_ci_label,
    
    significant_wins_bonferroni_005 =
      n_significant_wins_bonferroni_005,
    
    significant_losses_bonferroni_005 =
      n_significant_losses_bonferroni_005,
    
    significant_win_rate_bonferroni_005,
    
    significant_wins_bonferroni_001 =
      n_significant_wins_bonferroni_001,
    
    significant_wins_holm_005 =
      n_significant_wins_holm_005
  ) %>%
  arrange(
    desc(
      significant_wins_bonferroni_005
    ),
    significant_losses_bonferroni_005,
    mean_rank,
    configuration
  )

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q22 - PAIRED WILCOXON TESTS OF FALSE-DETECTION BURDEN\n")
cat("=====================================\n")
cat("Null series per comparison:", n_null_series, "\n")
cat("Configuration pairs: 190\n")
cat("Alternative: two-sided\n")
cat("Primary correction: Bonferroni across 190 pairs\n")
cat("Sensitivity: Bonferroni alpha = 0.01; Holm alpha = 0.05\n")
cat("Lower n_detected values indicate better performance\n")

cat("\n--- Pairwise-test summary ---\n")

print(
  null_wilcoxon_summary,
  width = Inf
)

cat("\n--- Configuration ranking summary ---\n")

print(
  null_wilcoxon_summary_table,
  n = Inf,
  width = Inf
)

cat("\nComplete results retained in:\n")
cat("  pairwise_null_wilcoxon\n")
cat("  directed_null_wilcoxon\n")
cat("  null_wilcoxon_ranking\n")
cat("  null_wilcoxon_summary\n")
cat("  null_wilcoxon_summary_table\n")

# =========================================================
# Q23 - FRIEDMAN-NEMENYI ANALYSIS
# =========================================================
# Invariant series are excluded from rank-based comparisons.
# Nemenyi significance is determined by the critical difference.
# Non-significance sets may overlap.
# Plot segments represent mean rank +/- CD / 2 and are not
# confidence intervals.

run_friedman_nemenyi <- function(
    df,
    metric_col,
    subset_name,
    alpha = 0.05) {
  
  validate_metric(
    df,
    metric_col
  )
  
  matrix_data <- df %>%
    select(
      serie_id,
      method_label,
      metric_value =
        all_of(metric_col)
    ) %>%
    pivot_wider(
      names_from =
        method_label,
      values_from =
        metric_value
    ) %>%
    arrange(
      serie_id
    )
  
  if (
    anyDuplicated(
      matrix_data$serie_id
    ) > 0L
  ) {
    stop(
      paste0(
        "Duplicated series were found for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  metric_matrix <- matrix_data %>%
    select(
      -serie_id
    ) %>%
    as.matrix()
  
  if (
    !is.numeric(metric_matrix) ||
    anyNA(metric_matrix)
  ) {
    stop(
      paste0(
        "A complete numeric F1 matrix is required for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  n_series_total <- nrow(
    metric_matrix
  )
  
  n_methods <- ncol(
    metric_matrix
  )
  
  if (n_methods != 20L) {
    stop(
      paste0(
        "Expected 20 configurations for ",
        subset_name,
        ", but found ",
        n_methods,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    !setequal(
      colnames(metric_matrix),
      config_catalog$method_label
    )
  ) {
    stop(
      paste0(
        "The configurations do not match the validated ",
        "configuration catalog for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      metric_matrix < 0 |
      metric_matrix > 1
    )
  ) {
    stop(
      paste0(
        "F1 values must remain within [0, 1] for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  informative_rows <- apply(
    metric_matrix,
    1,
    function(row_values) {
      length(
        unique(
          row_values
        )
      ) > 1L
    }
  )
  
  n_series_informative <- sum(
    informative_rows
  )
  
  n_series_invariant <-
    n_series_total -
    n_series_informative
  
  if (
    n_series_informative +
    n_series_invariant !=
    n_series_total
  ) {
    stop(
      paste0(
        "Informative and invariant series counts are inconsistent for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (n_series_informative < 3L) {
    stop(
      paste0(
        "Fewer than three informative series were available for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  analysis_matrix <- metric_matrix[
    informative_rows,
    ,
    drop = FALSE
  ]
  
  friedman_result <- friedman.test(
    analysis_matrix
  )
  
  rank_matrix <- t(
    apply(
      analysis_matrix,
      1,
      function(row_values) {
        rank(
          -row_values,
          ties.method = "average"
        )
      }
    )
  )
  
  colnames(rank_matrix) <- colnames(
    analysis_matrix
  )
  
  if (
    nrow(rank_matrix) !=
    n_series_informative ||
    ncol(rank_matrix) !=
    n_methods ||
    anyNA(rank_matrix)
  ) {
    stop(
      paste0(
        "The rank matrix is incomplete or invalid for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  mean_ranks <- sort(
    colMeans(
      rank_matrix
    )
  )
  
  standard_error <- sqrt(
    n_methods *
      (n_methods + 1) /
      (
        6 *
          n_series_informative
      )
  )
  
  q_critical <- qtukey(
    p = 1 - alpha,
    nmeans = n_methods,
    df = Inf
  ) / sqrt(2)
  
  critical_difference <-
    q_critical *
    standard_error
  
  nemenyi_pairwise <- tidyr::expand_grid(
    configuration_a =
      names(mean_ranks),
    
    configuration_b =
      names(mean_ranks)
  ) %>%
    filter(
      configuration_a <
        configuration_b
    ) %>%
    mutate(
      rank_a =
        mean_ranks[
          configuration_a
        ],
      
      rank_b =
        mean_ranks[
          configuration_b
        ],
      
      absolute_rank_difference = abs(
        rank_a -
          rank_b
      ),
      
      nemenyi_q =
        absolute_rank_difference /
        standard_error *
        sqrt(2),
      
      p_value_nemenyi = ptukey(
        q = nemenyi_q,
        nmeans = n_methods,
        df = Inf,
        lower.tail = FALSE
      ),
      
      significant =
        p_value_nemenyi <
        alpha,
      
      different_by_cd =
        absolute_rank_difference >
        critical_difference
    ) %>%
    arrange(
      p_value_nemenyi,
      desc(
        absolute_rank_difference
      )
    )
  
  if (nrow(nemenyi_pairwise) != 190L) {
    stop(
      paste0(
        "Expected 190 Nemenyi comparisons for ",
        subset_name,
        ", but found ",
        nrow(nemenyi_pairwise),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    anyNA(
      nemenyi_pairwise$p_value_nemenyi
    ) ||
    any(
      nemenyi_pairwise$p_value_nemenyi < 0 |
      nemenyi_pairwise$p_value_nemenyi > 1
    )
  ) {
    stop(
      paste0(
        "Invalid Nemenyi p-values were produced for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      nemenyi_pairwise$significant !=
      nemenyi_pairwise$different_by_cd
    )
  ) {
    stop(
      paste0(
        "Nemenyi p-values and critical-difference decisions ",
        "are inconsistent for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  rank_table <- tibble(
    method_label =
      names(mean_ranks),
    
    mean_rank =
      as.numeric(mean_ranks)
  ) %>%
    left_join(
      config_catalog,
      by = "method_label"
    ) %>%
    arrange(
      mean_rank,
      method_label
    )
  
  if (
    nrow(rank_table) != 20L ||
    anyNA(
      rank_table$method_family
    )
  ) {
    stop(
      paste0(
        "Missing or incomplete configuration metadata for ",
        subset_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  list(
    subset =
      subset_name,
    
    metric =
      metric_col,
    
    alpha =
      alpha,
    
    friedman =
      friedman_result,
    
    rank_matrix =
      rank_matrix,
    
    mean_ranks =
      mean_ranks,
    
    rank_table =
      rank_table,
    
    nemenyi_pairwise =
      nemenyi_pairwise,
    
    critical_difference =
      critical_difference,
    
    n_series_total =
      n_series_total,
    
    n_series_informative =
      n_series_informative,
    
    n_series_invariant =
      n_series_invariant,
    
    n_methods =
      n_methods
  )
}

# ---------------------------------------------------------
# Identify overlapping non-significance sets
# ---------------------------------------------------------

find_overlapping_cd_groups <- function(
    rank_table,
    critical_difference) {
  
  required_columns <- c(
    "method_label",
    "mean_rank"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(rank_table)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Missing rank-table column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(critical_difference) ||
    length(critical_difference) != 1L ||
    is.na(critical_difference) ||
    critical_difference <= 0
  ) {
    stop(
      "critical_difference must be one positive numeric value.",
      call. = FALSE
    )
  }
  
  ordered_table <- rank_table %>%
    arrange(
      mean_rank,
      method_label
    ) %>%
    mutate(
      rank_position =
        row_number()
    )
  
  possible_groups <- purrr::map_dfr(
    seq_len(
      nrow(ordered_table)
    ),
    function(start_position) {
      
      valid_end_positions <- which(
        ordered_table$mean_rank -
          ordered_table$mean_rank[
            start_position
          ] <=
          critical_difference
      )
      
      valid_end_positions <-
        valid_end_positions[
          valid_end_positions >=
            start_position
        ]
      
      end_position <- max(
        valid_end_positions
      )
      
      tibble(
        start_position =
          start_position,
        
        end_position =
          end_position,
        
        group_size =
          end_position -
          start_position +
          1L,
        
        lower_rank =
          ordered_table$mean_rank[
            start_position
          ],
        
        upper_rank =
          ordered_table$mean_rank[
            end_position
          ]
      )
    }
  ) %>%
    filter(
      group_size >= 2L
    )
  
  if (nrow(possible_groups) == 0L) {
    return(
      tibble(
        group_id =
          integer(),
        
        start_position =
          integer(),
        
        end_position =
          integer(),
        
        group_size =
          integer(),
        
        lower_rank =
          double(),
        
        upper_rank =
          double(),
        
        members =
          character()
      )
    )
  }
  
  maximal_groups <- possible_groups %>%
    rowwise() %>%
    mutate(
      is_contained = any(
        possible_groups$start_position <=
          start_position &
          possible_groups$end_position >=
          end_position &
          (
            possible_groups$start_position <
              start_position |
              possible_groups$end_position >
              end_position
          )
      )
    ) %>%
    ungroup() %>%
    filter(
      !is_contained
    ) %>%
    distinct(
      start_position,
      end_position,
      .keep_all = TRUE
    ) %>%
    arrange(
      start_position,
      end_position
    ) %>%
    mutate(
      group_id =
        row_number(),
      
      members = map2_chr(
        start_position,
        end_position,
        function(start_value, end_value) {
          paste(
            ordered_table$method_label[
              start_value:end_value
            ],
            collapse = ", "
          )
        }
      )
    ) %>%
    select(
      group_id,
      start_position,
      end_position,
      group_size,
      lower_rank,
      upper_rank,
      members
    )
  
  if (
    any(
      maximal_groups$upper_rank -
      maximal_groups$lower_rank >
      critical_difference
    )
  ) {
    stop(
      "An invalid non-significance set was produced.",
      call. = FALSE
    )
  }
  
  maximal_groups
}

# ---------------------------------------------------------
# Build a critical-difference reference plot
# ---------------------------------------------------------

plot_friedman_cd <- function(
    result_object,
    title_text = result_object$subset) {
  
  plot_data <- result_object$rank_table %>%
    mutate(
      lower_cd_reference =
        mean_rank -
        result_object$critical_difference / 2,
      
      upper_cd_reference =
        mean_rank +
        result_object$critical_difference / 2
    )
  
  ggplot(
    plot_data,
    aes(
      x = mean_rank,
      y = reorder(
        method_label,
        -mean_rank
      ),
      color = method_family
    )
  ) +
    geom_segment(
      aes(
        x = lower_cd_reference,
        xend = upper_cd_reference,
        yend = reorder(
          method_label,
          -mean_rank
        )
      ),
      linewidth = 1.4,
      alpha = 0.35
    ) +
    geom_point(
      size = 3
    ) +
    scale_color_manual(
      values = wong_palette,
      drop = FALSE
    ) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Mean rank (lower is better)",
      y = NULL,
      color = "Family"
    ) +
    theme_pub(
      base_size = 10
    ) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
}

# ---------------------------------------------------------
# Run overall analyses
# ---------------------------------------------------------

friedman_overall_hard <- run_friedman_nemenyi(
  df = series_with_cp,
  metric_col = "hard_F1",
  subset_name = "Overall - Hard F1"
)

friedman_overall_soft <- run_friedman_nemenyi(
  df = series_with_cp,
  metric_col = "soft_F1",
  subset_name = "Overall - Soft F1"
)

# ---------------------------------------------------------
# Run regime-specific analyses
# ---------------------------------------------------------

friedman_regime_metric_grid <- tidyr::crossing(
  regime =
    regime_levels,
  
  metric_col = c(
    "hard_F1",
    "soft_F1"
  )
) %>%
  mutate(
    analysis = paste0(
      regime,
      "_",
      if_else(
        metric_col == "hard_F1",
        "hard",
        "soft"
      )
    ),
    
    metric_label = if_else(
      metric_col == "hard_F1",
      "Hard F1",
      "Soft F1"
    ),
    
    subset_name = paste0(
      "Regime ",
      regime,
      " - ",
      metric_label
    )
  )

friedman_results_by_regime <- pmap(
  friedman_regime_metric_grid,
  function(
    regime,
    metric_col,
    analysis,
    metric_label,
    subset_name) {
    
    regime_data <- series_with_cp %>%
      filter(
        as.character(regime) ==
          .env$regime
      )
    
    run_friedman_nemenyi(
      df = regime_data,
      metric_col = metric_col,
      subset_name = subset_name
    )
  }
)

names(friedman_results_by_regime) <-
  friedman_regime_metric_grid$analysis

friedman_nemenyi_results <- c(
  list(
    overall_hard =
      friedman_overall_hard,
    
    overall_soft =
      friedman_overall_soft
  ),
  friedman_results_by_regime
)

expected_analysis_names <- c(
  "overall_hard",
  "overall_soft",
  "mean_hard",
  "mean_soft",
  "trend_hard",
  "trend_soft",
  "variance_hard",
  "variance_soft"
)

if (
  length(friedman_nemenyi_results) != 8L ||
  !setequal(
    names(friedman_nemenyi_results),
    expected_analysis_names
  )
) {
  stop(
    paste0(
      "Expected eight Friedman-Nemenyi analysis objects, but found ",
      length(friedman_nemenyi_results),
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Non-significance sets
# ---------------------------------------------------------

friedman_non_significance_groups <- purrr::map(
  friedman_nemenyi_results,
  ~ find_overlapping_cd_groups(
    rank_table =
      .x$rank_table,
    critical_difference =
      .x$critical_difference
  )
)

# ---------------------------------------------------------
# Selected figure
# ---------------------------------------------------------
# Retain the overall hard-F1 critical-difference plot.

p_friedman_overall_hard <- plot_friedman_cd(
  result_object =
    friedman_overall_hard,
  title_text =
    friedman_overall_hard$subset
)

print(
  p_friedman_overall_hard
)

# ---------------------------------------------------------
# Summary
# ---------------------------------------------------------

friedman_nemenyi_summary <- purrr::imap_dfr(
  friedman_nemenyi_results,
  function(result_object, result_name) {
    
    tibble(
      analysis =
        result_name,
      
      subset =
        result_object$subset,
      
      n_series_total =
        result_object$n_series_total,
      
      n_series_informative =
        result_object$n_series_informative,
      
      n_series_invariant =
        result_object$n_series_invariant,
      
      n_methods =
        result_object$n_methods,
      
      friedman_chi_square =
        unname(
          result_object$friedman$statistic
        ),
      
      friedman_df =
        unname(
          result_object$friedman$parameter
        ),
      
      friedman_p_value =
        result_object$friedman$p.value,
      
      critical_difference =
        result_object$critical_difference,
      
      n_significant_nemenyi_pairs = sum(
        result_object$nemenyi_pairwise$
          significant
      )
    )
  }
)

if (
  nrow(friedman_nemenyi_summary) != 8L ||
  any(
    friedman_nemenyi_summary$n_methods != 20L
  ) ||
  any(
    friedman_nemenyi_summary$n_series_informative +
    friedman_nemenyi_summary$n_series_invariant !=
    friedman_nemenyi_summary$n_series_total
  ) ||
  any(
    friedman_nemenyi_summary$
    n_significant_nemenyi_pairs < 0L
  ) ||
  any(
    friedman_nemenyi_summary$
    n_significant_nemenyi_pairs > 190L
  )
) {
  stop(
    "The Friedman-Nemenyi summary is internally inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q23 - FRIEDMAN-NEMENYI ANALYSIS\n")
cat("=====================================\n")

print(
  friedman_nemenyi_summary,
  n = Inf,
  width = Inf
)


cat("\nSelected figure retained in:\n")
cat("  p_friedman_overall_hard\n")

cat("\nComplete results retained in:\n")
cat("  friedman_nemenyi_results\n")
cat("  friedman_non_significance_groups\n")
cat("  friedman_nemenyi_summary\n")

# =========================================================
# Q24 - BOOTSTRAP INTERVALS FOR MEAN RANKS
# =========================================================
# Informative series are resampled with replacement.
# Percentile intervals describe mean-rank stability and are not
# used for Friedman-Nemenyi pairwise inference.

mean_rank_bootstrap_seed <- 2025L

# ---------------------------------------------------------
# Bootstrap one mean-rank analysis
# ---------------------------------------------------------

bootstrap_mean_ranks <- function(
    result_object,
    n_boot,
    alpha = 0.05,
    seed = 2025L) {
  
  rank_matrix <- result_object$rank_matrix
  
  if (
    is.null(rank_matrix) ||
    !is.matrix(rank_matrix) ||
    !is.numeric(rank_matrix) ||
    anyNA(rank_matrix)
  ) {
    stop(
      paste0(
        "A complete numeric rank matrix is required for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  n_series <- nrow(
    rank_matrix
  )
  
  n_methods <- ncol(
    rank_matrix
  )
  
  if (
    n_series !=
    result_object$n_series_informative ||
    n_methods !=
    result_object$n_methods
  ) {
    stop(
      paste0(
        "Rank-matrix dimensions do not match the validated ",
        "Friedman-Nemenyi result for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    is.null(
      colnames(rank_matrix)
    ) ||
    any(
      colnames(rank_matrix) == ""
    ) ||
    anyDuplicated(
      colnames(rank_matrix)
    ) > 0L
  ) {
    stop(
      paste0(
        "Missing or duplicated configuration names in the ",
        "rank matrix for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    !setequal(
      colnames(rank_matrix),
      config_catalog$method_label
    )
  ) {
    stop(
      paste0(
        "The rank-matrix configurations do not match the ",
        "validated configuration catalog for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(n_boot) ||
    length(n_boot) != 1L ||
    is.na(n_boot) ||
    n_boot < 1 ||
    n_boot != as.integer(n_boot)
  ) {
    stop(
      "n_boot must be one positive integer.",
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(alpha) ||
    length(alpha) != 1L ||
    is.na(alpha) ||
    alpha <= 0 ||
    alpha >= 1
  ) {
    stop(
      "alpha must be one numeric value between zero and one.",
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(seed) ||
    length(seed) != 1L ||
    is.na(seed) ||
    seed != as.integer(seed)
  ) {
    stop(
      "seed must be one integer value.",
      call. = FALSE
    )
  }
  
  set.seed(
    seed
  )
  
  bootstrap_rank_matrix <- replicate(
    n_boot,
    {
      sampled_rows <- sample.int(
        n = n_series,
        size = n_series,
        replace = TRUE
      )
      
      colMeans(
        rank_matrix[
          sampled_rows,
          ,
          drop = FALSE
        ]
      )
    }
  )
  
  if (
    nrow(bootstrap_rank_matrix) !=
    n_methods ||
    ncol(bootstrap_rank_matrix) !=
    n_boot ||
    anyNA(bootstrap_rank_matrix)
  ) {
    stop(
      paste0(
        "Unexpected bootstrap dimensions or missing values for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  rownames(bootstrap_rank_matrix) <-
    colnames(rank_matrix)
  
  point_estimate <- colMeans(
    rank_matrix
  )
  
  bootstrap_result <- tibble(
    method_label =
      names(point_estimate),
    
    mean_rank =
      as.numeric(point_estimate),
    
    bootstrap_standard_error = apply(
      bootstrap_rank_matrix,
      1,
      sd
    ),
    
    bootstrap_lower = apply(
      bootstrap_rank_matrix,
      1,
      quantile,
      probs = alpha / 2,
      names = FALSE,
      type = 7
    ),
    
    bootstrap_upper = apply(
      bootstrap_rank_matrix,
      1,
      quantile,
      probs = 1 - alpha / 2,
      names = FALSE,
      type = 7
    )
  ) %>%
    left_join(
      config_catalog,
      by = "method_label"
    ) %>%
    mutate(
      subset =
        result_object$subset,
      
      metric =
        result_object$metric,
      
      n_series_total =
        result_object$n_series_total,
      
      n_series_informative =
        result_object$n_series_informative,
      
      n_series_invariant =
        result_object$n_series_invariant,
      
      n_boot =
        as.integer(n_boot),
      
      confidence_level =
        100 *
        (1 - alpha),
      
      bootstrap_seed =
        as.integer(seed),
      
      mean_rank_ci_label = sprintf(
        "%.2f [%.2f, %.2f]",
        mean_rank,
        bootstrap_lower,
        bootstrap_upper
      )
    ) %>%
    arrange(
      mean_rank,
      method_label
    )
  
  if (
    nrow(bootstrap_result) !=
    n_methods ||
    anyNA(
      bootstrap_result$method_family
    ) ||
    anyNA(
      bootstrap_result$bootstrap_standard_error
    ) ||
    anyNA(
      bootstrap_result$bootstrap_lower
    ) ||
    anyNA(
      bootstrap_result$bootstrap_upper
    )
  ) {
    stop(
      paste0(
        "Incomplete bootstrap results were produced for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      bootstrap_result$bootstrap_standard_error < 0
    ) ||
    any(
      bootstrap_result$bootstrap_lower >
      bootstrap_result$bootstrap_upper
    )
  ) {
    stop(
      paste0(
        "Invalid bootstrap intervals were produced for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  rank_validation <- bootstrap_result %>%
    select(
      method_label,
      observed_mean_rank =
        mean_rank
    ) %>%
    left_join(
      result_object$rank_table %>%
        select(
          method_label,
          expected_mean_rank =
            mean_rank
        ),
      by = "method_label"
    )
  
  if (
    nrow(rank_validation) !=
    n_methods ||
    anyNA(
      rank_validation$expected_mean_rank
    ) ||
    any(
      abs(
        rank_validation$observed_mean_rank -
        rank_validation$expected_mean_rank
      ) > 1e-12
    )
  ) {
    stop(
      paste0(
        "Bootstrap point estimates do not reproduce the ",
        "validated mean ranks for ",
        result_object$subset,
        "."
      ),
      call. = FALSE
    )
  }
  
  list(
    subset =
      result_object$subset,
    
    metric =
      result_object$metric,
    
    seed =
      as.integer(seed),
    
    n_boot =
      as.integer(n_boot),
    
    alpha =
      alpha,
    
    bootstrap_rank_matrix =
      bootstrap_rank_matrix,
    
    rank_table =
      bootstrap_result
  )
}

# ---------------------------------------------------------
# Run bootstrap analyses
# ---------------------------------------------------------

if (run_bootstrap) {
  
  if (
    !exists(
      "friedman_nemenyi_results"
    ) ||
    !is.list(
      friedman_nemenyi_results
    ) ||
    length(
      friedman_nemenyi_results
    ) != 8L
  ) {
    stop(
      paste0(
        "The bootstrap analysis requires the eight validated ",
        "analyses stored in friedman_nemenyi_results."
      ),
      call. = FALSE
    )
  }
  
  expected_analysis_names <- c(
    "overall_hard",
    "overall_soft",
    "mean_hard",
    "mean_soft",
    "trend_hard",
    "trend_soft",
    "variance_hard",
    "variance_soft"
  )
  
  if (
    !setequal(
      names(
        friedman_nemenyi_results
      ),
      expected_analysis_names
    )
  ) {
    stop(
      paste0(
        "The Friedman-Nemenyi result list does not contain ",
        "the eight expected analyses."
      ),
      call. = FALSE
    )
  }
  
  mean_rank_bootstrap_results <- purrr::map(
    friedman_nemenyi_results,
    ~ bootstrap_mean_ranks(
      result_object = .x,
      n_boot = n_boot,
      alpha = 0.05,
      seed = mean_rank_bootstrap_seed
    )
  )
  
  if (
    length(
      mean_rank_bootstrap_results
    ) !=
    length(
      friedman_nemenyi_results
    )
  ) {
    stop(
      "The bootstrap procedure did not return all expected analyses.",
      call. = FALSE
    )
  }
  
  mean_rank_bootstrap_summary <- purrr::imap_dfr(
    mean_rank_bootstrap_results,
    function(
    bootstrap_result,
    result_name) {
      
      bootstrap_result$rank_table %>%
        mutate(
          analysis =
            result_name,
          .before = 1
        ) %>%
        select(
          analysis,
          subset,
          metric,
          method_label,
          method_family,
          mean_rank,
          bootstrap_standard_error,
          bootstrap_lower,
          bootstrap_upper,
          mean_rank_ci_label,
          n_series_total,
          n_series_informative,
          n_series_invariant,
          n_boot,
          confidence_level,
          bootstrap_seed
        )
    }
  )
  
  expected_bootstrap_rows <-
    length(
      friedman_nemenyi_results
    ) *
    20L
  
  if (
    nrow(
      mean_rank_bootstrap_summary
    ) !=
    expected_bootstrap_rows
  ) {
    stop(
      paste0(
        "Expected ",
        expected_bootstrap_rows,
        " configuration-level bootstrap results, but found ",
        nrow(
          mean_rank_bootstrap_summary
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      mean_rank_bootstrap_summary$n_boot !=
      n_boot
    ) ||
    any(
      mean_rank_bootstrap_summary$bootstrap_seed !=
      mean_rank_bootstrap_seed
    ) ||
    any(
      mean_rank_bootstrap_summary$confidence_level !=
      95
    )
  ) {
    stop(
      "Inconsistent bootstrap settings were found.",
      call. = FALSE
    )
  }
  
  if (
    any(
      mean_rank_bootstrap_summary$
      n_series_informative +
      mean_rank_bootstrap_summary$
      n_series_invariant !=
      mean_rank_bootstrap_summary$
      n_series_total
    )
  ) {
    stop(
      paste0(
        "Informative and invariant series counts are ",
        "inconsistent in the bootstrap summary."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      mean_rank_bootstrap_summary$
      bootstrap_lower >
      mean_rank_bootstrap_summary$
      bootstrap_upper
    )
  ) {
    stop(
      "Invalid percentile intervals were found.",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------
  # Overall and regime-specific tables
  # -------------------------------------------------------
  
  mean_rank_bootstrap_overall_table <-
    mean_rank_bootstrap_summary %>%
    filter(
      analysis %in% c(
        "overall_hard",
        "overall_soft"
      )
    ) %>%
    arrange(
      analysis,
      mean_rank,
      method_label
    )
  
  mean_rank_bootstrap_regime_table <-
    mean_rank_bootstrap_summary %>%
    filter(
      !analysis %in% c(
        "overall_hard",
        "overall_soft"
      )
    ) %>%
    arrange(
      analysis,
      mean_rank,
      method_label
    )
  
  if (
    nrow(
      mean_rank_bootstrap_overall_table
    ) != 40L ||
    nrow(
      mean_rank_bootstrap_regime_table
    ) != 120L
  ) {
    stop(
      paste0(
        "The overall and regime-specific bootstrap tables ",
        "have unexpected dimensions."
      ),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------
  # Mean-rank summary table
  # -------------------------------------------------------
  
  mean_rank_bootstrap_summary_table <-
    mean_rank_bootstrap_summary %>%
    transmute(
      analysis,
      subset,
      metric,
      method_label,
      method_family,
      mean_rank,
      bootstrap_lower,
      bootstrap_upper,
      mean_rank_ci_label,
      bootstrap_standard_error,
      n_series_informative
    ) %>%
    arrange(
      analysis,
      mean_rank,
      method_label
    )
  
  # -------------------------------------------------------
  # Output
  # -------------------------------------------------------
  
  cat("\n=====================================\n")
  cat("Q24 - BOOTSTRAP INTERVALS FOR MEAN RANKS\n")
  cat("=====================================\n")
  cat("Bootstrap replicates:", n_boot, "\n")
  cat("Confidence level: 95%\n")
  cat("Bootstrap seed:", mean_rank_bootstrap_seed, "\n")
  cat("Population: informative series from Q23\n")
  cat("Interpretation: rank stability, not pairwise significance\n")
  
  cat("\n--- Overall mean-rank intervals ---\n")
  
  print(
    mean_rank_bootstrap_overall_table %>%
      select(
        analysis,
        method_label,
        method_family,
        mean_rank_ci_label,
        bootstrap_standard_error,
        n_series_informative
      ),
    n = Inf,
    width = Inf
  )
  
  cat("\n--- Regime-specific mean-rank intervals ---\n")
  
  print(
    mean_rank_bootstrap_regime_table %>%
      select(
        analysis,
        method_label,
        method_family,
        mean_rank_ci_label,
        bootstrap_standard_error,
        n_series_informative
      ),
    n = Inf,
    width = Inf
  )
  
  cat("\nMean-rank summary table retained in:\n")
  cat("  mean_rank_bootstrap_summary_table\n")
  
  cat("\nComplete bootstrap results retained in:\n")
  cat("  mean_rank_bootstrap_results\n")
  cat("  mean_rank_bootstrap_summary\n")
  
} else {
  
  mean_rank_bootstrap_results <- NULL
  mean_rank_bootstrap_summary <- NULL
  mean_rank_bootstrap_overall_table <- NULL
  mean_rank_bootstrap_regime_table <- NULL
  mean_rank_bootstrap_summary_table <- NULL
  
  cat("\n=====================================\n")
  cat("Q24 - BOOTSTRAP INTERVALS FOR MEAN RANKS\n")
  cat("=====================================\n")
  cat("Skipped: run_bootstrap = FALSE\n")
}

# =========================================================
# Q25 - PAIRED WILCOXON TESTS ON SHARED POSITIVE-F1 SERIES
# =========================================================
# Each configuration pair is compared only on series where
# both configurations obtain F1 > 0.
# Significance is determined by the paired Wilcoxon test.
# Direction is assigned from the mean paired difference.

shared_positive_configuration_levels <- config_catalog %>%
  pull(
    method_label
  )

if (
  length(
    shared_positive_configuration_levels
  ) != 20L ||
  anyDuplicated(
    shared_positive_configuration_levels
  ) > 0L
) {
  stop(
    paste0(
      "Expected 20 unique configurations, but found ",
      length(
        shared_positive_configuration_levels
      ),
      "."
    ),
    call. = FALSE
  )
}

shared_positive_configuration_pairs <- combn(
  shared_positive_configuration_levels,
  2,
  simplify = FALSE
) %>%
  map_dfr(
    ~ tibble(
      configuration_a =
        .x[[1]],
      
      configuration_b =
        .x[[2]]
    )
  )

if (
  nrow(
    shared_positive_configuration_pairs
  ) != 190L
) {
  stop(
    paste0(
      "Expected 190 unique configuration pairs, but found ",
      nrow(
        shared_positive_configuration_pairs
      ),
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Run one paired comparison
# ---------------------------------------------------------

run_shared_positive_wilcoxon <- function(
    df,
    configuration_a,
    configuration_b,
    metric_col,
    metric_label) {
  
  validate_metric(
    df,
    metric_col
  )
  
  paired_data <- df %>%
    filter(
      method_label %in% c(
        configuration_a,
        configuration_b
      )
    ) %>%
    select(
      serie_id,
      method_label,
      metric_value =
        all_of(metric_col)
    ) %>%
    pivot_wider(
      names_from =
        method_label,
      values_from =
        metric_value
    ) %>%
    arrange(
      serie_id
    )
  
  expected_series <- n_distinct(
    df$serie_id
  )
  
  if (
    expected_series !=
    n_series_overall
  ) {
    stop(
      paste0(
        "Expected ",
        n_series_overall,
        " series with reference change-points, but found ",
        expected_series,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    nrow(paired_data) !=
    expected_series
  ) {
    stop(
      paste0(
        "Incomplete pair for ",
        configuration_a,
        " versus ",
        configuration_b,
        " under ",
        metric_label,
        ". Expected ",
        expected_series,
        " paired series but found ",
        nrow(paired_data),
        "."
      ),
      call. = FALSE
    )
  }
  
  value_a <- paired_data[[
    configuration_a
  ]]
  
  value_b <- paired_data[[
    configuration_b
  ]]
  
  if (
    is.null(value_a) ||
    is.null(value_b) ||
    anyNA(value_a) ||
    anyNA(value_b)
  ) {
    stop(
      paste0(
        "Missing standardized F1 values for ",
        configuration_a,
        " versus ",
        configuration_b,
        " under ",
        metric_label,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      value_a < 0 |
      value_a > 1
    ) ||
    any(
      value_b < 0 |
      value_b > 1
    )
  ) {
    stop(
      paste0(
        "F1 values must remain within [0, 1] for ",
        configuration_a,
        " versus ",
        configuration_b,
        " under ",
        metric_label,
        "."
      ),
      call. = FALSE
    )
  }
  
  shared_positive_data <- tibble(
    serie_id =
      paired_data$serie_id,
    
    value_a =
      value_a,
    
    value_b =
      value_b
  ) %>%
    filter(
      value_a > 0,
      value_b > 0
    ) %>%
    mutate(
      paired_difference =
        value_a -
        value_b
    )
  
  n_common <- nrow(
    shared_positive_data
  )
  
  n_a_higher <- sum(
    shared_positive_data$
      paired_difference > 0
  )
  
  n_tied <- sum(
    shared_positive_data$
      paired_difference == 0
  )
  
  n_b_higher <- sum(
    shared_positive_data$
      paired_difference < 0
  )
  
  n_nonzero_differences <- sum(
    shared_positive_data$
      paired_difference != 0
  )
  
  if (
    n_a_higher +
    n_tied +
    n_b_higher !=
    n_common
  ) {
    stop(
      paste0(
        "Direction counts are inconsistent for ",
        configuration_a,
        " versus ",
        configuration_b,
        " under ",
        metric_label,
        "."
      ),
      call. = FALSE
    )
  }
  
  test_status <- case_when(
    n_common == 0L ~
      "No shared positive-F1 series",
    
    n_nonzero_differences == 0L ~
      "All shared-positive differences tied",
    
    TRUE ~
      "Test performed"
  )
  
  if (
    test_status ==
    "Test performed"
  ) {
    
    wilcoxon_result <- suppressWarnings(
      wilcox.test(
        shared_positive_data$value_a,
        shared_positive_data$value_b,
        paired = TRUE,
        alternative = "two.sided",
        exact = FALSE,
        correct = TRUE
      )
    )
    
    wilcoxon_statistic <- unname(
      wilcoxon_result$statistic
    )
    
    p_value <-
      wilcoxon_result$p.value
    
  } else {
    
    wilcoxon_statistic <-
      NA_real_
    
    p_value <-
      NA_real_
  }
  
  if (n_common > 0L) {
    
    mean_a <- mean(
      shared_positive_data$value_a
    )
    
    mean_b <- mean(
      shared_positive_data$value_b
    )
    
    median_a <- median(
      shared_positive_data$value_a
    )
    
    median_b <- median(
      shared_positive_data$value_b
    )
    
    mean_paired_difference <- mean(
      shared_positive_data$
        paired_difference
    )
    
    median_paired_difference <- median(
      shared_positive_data$
        paired_difference
    )
    
  } else {
    
    mean_a <- NA_real_
    mean_b <- NA_real_
    median_a <- NA_real_
    median_b <- NA_real_
    mean_paired_difference <- NA_real_
    median_paired_difference <- NA_real_
  }
  
  tibble(
    metric =
      metric_label,
    
    metric_col =
      metric_col,
    
    configuration_a =
      configuration_a,
    
    configuration_b =
      configuration_b,
    
    n_total_series =
      expected_series,
    
    n_common =
      n_common,
    
    n_nonzero_differences =
      n_nonzero_differences,
    
    n_a_higher =
      n_a_higher,
    
    n_tied =
      n_tied,
    
    n_b_higher =
      n_b_higher,
    
    mean_a =
      mean_a,
    
    mean_b =
      mean_b,
    
    median_a =
      median_a,
    
    median_b =
      median_b,
    
    mean_paired_difference =
      mean_paired_difference,
    
    median_paired_difference =
      median_paired_difference,
    
    wilcoxon_statistic =
      wilcoxon_statistic,
    
    p_value =
      p_value,
    
    test_status =
      test_status
  )
}

# ---------------------------------------------------------
# Run all comparisons for one matching criterion
# ---------------------------------------------------------

run_shared_positive_wilcoxon_for_metric <- function(
    df,
    metric_col,
    metric_label) {
  
  pairwise_results <-
    shared_positive_configuration_pairs %>%
    pmap_dfr(
      function(
    configuration_a,
    configuration_b) {
        
        run_shared_positive_wilcoxon(
          df = df,
          configuration_a =
            configuration_a,
          configuration_b =
            configuration_b,
          metric_col =
            metric_col,
          metric_label =
            metric_label
        )
      }
    )
  
  if (
    nrow(pairwise_results) !=
    190L
  ) {
    stop(
      paste0(
        "Expected 190 results for ",
        metric_label,
        ", but found ",
        nrow(pairwise_results),
        "."
      ),
      call. = FALSE
    )
  }
  
  pairwise_results %>%
    mutate(
      test_performed =
        test_status ==
        "Test performed",
      
      p_bonferroni = p.adjust(
        p_value,
        method = "bonferroni",
        n = 190
      ),
      
      p_holm = p.adjust(
        p_value,
        method = "holm",
        n = 190
      ),
      
      significant_bonferroni_005 =
        !is.na(p_bonferroni) &
        p_bonferroni < 0.05,
      
      significant_bonferroni_001 =
        !is.na(p_bonferroni) &
        p_bonferroni < 0.01,
      
      significant_holm_005 =
        !is.na(p_holm) &
        p_holm < 0.05
    ) %>%
    arrange(
      configuration_a,
      configuration_b
    )
}

# ---------------------------------------------------------
# Run hard- and soft-F1 comparisons
# ---------------------------------------------------------

pairwise_wilcoxon_hard <-
  run_shared_positive_wilcoxon_for_metric(
    df = series_with_cp,
    metric_col = "hard_F1",
    metric_label = "Hard F1"
  )

pairwise_wilcoxon_soft <-
  run_shared_positive_wilcoxon_for_metric(
    df = series_with_cp,
    metric_col = "soft_F1",
    metric_label = "Soft F1"
  )

pairwise_shared_positive_wilcoxon <- bind_rows(
  pairwise_wilcoxon_hard,
  pairwise_wilcoxon_soft
)

if (
  nrow(
    pairwise_shared_positive_wilcoxon
  ) != 380L
) {
  stop(
    paste0(
      "Expected 380 metric-specific results, but found ",
      nrow(
        pairwise_shared_positive_wilcoxon
      ),
      "."
    ),
    call. = FALSE
  )
}

if (
  any(
    pairwise_shared_positive_wilcoxon$
    n_total_series !=
    n_series_overall
  ) ||
  any(
    pairwise_shared_positive_wilcoxon$
    n_common < 0L
  ) ||
  any(
    pairwise_shared_positive_wilcoxon$
    n_common >
    n_series_overall
  )
) {
  stop(
    "Invalid total-series or shared-positive-series counts were found.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Validate test availability
# ---------------------------------------------------------

if (
  any(
    pairwise_shared_positive_wilcoxon$
    test_performed &
    (
      is.na(
        pairwise_shared_positive_wilcoxon$
        wilcoxon_statistic
      ) |
      is.na(
        pairwise_shared_positive_wilcoxon$
        p_value
      )
    )
  ) ||
  any(
    !pairwise_shared_positive_wilcoxon$
    test_performed &
    (
      !is.na(
        pairwise_shared_positive_wilcoxon$
        wilcoxon_statistic
      ) |
      !is.na(
        pairwise_shared_positive_wilcoxon$
        p_value
      )
    )
  )
) {
  stop(
    "Wilcoxon results are inconsistent with the test status.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Expand pairwise results into directed comparisons
# ---------------------------------------------------------

directed_shared_positive_a <-
  pairwise_shared_positive_wilcoxon %>%
  transmute(
    metric,
    
    configuration =
      configuration_a,
    
    competitor =
      configuration_b,
    
    n_common,
    n_nonzero_differences,
    
    mean_configuration =
      mean_a,
    
    mean_competitor =
      mean_b,
    
    median_configuration =
      median_a,
    
    median_competitor =
      median_b,
    
    mean_difference =
      mean_paired_difference,
    
    median_difference =
      median_paired_difference,
    
    n_configuration_higher =
      n_a_higher,
    
    n_tied,
    
    n_configuration_lower =
      n_b_higher,
    
    test_status,
    test_performed,
    p_value,
    p_bonferroni,
    p_holm,
    significant_bonferroni_005,
    significant_bonferroni_001,
    significant_holm_005
  )

directed_shared_positive_b <-
  pairwise_shared_positive_wilcoxon %>%
  transmute(
    metric,
    
    configuration =
      configuration_b,
    
    competitor =
      configuration_a,
    
    n_common,
    n_nonzero_differences,
    
    mean_configuration =
      mean_b,
    
    mean_competitor =
      mean_a,
    
    median_configuration =
      median_b,
    
    median_competitor =
      median_a,
    
    mean_difference =
      -mean_paired_difference,
    
    median_difference =
      -median_paired_difference,
    
    n_configuration_higher =
      n_b_higher,
    
    n_tied,
    
    n_configuration_lower =
      n_a_higher,
    
    test_status,
    test_performed,
    p_value,
    p_bonferroni,
    p_holm,
    significant_bonferroni_005,
    significant_bonferroni_001,
    significant_holm_005
  )

directed_shared_positive_wilcoxon <- bind_rows(
  directed_shared_positive_a,
  directed_shared_positive_b
) %>%
  mutate(
    significant_win_bonferroni_005 =
      significant_bonferroni_005 &
      mean_difference > 0,
    
    significant_loss_bonferroni_005 =
      significant_bonferroni_005 &
      mean_difference < 0,
    
    significant_win_bonferroni_001 =
      significant_bonferroni_001 &
      mean_difference > 0,
    
    significant_loss_bonferroni_001 =
      significant_bonferroni_001 &
      mean_difference < 0,
    
    significant_win_holm_005 =
      significant_holm_005 &
      mean_difference > 0,
    
    significant_loss_holm_005 =
      significant_holm_005 &
      mean_difference < 0
  )

if (
  nrow(
    directed_shared_positive_wilcoxon
  ) != 760L
) {
  stop(
    paste0(
      "Expected 760 directed results, but found ",
      nrow(
        directed_shared_positive_wilcoxon
      ),
      "."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Configuration-level significant-win ranking
# ---------------------------------------------------------

shared_positive_wilcoxon_ranking <-
  directed_shared_positive_wilcoxon %>%
  group_by(
    metric,
    configuration
  ) %>%
  summarise(
    n_possible_comparisons =
      n(),
    
    n_testable_comparisons = sum(
      test_performed
    ),
    
    n_unavailable_comparisons =
      n_possible_comparisons -
      n_testable_comparisons,
    
    minimum_shared_series = if (
      any(test_performed)
    ) {
      min(
        n_common[
          test_performed
        ]
      )
    } else {
      NA_integer_
    },
    
    median_shared_series = if (
      any(test_performed)
    ) {
      median(
        n_common[
          test_performed
        ]
      )
    } else {
      NA_real_
    },
    
    maximum_shared_series = if (
      any(test_performed)
    ) {
      max(
        n_common[
          test_performed
        ]
      )
    } else {
      NA_integer_
    },
    
    n_significant_wins_bonferroni_005 = sum(
      significant_win_bonferroni_005
    ),
    
    n_significant_losses_bonferroni_005 = sum(
      significant_loss_bonferroni_005
    ),
    
    n_significant_wins_bonferroni_001 = sum(
      significant_win_bonferroni_001
    ),
    
    n_significant_losses_bonferroni_001 = sum(
      significant_loss_bonferroni_001
    ),
    
    n_significant_wins_holm_005 = sum(
      significant_win_holm_005
    ),
    
    n_significant_losses_holm_005 = sum(
      significant_loss_holm_005
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    significant_win_rate_bonferroni_005 = round(
      100 *
        n_significant_wins_bonferroni_005 /
        n_possible_comparisons,
      1
    ),
    
    significant_win_rate_bonferroni_001 = round(
      100 *
        n_significant_wins_bonferroni_001 /
        n_possible_comparisons,
      1
    ),
    
    significant_win_rate_holm_005 = round(
      100 *
        n_significant_wins_holm_005 /
        n_possible_comparisons,
      1
    )
  ) %>%
  left_join(
    config_catalog %>%
      rename(
        configuration =
          method_label
      ),
    by = "configuration"
  ) %>%
  arrange(
    metric,
    desc(
      n_significant_wins_bonferroni_005
    ),
    n_significant_losses_bonferroni_005,
    configuration
  )

if (
  nrow(
    shared_positive_wilcoxon_ranking
  ) != 40L ||
  any(
    shared_positive_wilcoxon_ranking$
    n_possible_comparisons !=
    19L
  )
) {
  stop(
    paste0(
      "Each of the 20 configurations must have ",
      "19 comparisons under each F1 criterion."
    ),
    call. = FALSE
  )
}

if (
  anyNA(
    shared_positive_wilcoxon_ranking$
    method_family
  ) ||
  any(
    shared_positive_wilcoxon_ranking$
    n_testable_comparisons +
    shared_positive_wilcoxon_ranking$
    n_unavailable_comparisons !=
    shared_positive_wilcoxon_ranking$
    n_possible_comparisons
  )
) {
  stop(
    "The configuration-level Wilcoxon ranking is inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Metric-level summary
# ---------------------------------------------------------

shared_positive_wilcoxon_summary <-
  pairwise_shared_positive_wilcoxon %>%
  group_by(
    metric
  ) %>%
  summarise(
    n_unique_pairs =
      n(),
    
    n_tests_performed = sum(
      test_performed
    ),
    
    n_unavailable_pairs = sum(
      !test_performed
    ),
    
    n_significant_pairs_bonferroni_005 = sum(
      significant_bonferroni_005
    ),
    
    n_significant_pairs_bonferroni_001 = sum(
      significant_bonferroni_001
    ),
    
    n_significant_pairs_holm_005 = sum(
      significant_holm_005
    ),
    
    minimum_n_common = min(
      n_common
    ),
    
    median_n_common = median(
      n_common
    ),
    
    maximum_n_common = max(
      n_common
    ),
    
    .groups = "drop"
  )

if (
  nrow(
    shared_positive_wilcoxon_summary
  ) != 2L ||
  any(
    shared_positive_wilcoxon_summary$
    n_unique_pairs !=
    190L
  ) ||
  any(
    shared_positive_wilcoxon_summary$
    n_tests_performed +
    shared_positive_wilcoxon_summary$
    n_unavailable_pairs !=
    190L
  )
) {
  stop(
    "The metric-level Wilcoxon summary is internally inconsistent.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Validate significant-comparison counts
# ---------------------------------------------------------

shared_positive_direction_validation <-
  shared_positive_wilcoxon_ranking %>%
  group_by(
    metric
  ) %>%
  summarise(
    total_wins_bonferroni_005 = sum(
      n_significant_wins_bonferroni_005
    ),
    
    total_losses_bonferroni_005 = sum(
      n_significant_losses_bonferroni_005
    ),
    
    total_wins_bonferroni_001 = sum(
      n_significant_wins_bonferroni_001
    ),
    
    total_losses_bonferroni_001 = sum(
      n_significant_losses_bonferroni_001
    ),
    
    total_wins_holm_005 = sum(
      n_significant_wins_holm_005
    ),
    
    total_losses_holm_005 = sum(
      n_significant_losses_holm_005
    ),
    
    .groups = "drop"
  ) %>%
  left_join(
    shared_positive_wilcoxon_summary %>%
      select(
        metric,
        n_significant_pairs_bonferroni_005,
        n_significant_pairs_bonferroni_001,
        n_significant_pairs_holm_005
      ),
    by = "metric"
  )

invalid_shared_positive_direction_counts <-
  shared_positive_direction_validation %>%
  filter(
    total_wins_bonferroni_005 !=
      n_significant_pairs_bonferroni_005 |
      
      total_losses_bonferroni_005 !=
      n_significant_pairs_bonferroni_005 |
      
      total_wins_bonferroni_001 !=
      n_significant_pairs_bonferroni_001 |
      
      total_losses_bonferroni_001 !=
      n_significant_pairs_bonferroni_001 |
      
      total_wins_holm_005 !=
      n_significant_pairs_holm_005 |
      
      total_losses_holm_005 !=
      n_significant_pairs_holm_005
  )

if (
  nrow(
    invalid_shared_positive_direction_counts
  ) > 0L
) {
  stop(
    paste0(
      "Significant-win and significant-loss counts are ",
      "inconsistent with the significant-pair counts."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Validate significant-comparison directions
# ---------------------------------------------------------

significant_shared_positive_zero_mean <-
  pairwise_shared_positive_wilcoxon %>%
  filter(
    significant_bonferroni_005 |
      significant_bonferroni_001 |
      significant_holm_005,
    
    mean_paired_difference == 0
  )

if (
  nrow(
    significant_shared_positive_zero_mean
  ) > 0L
) {
  stop(
    paste0(
      "Significant comparisons with zero mean paired ",
      "difference were found."
    ),
    call. = FALSE
  )
}

shared_positive_direction_consistency <-
  pairwise_shared_positive_wilcoxon %>%
  filter(
    significant_bonferroni_005
  ) %>%
  group_by(
    metric
  ) %>%
  summarise(
    n_significant_pairs =
      n(),
    
    n_positive_direction = sum(
      mean_paired_difference > 0
    ),
    
    n_negative_direction = sum(
      mean_paired_difference < 0
    ),
    
    n_zero_direction = sum(
      mean_paired_difference == 0
    ),
    
    .groups = "drop"
  )

if (
  any(
    shared_positive_direction_consistency$
    n_positive_direction +
    shared_positive_direction_consistency$
    n_negative_direction !=
    shared_positive_direction_consistency$
    n_significant_pairs
  ) ||
  any(
    shared_positive_direction_consistency$
    n_zero_direction != 0L
  )
) {
  stop(
    "Inconsistent directions were found for significant pairs.",
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Configuration ranking table
# ---------------------------------------------------------

shared_positive_wilcoxon_summary_table <-
  shared_positive_wilcoxon_ranking %>%
  transmute(
    metric,
    configuration,
    method_family,
    
    significant_wins_bonferroni_005 =
      n_significant_wins_bonferroni_005,
    
    significant_losses_bonferroni_005 =
      n_significant_losses_bonferroni_005,
    
    significant_win_rate_bonferroni_005,
    
    significant_wins_bonferroni_001 =
      n_significant_wins_bonferroni_001,
    
    significant_wins_holm_005 =
      n_significant_wins_holm_005,
    
    minimum_shared_series,
    median_shared_series,
    maximum_shared_series
  ) %>%
  arrange(
    metric,
    desc(
      significant_wins_bonferroni_005
    ),
    significant_losses_bonferroni_005,
    configuration
  )

# ---------------------------------------------------------
# Output
# ---------------------------------------------------------

cat("\n=====================================\n")
cat("Q25 - PAIRED WILCOXON TESTS ON SHARED POSITIVE-F1 SERIES\n")
cat("=====================================\n")
cat("Configuration pairs per criterion: 190\n")
cat("Alternative: two-sided\n")
cat("Primary correction: Bonferroni across 190 pairs\n")
cat("Sensitivity: Bonferroni alpha = 0.01; Holm alpha = 0.05\n")

cat("\n--- Metric-level summary ---\n")

print(
  shared_positive_wilcoxon_summary,
  n = Inf,
  width = Inf
)

cat("\n--- Configuration ranking summary ---\n")

print(
  shared_positive_wilcoxon_summary_table,
  n = Inf,
  width = Inf
)

cat("\nComplete results retained in:\n")
cat("  pairwise_shared_positive_wilcoxon\n")
cat("  directed_shared_positive_wilcoxon\n")
cat("  shared_positive_wilcoxon_ranking\n")
cat("  shared_positive_wilcoxon_summary\n")
cat("  shared_positive_wilcoxon_summary_table\n")

if (!run_bootstrap) {
  stop(
    paste0(
      "The short-list summary requires ",
      "run_bootstrap = TRUE."
    ),
    call. = FALSE
  )
}

# =========================================================
# SHORT-LIST SUMMARY
# =========================================================

short_list_configurations <- c(
  "Joinpoint(F)",
  "MFEF+JP(F,12,0.95)"
)

short_list_evidence <- consolidated_performance_table %>%
  filter(
    method_label %in%
      short_list_configurations
  ) %>%
  transmute(
    configuration =
      method_label,
    
    method_family,
    
    hard_positive_coverage =
      n_positive_hard,
    
    hard_positive_coverage_pct = round(
      100 *
        n_positive_hard /
        n_series_overall,
      1
    ),
    
    hard_winner_assignments =
      n_wins_hard,
    
    soft_winner_assignments =
      n_wins_soft,
    
    conditional_hard_f1 =
      conditional_hard_median,
    
    conditional_hard_lower =
      conditional_hard_lower,
    
    conditional_hard_upper =
      conditional_hard_upper,
    
    conditional_soft_f1 =
      conditional_soft_median,
    
    conditional_soft_lower =
      conditional_soft_lower,
    
    conditional_soft_upper =
      conditional_soft_upper
  ) %>%
  left_join(
    shared_positive_wilcoxon_ranking %>%
      filter(
        metric ==
          "Hard F1",
        configuration %in%
          short_list_configurations
      ) %>%
      transmute(
        configuration,
        
        hard_significant_wins =
          n_significant_wins_bonferroni_005,
        
        hard_significant_losses =
          n_significant_losses_bonferroni_005,
        
        hard_significant_win_rate =
          significant_win_rate_bonferroni_005
      ),
    by = "configuration"
  ) %>%
  left_join(
    shared_positive_wilcoxon_ranking %>%
      filter(
        metric ==
          "Soft F1",
        configuration %in%
          short_list_configurations
      ) %>%
      transmute(
        configuration,
        
        soft_significant_wins =
          n_significant_wins_bonferroni_005,
        
        soft_significant_losses =
          n_significant_losses_bonferroni_005,
        
        soft_significant_win_rate =
          significant_win_rate_bonferroni_005
      ),
    by = "configuration"
  ) %>%
  left_join(
    mean_rank_bootstrap_summary %>%
      filter(
        analysis ==
          "overall_hard",
        method_label %in%
          short_list_configurations
      ) %>%
      transmute(
        configuration =
          method_label,
        
        overall_hard_mean_rank =
          mean_rank,
        
        overall_hard_rank_lower =
          bootstrap_lower,
        
        overall_hard_rank_upper =
          bootstrap_upper,
        
        overall_hard_rank_ci =
          mean_rank_ci_label
      ),
    by = "configuration"
  ) %>%
  left_join(
    null_rank_table %>%
      filter(
        method_label %in%
          short_list_configurations
      ) %>%
      transmute(
        configuration =
          method_label,
        
        null_mean_rank =
          mean_rank,
        
        null_mean_detection_burden =
          mean_n_detected
      ),
    by = "configuration"
  ) %>%
  mutate(
    configuration = factor(
      configuration,
      levels =
        short_list_configurations
    )
  ) %>%
  arrange(
    configuration
  ) %>%
  mutate(
    configuration =
      as.character(
        configuration
      )
  )

if (
  nrow(
    short_list_evidence
  ) != 2L ||
  !setequal(
    short_list_evidence$
    configuration,
    short_list_configurations
  )
) {
  stop(
    paste0(
      "The short-list evidence table must contain ",
      "Joinpoint(F) and MFEF+JP(F,12,0.95)."
    ),
    call. = FALSE
  )
}

required_short_list_columns <- c(
  "hard_positive_coverage",
  "hard_positive_coverage_pct",
  "conditional_hard_f1",
  "hard_significant_wins",
  "hard_significant_losses",
  "overall_hard_mean_rank",
  "null_mean_rank",
  "null_mean_detection_burden"
)

missing_short_list_values <- short_list_evidence %>%
  select(
    all_of(
      required_short_list_columns
    )
  ) %>%
  summarise(
    across(
      everything(),
      ~ anyNA(.x)
    )
  ) %>%
  unlist(
    use.names = TRUE
  )

if (
  any(
    missing_short_list_values
  )
) {
  stop(
    paste0(
      "Missing short-list evidence in: ",
      paste(
        names(
          missing_short_list_values
        )[
          missing_short_list_values
        ],
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


joinpoint_short_list <- short_list_evidence %>%
  filter(
    configuration ==
      "Joinpoint(F)"
  )

mfef_jp_short_list <- short_list_evidence %>%
  filter(
    configuration ==
      "MFEF+JP(F,12,0.95)"
  )

# ---------------------------------------------------------
# Validate the Nemenyi critical-difference statement
# ---------------------------------------------------------

overall_hard_rank_order <- friedman_overall_hard$rank_table %>%
  arrange(
    mean_rank,
    method_label
  )

joinpoint_hard_rank <- overall_hard_rank_order %>%
  filter(
    method_label ==
      "Joinpoint(F)"
  ) %>%
  pull(
    mean_rank
  )

next_hard_rank <- overall_hard_rank_order %>%
  filter(
    method_label !=
      "Joinpoint(F)"
  ) %>%
  slice(
    1L
  ) %>%
  pull(
    mean_rank
  )

joinpoint_rank_gap <-
  next_hard_rank -
  joinpoint_hard_rank

overall_hard_cd <-
  friedman_overall_hard$
  critical_difference

joinpoint_exceeds_cd <-
  joinpoint_rank_gap >
  overall_hard_cd

if (!joinpoint_exceeds_cd) {
  stop(
    paste0(
      "Joinpoint(F) does not exceed the Nemenyi critical ",
      "difference relative to the next overall hard-F1 rank."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------
# Build the short-list summary
# ---------------------------------------------------------

short_list_table <- tibble::tibble(
  configuration = c(
    "Joinpoint(F)",
    "MFEF+JP(F,12,0.95)"
  ),
  
  sustained_by = c(
    paste0(
      "Lowest overall hard-F1 Friedman mean rank, ",
      sprintf(
        "%.2f",
        joinpoint_short_list$
          overall_hard_mean_rank
      ),
      "; mean-rank difference from the next configuration ",
      "exceeds the Nemenyi critical difference"
    ),
    
    paste0(
      mfef_jp_short_list$
        hard_significant_wins,
      " of 19 significant hard-F1 wins; ",
      mfef_jp_short_list$
        soft_significant_wins,
      " of 19 significant soft-F1 wins; ",
      "largest soft-F1 winner count, ",
      mfef_jp_short_list$
        soft_winner_assignments,
      "; lowest null-series mean-rank point estimate, ",
      sprintf(
        "%.2f",
        mfef_jp_short_list$
          null_mean_rank
      )
    )
  ),
  
  observed_profile = c(
    paste0(
      "Positive-score coverage ",
      sprintf(
        "%.1f",
        joinpoint_short_list$
          hard_positive_coverage_pct
      ),
      "%; median conditional hard F1 = ",
      sprintf(
        "%.2f",
        joinpoint_short_list$
          conditional_hard_f1
      ),
      "; broad exact-match coverage profile"
    ),
    
    paste0(
      "Positive-score coverage ",
      sprintf(
        "%.1f",
        mfef_jp_short_list$
          hard_positive_coverage_pct
      ),
      "%; median conditional hard F1 = ",
      sprintf(
        "%.2f",
        mfef_jp_short_list$
          conditional_hard_f1
      ),
      "; mean null-series burden = ",
      sprintf(
        "%.2f",
        mfef_jp_short_list$
          null_mean_detection_burden
      ),
      "; selective high-confidence profile"
    )
  )
)

if (
  nrow(
    short_list_table
  ) != 2L ||
  anyNA(
    short_list_table
  )
) {
  stop(
    "The short-list summary is incomplete.",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("SHORT-LIST SUMMARY\n")
cat("=====================================\n")

print(
  short_list_table,
  n = Inf,
  width = Inf
)

short_list_table_file <- file.path(
  synthetic_results_directory,
  "short_list_table.rds"
)

short_list_evidence_file <- file.path(
  synthetic_results_directory,
  "short_list_evidence.rds"
)

saveRDS(
  short_list_table,
  short_list_table_file
)

saveRDS(
  short_list_evidence,
  short_list_evidence_file
)

if (
  !file.exists(
    short_list_table_file
  ) ||
  !file.exists(
    short_list_evidence_file
  )
) {
  stop(
    "The short-list table artifacts were not saved.",
    call. = FALSE
  )
}

cat(
  "\nShort-list table saved:",
  short_list_table_file,
  "\n"
)

cat(
  "Short-list evidence saved:",
  short_list_evidence_file,
  "\n"
)


# =========================================================
# OVERALL HARD-F1 WILCOXON SUMMARY
# =========================================================

wilcoxon_overall_hard_table <-
  shared_positive_wilcoxon_ranking %>%
  filter(
    metric ==
      "Hard F1"
  ) %>%
  transmute(
    configuration,
    method_family,
    
    significant_wins =
      n_significant_wins_bonferroni_005,
    
    significant_losses =
      n_significant_losses_bonferroni_005,
    
    significant_win_rate =
      significant_win_rate_bonferroni_005
  ) %>%
  left_join(
    mean_rank_bootstrap_summary %>%
      filter(
        analysis ==
          "overall_hard"
      ) %>%
      transmute(
        configuration =
          method_label,
        
        friedman_mean_rank =
          mean_rank,
        
        friedman_rank_lower =
          bootstrap_lower,
        
        friedman_rank_upper =
          bootstrap_upper,
        
        friedman_mean_rank_ci =
          mean_rank_ci_label
      ),
    by = "configuration"
  ) %>%
  arrange(
    desc(
      significant_wins
    ),
    significant_losses,
    friedman_mean_rank,
    configuration
  )

wilcoxon_overall_hard_formatted_table <-
  wilcoxon_overall_hard_table %>%
  transmute(
    configuration,
    method_family,
    
    significant_wins = paste0(
      significant_wins,
      "/19"
    ),
    
    significant_losses = paste0(
      significant_losses,
      "/19"
    ),
    
    significant_win_rate = sprintf(
      "%.1f%%",
      significant_win_rate
    ),
    
    friedman_mean_rank_ci = sprintf(
      "%.2f [%.2f, %.2f]",
      friedman_mean_rank,
      friedman_rank_lower,
      friedman_rank_upper
    )
  )

if (
  nrow(
    wilcoxon_overall_hard_table
  ) != 20L ||
  nrow(
    wilcoxon_overall_hard_formatted_table
  ) != 20L ||
  anyNA(
    wilcoxon_overall_hard_table$
    method_family
  ) ||
  anyNA(
    wilcoxon_overall_hard_table$
    friedman_mean_rank
  ) ||
  anyNA(
    wilcoxon_overall_hard_table$
    friedman_rank_lower
  ) ||
  anyNA(
    wilcoxon_overall_hard_table$
    friedman_rank_upper
  )
) {
  stop(
    "The overall hard-F1 Wilcoxon summary is incomplete.",
    call. = FALSE
  )
}

if (
  any(
    wilcoxon_overall_hard_table$
    significant_wins +
    wilcoxon_overall_hard_table$
    significant_losses >
    19L
  )
) {
  stop(
    "Invalid overall hard-F1 Wilcoxon win-loss totals.",
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("OVERALL HARD-F1 WILCOXON SUMMARY\n")
cat("=====================================\n")

print(
  wilcoxon_overall_hard_formatted_table,
  n = Inf,
  width = Inf
)

wilcoxon_overall_hard_table_file <- file.path(
  synthetic_results_directory,
  "wilcoxon_overall_hard_table.rds"
)

wilcoxon_overall_hard_formatted_table_file <- file.path(
  synthetic_results_directory,
  "wilcoxon_overall_hard_formatted_table.rds"
)

saveRDS(
  wilcoxon_overall_hard_table,
  wilcoxon_overall_hard_table_file
)

saveRDS(
  wilcoxon_overall_hard_formatted_table,
  wilcoxon_overall_hard_formatted_table_file
)

if (
  !file.exists(
    wilcoxon_overall_hard_table_file
  ) ||
  !file.exists(
    wilcoxon_overall_hard_formatted_table_file
  )
) {
  stop(
    paste0(
      "The overall hard-F1 Wilcoxon table ",
      "artifacts were not saved."
    ),
    call. = FALSE
  )
}

cat(
  "\nOverall hard-F1 Wilcoxon table saved:",
  wilcoxon_overall_hard_table_file,
  "\n"
)


cat(
  "Formatted overall hard-F1 Wilcoxon summary saved:",
  wilcoxon_overall_hard_formatted_table_file,
  "\n"
)


# =========================================================
# SAVE SYNTHETIC TABLE ARTIFACTS
# =========================================================

synthetic_table_objects <- list(
  consolidated_performance_table =
    consolidated_performance_table,
  
  parameter_matched_tests =
    parameter_matched_tests,
  
  parameter_matched_summary =
    parameter_matched_summary,
  
  parameter_matched_summary_table =
    parameter_matched_summary_table,
  
  null_detection_summary =
    null_detection_summary,
  
  null_rank_summary =
    null_rank_summary,
  
  null_rank_summary_table =
    null_rank_summary_table,
  
  null_nemenyi_pairwise =
    null_nemenyi_pairwise,
  
  null_wilcoxon_summary =
    null_wilcoxon_summary,
  
  null_wilcoxon_summary_table =
    null_wilcoxon_summary_table,
  
  friedman_nemenyi_summary =
    friedman_nemenyi_summary,
  
  mean_rank_bootstrap_summary_table =
    mean_rank_bootstrap_summary_table,
  
  shared_positive_wilcoxon_summary =
    shared_positive_wilcoxon_summary,
  
  shared_positive_wilcoxon_summary_table =
    shared_positive_wilcoxon_summary_table
)

invalid_synthetic_table_objects <- names(
  synthetic_table_objects
)[
  purrr::map_lgl(
    synthetic_table_objects,
    function(table_object) {
      is.null(table_object) ||
        !is.data.frame(table_object) ||
        nrow(table_object) == 0L
    }
  )
]

if (length(invalid_synthetic_table_objects) > 0L) {
  stop(
    paste0(
      "Invalid synthetic table object(s): ",
      paste(
        invalid_synthetic_table_objects,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

synthetic_table_files <- purrr::imap_chr(
  synthetic_table_objects,
  function(
    table_object,
    table_name) {
    
    table_file <- file.path(
      synthetic_results_directory,
      paste0(
        table_name,
        ".rds"
      )
    )
    
    saveRDS(
      table_object,
      table_file
    )
    
    table_file
  }
)

missing_synthetic_table_files <- synthetic_table_files[
  !file.exists(
    synthetic_table_files
  )
]

if (length(missing_synthetic_table_files) > 0L) {
  stop(
    paste0(
      "Missing synthetic table artifact(s): ",
      paste(
        missing_synthetic_table_files,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

synthetic_table_file_sizes <- file.info(
  synthetic_table_files
)$size

empty_synthetic_table_files <- synthetic_table_files[
  is.na(
    synthetic_table_file_sizes
  ) |
    synthetic_table_file_sizes <= 0
]

if (length(empty_synthetic_table_files) > 0L) {
  stop(
    paste0(
      "Empty synthetic table artifact(s): ",
      paste(
        empty_synthetic_table_files,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

cat("\n=====================================\n")
cat("SYNTHETIC TABLE ARTIFACTS\n")
cat("=====================================\n")

for (table_file in synthetic_table_files) {
  cat(
    basename(
      table_file
    ),
    "|",
    file.info(
      table_file
    )$size,
    "bytes\n"
  )
}

cat(
  "\nAll synthetic table artifacts were saved successfully.\n"
)

# =========================================================
# VERIFY SYNTHETIC TABLE ARTIFACTS
# =========================================================

expected_synthetic_table_files <- c(
  short_list_table_file,
  short_list_evidence_file,
  wilcoxon_overall_hard_table_file,
  wilcoxon_overall_hard_formatted_table_file,
  unname(
    synthetic_table_files
  )
)

expected_synthetic_table_files <- unique(
  expected_synthetic_table_files
)

missing_expected_synthetic_table_files <-
  expected_synthetic_table_files[
    !file.exists(
      expected_synthetic_table_files
    )
  ]

if (
  length(
    missing_expected_synthetic_table_files
  ) > 0L
) {
  stop(
    paste0(
      "Missing expected synthetic table file(s): ",
      paste(
        missing_expected_synthetic_table_files,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

expected_synthetic_table_sizes <- file.info(
  expected_synthetic_table_files
)$size

if (
  any(
    is.na(
      expected_synthetic_table_sizes
    ) |
    expected_synthetic_table_sizes <= 0
  )
) {
  stop(
    "One or more expected synthetic table files are empty.",
    call. = FALSE
  )
}

synthetic_table_inventory <- tibble::tibble(
  file = basename(
    expected_synthetic_table_files
  ),
  
  path =
    expected_synthetic_table_files,
  
  size_bytes =
    expected_synthetic_table_sizes,
  
  modified = file.info(
    expected_synthetic_table_files
  )$mtime
) %>%
  arrange(
    file
  )

cat("\n=====================================\n")
cat("SYNTHETIC TABLE ARTIFACT INVENTORY\n")
cat("=====================================\n")

print(
  synthetic_table_inventory,
  n = Inf,
  width = Inf
)

cat(
  "\nNumber of synthetic table artifacts:",
  nrow(
    synthetic_table_inventory
  )
 
)

# =========================================================
# SAVE SYNTHETIC FIGURE ARTIFACTS
# =========================================================

remove_plot_titles <- function(
    plot_object) {
  
  plot_object +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank()
    )
}

synthetic_figure_objects <- list(
  wins_hard_vs_soft = list(
    plot = remove_plot_titles(
      p_wins_hard_vs_soft
    ),
    width = 9,
    height = 7
  ),
  
  pairwise_hard_f1_heatmap = list(
    plot = remove_plot_titles(
      p_pairwise_heatmap_hard
    ),
    width = 13,
    height = 11
  ),
  
  family_winner_share_by_regime = list(
    plot = remove_plot_titles(
      p_family_winner_share
    ),
    width = 10,
    height = 6
  ),
  
  conditional_f1_by_family_regime = list(
    plot = remove_plot_titles(
      p_boxplot_family_regime
    ),
    width = 12,
    height = 7
  ),
  
  null_rank_cd = list(
    plot = remove_plot_titles(
      p_null_rank_cd
    ),
    width = 10,
    height = 7
  ),
  
  friedman_overall_hard_cd = list(
    plot = remove_plot_titles(
      p_friedman_overall_hard
    ),
    width = 10,
    height = 7
  )
)

invalid_synthetic_figure_objects <- names(
  synthetic_figure_objects
)[
  purrr::map_lgl(
    synthetic_figure_objects,
    function(figure_specification) {
      is.null(
        figure_specification$plot
      ) ||
        !inherits(
          figure_specification$plot,
          "ggplot"
        ) ||
        !is.numeric(
          figure_specification$width
        ) ||
        !is.numeric(
          figure_specification$height
        )
    }
  )
]

if (length(invalid_synthetic_figure_objects) > 0L) {
  stop(
    paste0(
      "Invalid synthetic figure object(s): ",
      paste(
        invalid_synthetic_figure_objects,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

synthetic_figure_files <- purrr::imap_chr(
  synthetic_figure_objects,
  function(
    figure_specification,
    figure_name) {
    
    figure_file <- file.path(
      synthetic_figures_directory,
      paste0(
        figure_name,
        ".pdf"
      )
    )
    
    if (file.exists(figure_file)) {
      unlink(
        figure_file
      )
    }
    
    ggplot2::ggsave(
      filename = figure_file,
      plot = figure_specification$plot,
      device = "pdf",
      width = figure_specification$width,
      height = figure_specification$height,
      units = "in",
      limitsize = FALSE
    )
    
    figure_file
  }
)

missing_synthetic_figure_files <- synthetic_figure_files[
  !file.exists(
    synthetic_figure_files
  )
]

if (length(missing_synthetic_figure_files) > 0L) {
  stop(
    paste0(
      "Missing synthetic figure artifact(s): ",
      paste(
        missing_synthetic_figure_files,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

synthetic_figure_metadata <- file.info(
  synthetic_figure_files
)

empty_synthetic_figure_files <- synthetic_figure_files[
  is.na(
    synthetic_figure_metadata$size
  ) |
    synthetic_figure_metadata$size <= 0
]

if (length(empty_synthetic_figure_files) > 0L) {
  stop(
    paste0(
      "Empty synthetic figure artifact(s): ",
      paste(
        empty_synthetic_figure_files,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

synthetic_figure_inventory <- tibble::tibble(
  file = basename(
    synthetic_figure_files
  ),
  
  path =
    synthetic_figure_files,
  
  size_bytes =
    synthetic_figure_metadata$size,
  
  modified =
    synthetic_figure_metadata$mtime
) %>%
  arrange(
    file
  )

cat("\n=====================================\n")
cat("SYNTHETIC FIGURE ARTIFACTS\n")
cat("=====================================\n")

print(
  synthetic_figure_inventory,
  n = Inf,
  width = Inf
)

cat(
  "\nNumber of synthetic figure artifacts:",
  nrow(
    synthetic_figure_inventory
  ),
  "\n"
)

cat(
  "All synthetic figure artifacts were saved successfully.\n"
)
