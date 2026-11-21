# Paired ablation of the hierarchical MFEF against a single-level FCO.
# Run after 02-run-mfef-synthetic.R with MFEF_FLAT_ABLATION_ONLY=true.

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
detection_dir <- file.path(root, "outputs", "synthetic", "detections")
analysis_dir <- file.path(root, "outputs", "synthetic", "analysis")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

production_files <- list.files(
  detection_dir,
  pattern = "^batch_mfef_joinpoint_hybrid_soft_hard_.*\\.csv$",
  full.names = TRUE
)
flat_files <- list.files(
  detection_dir,
  pattern = "^batch_fco_flat_ablation_.*\\.csv$",
  full.names = TRUE
)

if (length(production_files) == 0L) {
  stop("Production detection files were not found.", call. = FALSE)
}
if (length(flat_files) == 0L) {
  stop(
    paste0(
      "Flat-ablation files were not found. Run script 02 with ",
      "MFEF_FLAT_ABLATION_ONLY=true first."
    ),
    call. = FALSE
  )
}

read_batches <- function(files) {
  do.call(rbind, lapply(files, read.csv, check.names = FALSE))
}

production <- read_batches(production_files)
flat <- read_batches(flat_files)
hierarchical <- production[
  production$method == "MFEF" & production$status == "ok",
]
flat <- flat[flat$method == "FCO_FLAT" & flat$status == "ok",]

keys <- c("serie_id", "serie_id_nome", "time_tolerance", "threshold")
paired <- merge(
  hierarchical,
  flat,
  by = keys,
  suffixes = c("_hierarchical", "_flat"),
  all = FALSE
)
if (nrow(paired) == 0L) {
  stop("No parameter-matched hierarchical/flat rows were found.", call. = FALSE)
}

paired$scenario <- sub("_rep_[0-9]+$", "", paired$serie_id_nome)
paired$scenario <- sub("_[1-5]q_", "_", paired$scenario)
paired$is_null <- grepl("^null_", paired$serie_id_nome)

# The metric implementation returns undefined F1 when both precision and
# recall are zero. For series with reference changes this is a complete miss
# and must enter the matched ablation as F1 = 0 rather than be dropped.
for (metric_name in c("hard_F1", "soft_F1")) {
  for (architecture in c("hierarchical", "flat")) {
    column_name <- paste0(metric_name, "_", architecture)
    paired[[column_name]][
      !paired$is_null & is.na(paired[[column_name]])
    ] <- 0
  }
}

cluster_bootstrap_ci <- function(scenario_differences, B = 2000L, seed = 42L) {
  set.seed(seed)
  n <- length(scenario_differences)
  draws <- replicate(
    B,
    mean(sample(scenario_differences, n, replace = TRUE), na.rm = TRUE)
  )
  unname(quantile(draws, c(0.025, 0.975), na.rm = TRUE))
}

paired_summary <- function(data, hierarchical_name, flat_name, criterion) {
  keep <- complete.cases(data[, c(hierarchical_name, flat_name)])
  data <- data[keep,]
  difference <- data[[hierarchical_name]] - data[[flat_name]]
  scenario_difference <- aggregate(
    difference,
    list(scenario = data$scenario),
    mean
  )$x
  series_test <- suppressWarnings(wilcox.test(difference, mu = 0, exact = FALSE))
  scenario_test <- suppressWarnings(
    wilcox.test(scenario_difference, mu = 0, exact = FALSE)
  )
  scenario_ci <- cluster_bootstrap_ci(scenario_difference)

  data.frame(
    criterion = criterion,
    time_tolerance = unique(data$time_tolerance),
    threshold = unique(data$threshold),
    n_series = length(difference),
    n_scenarios = length(scenario_difference),
    mean_difference_hierarchical_minus_flat = mean(difference),
    median_difference_hierarchical_minus_flat = median(difference),
    series_wilcoxon_p = series_test$p.value,
    scenario_mean_difference = mean(scenario_difference),
    scenario_cluster_bootstrap_lower = scenario_ci[1],
    scenario_cluster_bootstrap_upper = scenario_ci[2],
    scenario_wilcoxon_p = scenario_test$p.value
  )
}

results <- list()
result_index <- 1L
for (current_k in sort(unique(paired$time_tolerance))) {
  for (current_rho in sort(unique(paired$threshold))) {
    current <- paired[
      paired$time_tolerance == current_k & paired$threshold == current_rho,
    ]
    changed <- current[!current$is_null,]
    null <- current[current$is_null,]
    
    cat(
      "\nk =", current_k,
      "| rho =", current_rho,
      "| paired =", nrow(current),
      "| changed =", nrow(changed),
      "| null =", nrow(null),
      "\n"
    )

    for (metric in c("hard_F1", "soft_F1")) {
      results[[result_index]] <- paired_summary(
        changed,
        paste0(metric, "_hierarchical"),
        paste0(metric, "_flat"),
        metric
      )
      result_index <- result_index + 1L
    }

    results[[result_index]] <- paired_summary(
      null,
      "n_detected_hierarchical",
      "n_detected_flat",
      "null_false_detection_burden"
    )
    result_index <- result_index + 1L
  }
}

ablation_summary <- do.call(rbind, results)
ablation_summary$series_wilcoxon_p_holm <- p.adjust(
  ablation_summary$series_wilcoxon_p,
  method = "holm"
)
ablation_summary$scenario_wilcoxon_p_holm <- p.adjust(
  ablation_summary$scenario_wilcoxon_p,
  method = "holm"
)
output_file <- file.path(analysis_dir, "flat_ablation_paired_summary.csv")
write.csv(ablation_summary, output_file, row.names = FALSE)
print(ablation_summary, row.names = FALSE)
cat("\nSaved:", output_file, "\n")
