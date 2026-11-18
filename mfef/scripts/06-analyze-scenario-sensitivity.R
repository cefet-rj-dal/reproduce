# Scenario-level sensitivity analysis for the validated 20 configurations.
# Replicates are averaged within factorial scenarios before inference.

library(ggplot2)
library(ggrepel)

detection_dir <- file.path("outputs", "synthetic", "detections")
analysis_dir <- file.path("outputs", "synthetic", "analysis")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  detection_dir,
  pattern = "^batch_mfef_joinpoint_hybrid_soft_hard_.*\\.csv$",
  full.names = TRUE
)
if (length(files) != 2L) {
  stop("Expected the two validated production batches.", call. = FALSE)
}

results <- do.call(rbind, lapply(files, read.csv, check.names = FALSE))
results <- results[results$status == "ok",]
results$scenario <- sub("_rep_[0-9]+$", "", results$serie_id_nome)
results$scenario <- sub("_[1-5]q_", "_", results$scenario)
results$is_null <- grepl("^null_", results$serie_id_nome)
results$hard_F1[!results$is_null & is.na(results$hard_F1)] <- 0
results$soft_F1[!results$is_null & is.na(results$soft_F1)] <- 0

format_rho <- function(x) sub("0$", "", format(x, nsmall = 2, trim = TRUE))
results$configuration <- ifelse(
  results$method == "Joinpoint",
  paste0("Joinpoint(", ifelse(results$log_transform, "T", "F"), ")"),
  ifelse(
    results$method == "MFEF",
    paste0("MFEF(", results$time_tolerance, ",", format_rho(results$threshold), ")"),
    paste0(
      "MFEF+JP(",
      ifelse(results$method == "MFEF_JP_TRUE", "T", "F"),
      ",", results$time_tolerance, ",", format_rho(results$threshold), ")"
    )
  )
)

cluster_ci <- function(x, B = 2000L, seed = 42L) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) stop("No finite scenario differences.", call. = FALSE)
  set.seed(seed)
  draws <- replicate(B, mean(sample(x, length(x), replace = TRUE)))
  unname(quantile(draws, c(0.025, 0.975)))
}

scenario_matrix <- function(data, metric) {
  aggregated <- aggregate(
    data[[metric]],
    list(scenario = data$scenario, configuration = data$configuration),
    mean
  )
  names(aggregated)[3] <- "value"
  wide <- reshape(
    aggregated,
    idvar = "scenario",
    timevar = "configuration",
    direction = "wide"
  )
  configuration_names <- sub("^value\\.", "", names(wide)[-1])
  matrix_value <- as.matrix(wide[-1])
  colnames(matrix_value) <- configuration_names
  rownames(matrix_value) <- wide$scenario
  matrix_value
}

changed <- results[!results$is_null,]
comparison_pairs <- list(
  c("MFEF+JP(F,12,0.95)", "Joinpoint(F)"),
  c("MFEF+JP(F,12,0.95)", "MFEF(12,0.95)")
)

rank_results <- list()
contrast_results <- list()
rank_index <- 1L
contrast_index <- 1L

# Central operating-profile summaries use every valid change series, retaining
# zero F1 values, so that selective conditional performance is not mistaken
# for overall detection performance.
operating_profiles <- do.call(
  rbind,
  lapply(sort(unique(results$configuration)), function(configuration_name) {
    change_rows <- results[
      !results$is_null &
        results$configuration == configuration_name,
    ]
    null_rows <- results[
      results$is_null &
        results$configuration == configuration_name,
    ]

    data.frame(
      configuration = configuration_name,
      n_change_series = nrow(change_rows),
      mean_hard_F1 = mean(change_rows$hard_F1),
      mean_hard_precision = mean(change_rows$hard_precision),
      mean_hard_recall = mean(change_rows$hard_recall),
      hard_positive_coverage = mean(change_rows$hard_F1 > 0),
      mean_soft_F1 = mean(change_rows$soft_F1),
      mean_soft_precision = mean(change_rows$soft_precision),
      mean_soft_recall = mean(change_rows$soft_recall),
      soft_positive_coverage = mean(change_rows$soft_F1 > 0),
      n_null_series = nrow(null_rows),
      mean_null_detections = mean(null_rows$n_detected)
    )
  })
)

operating_profiles$family <- ifelse(
  grepl("^Joinpoint", operating_profiles$configuration),
  "Joinpoint",
  ifelse(
    grepl("^MFEF\\+JP", operating_profiles$configuration),
    "MFEF+JP",
    "MFEF"
  )
)

central_configurations <- c(
  "Joinpoint(F)",
  "MFEF+JP(F,12,0.95)"
)
central_profiles <- operating_profiles[
  operating_profiles$configuration %in% central_configurations,
  setdiff(names(operating_profiles), "family")
]

profile_labels <- c(
  "Joinpoint(F)",
  "Joinpoint(T)",
  "MFEF(12,0.95)",
  "MFEF+JP(F,6,0.95)",
  "MFEF+JP(F,12,0.95)"
)
operating_profiles$display_label <- ifelse(
  operating_profiles$configuration %in% profile_labels,
  operating_profiles$configuration,
  ""
)

operating_profile_plot <- ggplot(
  operating_profiles,
  aes(
    x = mean_soft_recall,
    y = mean_soft_precision,
    color = family,
    size = mean_null_detections
  )
) +
  geom_point(alpha = 0.82) +
  geom_text_repel(
    aes(label = display_label),
    seed = 42,
    box.padding = 0.45,
    point.padding = 0.25,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Joinpoint" = "#D55E00",
      "MFEF" = "#009E73",
      "MFEF+JP" = "#0072B2"
    )
  ) +
  scale_size_continuous(
    name = "Mean detections\nin null series",
    range = c(3.5, 10)
  ) +
  labs(
    x = "Mean soft recall across all change series",
    y = "Mean soft precision across all change series",
    color = "Family"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

for (metric in c("hard_F1", "soft_F1")) {
  values <- scenario_matrix(changed, metric)
  friedman <- friedman.test(values)
  mean_ranks <- colMeans(t(apply(-values, 1, rank, ties.method = "average")))
  rank_results[[rank_index]] <- data.frame(
    metric = metric,
    configuration = names(mean_ranks),
    scenario_mean_rank = unname(mean_ranks),
    n_scenarios = nrow(values),
    friedman_p = friedman$p.value
  )
  rank_index <- rank_index + 1L

  for (pair in comparison_pairs) {
    if (!all(pair %in% colnames(values))) {
      stop(paste("Missing configuration:", paste(setdiff(pair, colnames(values)), collapse = ", ")))
    }
    difference <- values[, pair[1]] - values[, pair[2]]
    difference <- difference[is.finite(difference)]
    interval <- cluster_ci(difference)
    test <- suppressWarnings(wilcox.test(difference, mu = 0, exact = FALSE))
    contrast_results[[contrast_index]] <- data.frame(
      metric = metric,
      configuration = pair[1],
      comparator = pair[2],
      n_scenarios = length(difference),
      mean_scenario_difference = mean(difference),
      median_scenario_difference = median(difference),
      bootstrap_lower = interval[1],
      bootstrap_upper = interval[2],
      wilcoxon_p = test$p.value
    )
    contrast_index <- contrast_index + 1L
  }
}

null_values <- scenario_matrix(results[results$is_null,], "n_detected")
for (pair in comparison_pairs) {
  if (!all(pair %in% colnames(null_values))) {
    stop(paste("Missing null configuration:", paste(setdiff(pair, colnames(null_values)), collapse = ", ")))
  }
  difference <- null_values[, pair[1]] - null_values[, pair[2]]
  difference <- difference[is.finite(difference)]
  interval <- cluster_ci(difference)
  test <- suppressWarnings(wilcox.test(difference, mu = 0, exact = FALSE))
  contrast_results[[contrast_index]] <- data.frame(
    metric = "null_n_detected",
    configuration = pair[1],
    comparator = pair[2],
    n_scenarios = length(difference),
    mean_scenario_difference = mean(difference),
    median_scenario_difference = median(difference),
    bootstrap_lower = interval[1],
    bootstrap_upper = interval[2],
    wilcoxon_p = test$p.value
  )
  contrast_index <- contrast_index + 1L
}

rank_table <- do.call(rbind, rank_results)
contrast_table <- do.call(rbind, contrast_results)
contrast_table$wilcoxon_p_holm <- p.adjust(contrast_table$wilcoxon_p, "holm")

write.csv(
  rank_table,
  file.path(analysis_dir, "scenario_sensitivity_ranks.csv"),
  row.names = FALSE
)
write.csv(
  contrast_table,
  file.path(analysis_dir, "scenario_sensitivity_contrasts.csv"),
  row.names = FALSE
)
write.csv(
  central_profiles,
  file.path(analysis_dir, "central_operating_profiles.csv"),
  row.names = FALSE
)
write.csv(
  operating_profiles[setdiff(names(operating_profiles), "display_label")],
  file.path(analysis_dir, "all_operating_profiles.csv"),
  row.names = FALSE
)
ggsave(
  file.path(analysis_dir, "soft_precision_recall_operating_profiles.png"),
  operating_profile_plot,
  width = 11,
  height = 7.5,
  dpi = 600
)

print(rank_table[order(rank_table$metric, rank_table$scenario_mean_rank),], row.names = FALSE)
cat("\nCentral scenario-level contrasts:\n")
print(contrast_table, row.names = FALSE)
cat("\nCentral operating profiles:\n")
print(central_profiles, row.names = FALSE)
