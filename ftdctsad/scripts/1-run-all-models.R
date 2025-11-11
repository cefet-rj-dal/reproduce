library("daltoolbox")
library("harbinger")

#
# 1 - Run All Models (Fine-Tuning Detection Criteria)
# --------------------------------------------------
# Purpose:
# - Load a benchmark time series dataset with event labels
# - Run multiple anomaly detectors (SVM, LSTM Autoencoder, Conv1D, ELM)
# - Evaluate each run with standard classification metrics (incl. F1)
# - Save per-run results (distance/outlier/check variants) as RDS files
#
# Notes:
# - Expects an RData file containing an object `nab_sampleML`.
#   This script searches for it under `datasets/` first, then current dir.
# - Results are written under `results/rds/` (created if needed).

hutils <- harutils()

# Evaluate classification metrics (TP, FP, FN, TN, F1, etc.)
evaluate_har <- function(detection, event) {
  detection[is.na(detection)] <- FALSE
  detection <- as.logical(unlist(detection))
  event <- as.logical(unlist(event))

  TP <- sum(detection & event)
  FP <- sum(detection & !event)
  FN <- sum(!detection & event)
  TN <- sum(!detection & !event)

  confMatrix <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
                       dimnames = list(c("Detection = TRUE", "Detection = FALSE"),
                                       c("Event = TRUE", "Event = FALSE")))

  total <- TP + FP + FN + TN
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
  specificity <- ifelse((FP + TN) > 0, TN / (FP + TN), 0)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  accuracy <- (TP + TN) / total
  balanced_accuracy <- (sensitivity + specificity) / 2
  F1 <- ifelse((precision + sensitivity) > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0)

  metrics <- list(
    TP = TP, FP = FP, FN = FN, TN = TN,
    confMatrix = confMatrix,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    F1 = F1,
    balanced_accuracy = balanced_accuracy
  )
  return(metrics)
}

# Optional collector for error lines
results_header <- data.frame(dataset = character(0), sample = character(0), modelo = character(0),
                             f1 = numeric(0), erro = logical(0), causa_erro = character(0))

# Load dataset object
dict_dados <- new.env()
candidate_paths <- c("datasets/nab_sampleML.RData", "nab_sampleML.RData")
path_found <- candidate_paths[file.exists(candidate_paths)][1]
if (!is.na(path_found)) {
  load(path_found)
  if (exists("nab_sampleML")) {
    for (i in 1:length(nab_sampleML)) {
      nab_sampleML[[i]]$series <- as.numeric(nab_sampleML[[i]]$value)
    }
    dict_dados$nab_sampleML <- nab_sampleML
  } else {
    message("Object `nab_sampleML` not found inside the RData.")
  }
} else {
  message("File nab_sampleML.RData not found under datasets/ or current dir.")
}

# Ensure output directory exists
results_dir <- file.path("results", "rds")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# Run detection pipelines
for (dataset_name in ls(dict_dados)) {
  dataset_list <- dict_dados[[dataset_name]]
  cat("\nDataset:", dataset_name, "\n")

  for (sample_name in names(dataset_list)) {
    cat("Sample:", sample_name, "\n")
    dataset <- dataset_list[[sample_name]]

    dataset$event[is.na(dataset$event)] <- FALSE
    dataset$series[is.na(dataset$series)] <- mean(dataset$series, na.rm = TRUE)
    dataset$event <- as.logical(dataset$event)
    dataset$series <- as.numeric(dataset$series)

    if (length(unique(dataset$series)) <= 1) {
      message("Error: Insufficient or constant data in ", sample_name)
      next
    }

    n_event_states <- length(unique(dataset$event))
    n_series_values <- length(unique(dataset$series))

    if (n_event_states != 0 && n_series_values > 1) {
      distance_group <- list("l1", "l2")
      outliers_group <- list("boxplot", "gaussian", "ratio")
      check_group <- list("firstgroup", "highgroup")

      # Models under test
      modelos <- list(
        svm  = hanr_ml(ts_svm(ts_norm_gminmax(),  input_size = 4, kernel = "radial")),
        lstm = han_autoencoder(3, 2, lae_encode_decode, num_epochs = 1500),
        conv = hanr_ml(ts_conv1d(ts_norm_gminmax(), input_size = 4, epochs = 1000)),
        elm  = hanr_ml(ts_elm(ts_norm_gminmax(),   input_size = 4, nhid = 3, actfun = "purelin"))
      )

      for (model_name in names(modelos)) {
        model <- modelos[[model_name]]

        for (distance in distance_group) {
          for (outliers in outliers_group) {
            for (check in check_group) {
              model$har_distance <- if (distance == "l1") hutils$har_distance_l1 else hutils$har_distance_l2
              model$har_outliers <- switch(outliers,
                                           "boxplot"  = hutils$har_outliers_boxplot,
                                           "gaussian" = hutils$har_outliers_gaussian,
                                           "ratio"    = hutils$har_outliers_ratio)
              model$har_outliers_check <- if (check == "firstgroup") hutils$har_outliers_checks_firstgroup else hutils$har_outliers_checks_highgroup

              tryCatch({
                if (model_name == "elm" && length(unique(dataset$series)) <= 2) {
                  stop("Insufficient data for ELM model.")
                }

                model <- fit(model, dataset$series)
                detection <- detect(model, dataset$series)
                if (is.null(detection$event) || length(detection$event) == 0) {
                  stop("Error: Model did not return detected events for ", sample_name)
                }

                detection_event <- as.logical(detection$event)
                detection_event[is.na(detection_event)] <- FALSE
                dataset_event <- as.logical(dataset$event)
                dataset_event[is.na(dataset_event)] <- FALSE

                if (length(detection_event) != length(dataset_event)) {
                  stop("Error: Length mismatch between detection$event and dataset$event for ", sample_name)
                }

                evaluation <- evaluate_har(detection_event, dataset_event)
                cat("F1 Score for", model_name, ":", evaluation$F1, "\n")

                # Save result
                filename <- paste(model_name, distance, outliers, check, dataset_name, sample_name, sep = "_")
                saveRDS(evaluation, file = file.path(results_dir, paste0(filename, ".RDS")))
              }, error = function(e) {
                message("Model error ", model_name, " for ", sample_name, ": ", e$message)
              })
            }
          }
        }
      }
    } else {
      # Example of collecting an error row if needed
      # results_header <- rbind(results_header, data.frame(dataset=dataset_name, sample=sample_name, modelo="", f1=0, erro=TRUE, causa_erro="No data"))
      next
    }
  }
}

cat("\nExecution finished; files saved under results/rds/.\n")

