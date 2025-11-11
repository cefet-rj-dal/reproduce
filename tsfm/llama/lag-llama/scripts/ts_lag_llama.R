library('daltoolbox')

# Bridge to Python implementation used for zero-shot and fine-tune predictions
python_src = paste(dirname(getwd()), "llama/lag-llama/scripts/ts_lag_llama.py", sep="/")
python_cli_source <- paste(dirname(getwd()), "llama/lag-llama/scripts/finetune_CLI.R", sep="/")
python_cli_path <- paste(dirname(getwd()), "llama/lag-llama/scripts/finetune_CLI.py", sep="/")

# call_finetune_prediction
# Purpose: Trigger the Python CLI (finetune_CLI.py) to run fine-tuning/prediction.
# Inputs:
# - train_dataset: path to CSV with a single time series column
# - steps_ahead: forecast horizon
# - samples: number of samples to draw
# - num_epochs: fine-tuning epochs (if not zero-shot)
# - val_dataset: optional validation dataset path
# - zero_shot: whether to run in zero-shot mode
call_finetune_prediction <- function(train_dataset, steps_ahead, samples, num_epochs=30, val_dataset=NULL, zero_shot=FALSE) {
  # Required parameters
  parameters <- c(
    "--train_dataset", paste("'", as.character(train_dataset), "'", sep=""),
    "--steps_ahead", as.integer(steps_ahead)
  )

  # Optional parameters (epochs, validation dataset, zero-shot strategy)
  if (!is.null(val_dataset)){
    val_dataset <- paste("'", as.character(val_dataset), "'")
    parameters <- append(parameters, list("--val_dataset", val_dataset))
  }
  parameters <- append(parameters, list("--samples", as.integer(samples)))

  # Use CLI call instead of reticulate for speed and isolation
  status <- system2(command = "python3", args = c(python_cli_path, parameters))
}

# ts_lag_llama: constructor for daltoolbox-compatible regressor wrapper
ts_lag_llama <- function() {
  obj <- ts_reg()
  class(obj) <- append("ts_lag_llama", class(obj))
  return(obj)
}

#' @export
fit.ts_lag_llama <- function(object, x=NULL, y=NULL, steps_ahead=NULL, ...) {
  # No training state needed on the R side; returns self
  return(object)
}

#' @export
predict.ts_lag_llama <- function(object, x, y=NULL, steps_ahead=NULL, samples=100, zero_shot=TRUE, ...) {
  # Ensure Python function is available
  if (!exists("ts_lag_llama_predict"))
    reticulate::source_python(python_src)

  # Concatenate train (x) and optional test (y), then clip to context window
  ts <- c(x, y)
  if (!is.null(y))
    ts <- ts[1:(length(ts)-steps_ahead)]

  # Use zero-shot by default through Python backend
  predictions <- ts_lag_llama_predict(dataset=ts, steps_ahead=steps_ahead, samples=as.integer(samples), zs=TRUE)
  return(predictions[[1]]$samples)
}

#' @export
predict_finetune.ts_lag_llama <- function(object, x, y=NULL, steps_ahead=NULL, samples=100, zero_shot=FALSE, ...) {
  # Command-line based fine-tuning/prediction using the CLI wrapper
  ts <- c(x, y)
  if (!is.null(y))
    ts <- ts[1:(length(ts)-steps_ahead)]

  # Build a temporary CSV file to pass the time series to Python CLI
  context <- names(x)
  temp_file_path <- tempfile(fileext = ".csv", pattern=paste("time_series", context, sep = "_"))
  write.csv(ts, temp_file_path, row.names=FALSE)

  # Execute CLI call
  call_finetune_prediction(train_dataset=temp_file_path, steps_ahead=steps_ahead, samples=samples)

  # Read result CSV written by the Python script (conventional path)
  output_filename <- "/tmp/results_x.csv"
  print("Reading saved file at")
  print(output_filename)

  predictions_csv <- read.csv(output_filename)
  print(predictions_csv)
  return(predictions_csv)
}

