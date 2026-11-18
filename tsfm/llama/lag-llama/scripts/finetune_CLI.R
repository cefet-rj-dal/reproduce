# Helper to define arguments for the Python CLI and execute it
# Example paths (adjust to your environment):
# train_dataset <- "~/datasets/climate/input/climate.csv"
# script_name <- "~/llama/lag-llama/scripts/finetune_CLI.py"
# num_epochs <- 50
# num_predicts <- 1
# valid_dataset <- ""

fine_predict <- function(train_dataset, steps_ahead, num_epochs=50, val_dataset=NULL, samples=100, zero_shot=FALSE) {
  script_path <- "~/llama/lag-llama/scripts/finetune_CLI.py"

  train_dataset <- paste("'", as.character(train_dataset), "'")

  # Required parameters
  parameters <- c(
    "--train_dataset", train_dataset,
    "--steps_ahead", as.integer(steps_ahead)
  )

  # Optional parameters (epochs, validation dataset, zero-shot strategy)
  parameters <- append(parameters, list("--num_epochs", as.integer(num_epochs)))
  if (!is.null(val_dataset)){
    val_dataset <- paste("'", as.character(val_dataset), "'")
    parameters <- append(parameters, list("--val_dataset", val_dataset))
  }
  if (zero_shot){
    # Add flag to run in zero-shot mode
    parameters <- append(parameters, "--zero_shot")
  }
  parameters <- append(parameters, list("--samples", as.integer(samples)))

  # Execute Python script via CLI instead of reticulate for speed
  status <- system2(command = "python3", args = c(script_path, parameters))
}

