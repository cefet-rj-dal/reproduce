##
## 1 - Run Example Experiments (3W Oil Wells)
## ------------------------------------------
## Purpose:
## - Load a sample 3W well dataset from `dalevents`
## - Prepare features/labels and handle basic NA filling
## - Call `run_experiments(...)` to execute the configured combinations
## - Save results under a configured `results_path`
##
## Notes:
## - Hard-coded paths point to a user environment; adjust `load_all(...)`,
##   `source(...)` and `results_path` to your local setup or replace with
##   package-installed versions where available.
##

start_time <- Sys.time()
library(devtools)
library("dplyr")
library('ggplot2')
library('reticulate')
library('pROC')
library(data.table)
library(dplyr)
# devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)
library(dalevents)
library(caret)
# Local development helpers (optional): if you keep helper sources under `core/`,
# you can source them here. Otherwise, ensure required functions are available
# via installed packages.
# source('core/core.R')

# Where experiment CSV results will be written (relative to repo)
results_path <- 'results/'
data(oil_3w_Type_2)

df <- as.data.frame(oil_3w_Type_2$Type_2$`WELL-00002_20131104014101`)

str(df)

# Create a batch index feature (example)
df['batch_index'] <- as.numeric(rownames(df)) # alternative: floor(order/10)

## Target label
target = 'class'
df[target] <- factor(df[['class']])
slevels <- levels(df[[target]])

# Fill NA: set missing labels to 0 and NAs in features to 0
df[is.na(df[target]), target] <- 0
df[is.na(df)] <- 0

# Evaluation threshold (example)
th=0.5

# Feature selection (example subset)
features <- c(
  #"P_PDG", # All Zeros
  "P_TPT",
  # "T_TPT",
  "P_MON_CKP"
  # "T_JUS_CKP",
  # "P_JUS_CKGL"
  #'T_JUS_CKGL',# All NaN
  #"QGL"# All zeros
)

# Run configured experiments/combinations for this data frame
run_experiments(df, th, results_path)
