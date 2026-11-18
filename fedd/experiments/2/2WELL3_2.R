start_time <- Sys.time()
library(devtools)
library("dplyr")
library('ggplot2')
library('reticulate')
py_set_seed(1, disable_hash_randomization = TRUE)
library('pROC')
library(data.table)
library(dplyr)
# devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)
library(dalevents)
library(caret)
## Optional local helpers
# source('core/best_combinations.R')
# source(url("https://raw.githubusercontent.com/lucasgiutavares/3w_experiments/refs/heads/main/core.R?token=GHSAT0AAAAAAC37QWIS664SKOCXZOE4US5CZ3AFLEA"))

set.seed(1)

results_path <- 'results/'
event_type <- '2'
example <- 'WELL3_2'
data(oil_3w_Type_2)

df <- as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20170728150240`)

# read_parquet('<datasets_path>/timeseries/3w/4/WELL-00002_20131104004101.parquet')

str(df)

df['batch_index'] <- as.numeric(rownames(df))#as.integer(unlist(df['order']/10))

## Target
target = 'class'
df[target] <- factor(df[['class']])#factor(c(rep(c(0,1),times=nrow(df)/2), 0))
slevels <- levels(df[[target]])

# Fill NA
df[is.na(df[target]), target] <- 0
df[is.na(df)] <- 0

# Evaluation
th=0.5

# Feature Selection
features <- c(
  "P_PDG", # All Zeros
  "P_TPT",
  "T_TPT",
  "P_MON_CKP"
  # "T_JUS_CKP",
  # "P_JUS_CKGL",
  # "T_JUS_CKGL", # All NaN
  # "QGL"# All zeros
)

# Manual Features Selection
# plot(x=df[,'batch_index'], y=df[ ,features[1]])

# Setting Fixed Min Max
# max(df[ ,features[4]])
# min(df[ ,features[4]])

minmax_list <- list(
  P_PDG = list(min=0, max=10^10),
  P_TPT = list(min=0, max=10^10),
  T_TPT = list(min=0, max=10^10),
  P_MON_CKP = list(min=0, max=10^10)
)

run_experiments(df, features, th, results_path, event_type = event_type, example = example, minmax_list = minmax_list)
