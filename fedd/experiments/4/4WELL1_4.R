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
# source('core/core.R')

set.seed(1)

data_path <- 'data/'
results_path <- 'results/'
event_type <- '4'
example <- 'WELL1_3'
load(paste0(data_path, 'data_3w_tp4_real_sample_exp.RData'))

all_features <- c(
  "P_PDG",
  "P_TPT",
  "T_TPT",
  "P_MON_CKP",
  # "T_JUS_CKP",
  "P_JUS_CKGL",
  # "T_JUS_CKGL",
  "QGL",
  'class'
)

df <- as.data.frame(data_3w_tp4_real_sample_exp$`WELL-00001_20170316170000`)
names(df) <- gsub('-', '_', names(df))
df <- df[, all_features]

str(df)

df['batch_index'] <- as.numeric(rownames(df))#as.integer(unlist(df['order']/10))

# Target Fill NA
target = 'class'
df[is.na(df[target]), target] <- 0
df[is.na(df)] <- 0

## Target
df[target] <- factor(df[['class']])#factor(c(rep(c(0,1),times=nrow(df)/2), 0))
slevels <- levels(df[[target]])

# Evaluation
th=0.5

# Feature Selection
features <- c(
  # "P_PDG",
  "P_TPT",
  "T_TPT",
  "P_MON_CKP",
  # "T_JUS_CKP",
  "P_JUS_CKGL"
  # "T_JUS_CKGL",
  # "QGL"
)

# Manual Features Selection
# plot(x=df[,'batch_index'], y=df[ ,features[8]])
plot(x=df[,'batch_index'], y=df[ , 'class'])

# Manual Setting Fixed Min Max
# max(df[ ,features[1]])
# min(df[ ,features[1]])

minmax_list <- list(
  # P_PDG = list(min=0, max=10^10),
  P_TPT = list(min=0, max=10^10),
  T_TPT = list(min=0, max=10^3),
  P_MON_CKP = list(min=0, max=10^10),
  # T_JUS_CKP = list(min=0, max=10^3),
  P_JUS_CKGL = list(min=0, max=10^10)
  # T_JUS_CKGL = list(min=0, max=10^10),
  # QGL = list(min=0, max=10^2)
)

run_experiments(df, features, th, results_path, event_type = event_type, example = example, minmax_list = minmax_list)
