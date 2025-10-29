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
load_all('/home/lucas/heimdall/')
load_all('/home/lucas/daltoolbox/')
source('/home/lucas/3w_experiments/core.R')
source(url("https://raw.githubusercontent.com/lucasgiutavares/3w_experiments/refs/heads/main/core.R?token=GHSAT0AAAAAAC37QWIS664SKOCXZOE4US5CZ3AFLEA"))

results_path <- '//home/lucas/ijcnn2025/3w/results/'
data(oil_3w_Type_2)

df <- as.data.frame(oil_3w_Type_2$Type_2$`WELL-00002_20131104014101`)

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
  #"P_PDG", # All Zeros
  "P_TPT",
  # "T_TPT",
  "P_MON_CKP"
  # "T_JUS_CKP",
  # "P_JUS_CKGL"
  #'T_JUS_CKGL',# All NaN
  #"QGL"# All zeros
)

run_experiments(df, th, results_path)