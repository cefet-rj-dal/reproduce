## 4-modeling (Lag-LLaMA) — workflow
# Purpose: run Lag-LLaMA in zero-shot and fine-tuning settings across datasets.
# Dependencies: requires helper functions from wf_experiment.R and the R/Python bridge.

source("wf_experiment.R")
source(paste(dirname(getwd()), "llama/lag-llama/scripts/ts_lag_llama.R", sep="/"))

datasets <- c('climate', 'emissions', 'emissions-co2', 'fertilizers', 'gdp', 'pesticides')
test_size <- 5
samples <- c(30, 60, 90, 200, 500, 1000)

# Zero-shot runs
for (ds in datasets) {
  create_directories(sub('-.*', '', ds))
  df <- read.csv(sprintf('%s/input/%s.csv', sub('-.*','',ds), ds))
  for (j in (1:length(df))) {
    ts <- names(df)[j]
    filename <- sprintf('%s/%s_%s', sub('-.*','',ds), ts, 'llama-zeroshot')
    print(filename)
    run_llm(x=df[[j]], filename, ts_lag_llama(), test_size=test_size, samples=samples)
  }
}

# Fine-tune runs (uncomment to enable)
# samples <- c(60)
# for (ds in datasets) {
#   create_directories(sub('-.*', '', ds))
#   df <- read.csv(sprintf('%s/input/%s.csv', sub('-.*','',ds), ds))
#   for (j in (1:length(df))) {
#     ts <- names(df)[j]
#     filename <- sprintf('%s/%s_%s', sub('-.*','',ds), ts, 'llama-finetune')
#     print(filename)
#     run_llm(x=df[[j]], filename, ts_lag_llama(), test_size=test_size, samples=samples, prediction_type="finetune")
#   }
# }

