source("wf_experiment.R")

load("data/fertilizers.RData")

create_directories()

for (j in (1:length(fertilizers))) {
  filename <- sprintf("%s_%s", "arima", names(fertilizers)[j])
  run_ml(x = fertilizers[[j]], filename = filename, base_model = ts_arima(), train_size = 56, test_size = 4)
}  
  