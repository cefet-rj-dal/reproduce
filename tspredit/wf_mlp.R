#source("wf_experiment.R")

load("data/fertilizers.RData")

create_directories()

params <- list(
  sw_size = 8,
  input_size = 3:7,
  ranges = list(size = 1:10, decay = seq(0, 1, 1/9), maxit=10000),
  filter = ts_fil_none(), #ts_smooth(), ts_awareness(0.80), ts_aware_smooth(0.80)
  preprocess = list(ts_norm_an()), #ts_norm_an(), ts_norm_ean(), ts_norm_gminmax(), ts_norm_swminmax()
  augment = list(ts_aug_none())  #ts_aug_awareness(), ts_aug_flip(), ts_aug_jitter()
)

for (j in (1:length(fertilizers))) {
  filename <- sprintf("%s_%s", "mlp", names(fertilizers)[j])
  run_ml(x = fertilizers[[j]], filename = filename, base_model = ts_mlp(), train_size = 56, test_size = 4, params = params)
}  
