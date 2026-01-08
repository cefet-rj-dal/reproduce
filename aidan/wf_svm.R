source('models/wf_experiment.R')

datasets <- c('bioenergy', 'climate', 'emissions', 'fertilizers', 'gdp', 'pesticides')
test_size <- 8

sw_size <- c(6, 8, 10)
preprocess <- list(ts_norm_gminmax(), ts_norm_swminmax(), ts_norm_an(), ts_norm_ean(), ts_norm_diff())
augment <- list(ts_aug_none(), ts_aug_jitter(), ts_aug_awareness(), ts_aug_flip(), ts_aug_shrink(), ts_aug_stretch())
ranges <- list(kernel=c('linear', 'radial', 'sigmoid'), epsilon=seq(0.1, 1, 0.1), cost=seq(10, 100, 10))
params <- list(sw_size=sw_size, preprocess=preprocess, augment=augment, ranges=ranges)

for (ds in datasets) {
  create_directories(sub('-.*', '', ds))
  df <- getExportedValue('tspredbench', ds)
  for (ts in names(df)) {
    filename <- sprintf('%s/%s_%s', sub('-.*', '', ds), ts, 'svr')
    run(as.numeric(df[[ts]]), filename, ts_svm(), test_size=test_size, params=params, cv=FALSE)
  }
}
