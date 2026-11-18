source('models/wf_experiment.R')

datasets <- c('bioenergy', 'climate', 'emissions', 'fertilizers', 'gdp', 'pesticides')
test_size <- 8

for (ds in datasets) {
  create_directories(sub('-.*', '', ds))
  df <- getExportedValue('tspredbench', ds)
  for (ts in names(df)) {
    filename <- sprintf('%s/%s_%s', sub('-.*', '', ds), ts, 'arima')
    run(as.numeric(df[[ts]]), filename, ts_arima(), test_size=test_size)
  }
}
