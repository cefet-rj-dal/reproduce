source('models/wf_experiment.R')
source('~/chronos/zero shot/ts_chronos.R')

datasets <- list(
  list(path='bioenergy',    n=NA, test_size= 5),
  list(path='climate',      n=NA, test_size= 5),
  list(path='emissions',    n=NA, test_size= 5),
  list(path='fertilizers',  n=NA, test_size= 5),
  list(path='gdp',          n=NA, test_size= 5),
  list(path='inflation',    n=NA, test_size= 5),
  list(path='pesticides',   n=NA, test_size= 5),
  list(path='m1-yearly',    n=10, test_size= 6),
  list(path='m3-yearly',    n=10, test_size= 6),
  list(path='m4-yearly',    n=10, test_size= 6),
  list(path='m1-quarterly', n=10, test_size= 8),
  list(path='m3-quarterly', n=10, test_size= 8),
  list(path='m3-other',     n=10, test_size= 8),
  list(path='m4-quarterly', n=10, test_size= 8),
  list(path='m1-monthly',   n=10, test_size=18),
  list(path='m3-monthly',   n=10, test_size=18),
  list(path='m4-monthly',   n=10, test_size=18),
  list(path='m4-weekly',    n=10, test_size=13),
  list(path='m4-daily',     n=10, test_size=14),
  list(path='m4-hourly',    n=10, test_size=48)
)

for (ds in datasets) {
  create_directories(ds$path)
  df <- read.csv(sprintf('%s/input/%s.csv', ds$path, ds$path), header=FALSE, row.names=1)
  indices <- round(seq(1, nrow(df), length.out=ifelse(is.na(ds$n), nrow(df), ds$n)))
  
  for (i in indices) {
    filename <- sprintf('%s/%s-%s', ds$path, rownames(df)[i], 'chronos')
    ts <- get_time_series(df[i, ], ds$test_size)
    run_llm(ts, filename, ts_chronos(), test_size=ds$test_size, params='amazon/chronos-t5-tiny')
  }
}
