source('models/wf_experiment.R')

datasets <- list(
  list(path='bioenergy',    n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='climate',      n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='emissions',    n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='fertilizers',  n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='gdp',          n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='inflation',    n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='pesticides',   n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='m1-yearly',    n=10, test_size= 6, sw_size=c(6,8)), #Y21, Y121
  list(path='m3-yearly',    n=10, test_size= 6, sw_size=c(6,8)), #Y1, Y144, Y73
  list(path='m4-yearly',    n=10, test_size= 6, sw_size=c(6,8)), #Y12778, Y23000
  list(path='m1-quarterly', n=10, test_size= 8, sw_size=c(8,12)), #Q68
  list(path='m3-quarterly', n=10, test_size= 8, sw_size=c(8,12)), #Q504
  list(path='m3-other',     n=10, test_size= 8, sw_size=c(8,12)),
  list(path='m4-quarterly', n=10, test_size= 8, sw_size=c(8,12)),
  list(path='m1-monthly',   n=10, test_size=18, sw_size=c(6,12)),
  list(path='m3-monthly',   n=10, test_size=18, sw_size=c(6,12)),
  list(path='m4-monthly',   n=10, test_size=18, sw_size=c(6,12)),
  list(path='m4-weekly',    n=10, test_size=13, sw_size=c(14,28)),
  list(path='m4-daily',     n=10, test_size=14, sw_size=c(14,28)),
  list(path='m4-hourly',    n=10, test_size=48, sw_size=c(14,28)) #H276
)

preprocess <- list(ts_norm_diff())
augment <- list(ts_aug_none())
ranges <- list(k=c(3:8), algorithm=c('kd_tree', 'cover_tree', 'brute'))
params <- list(preprocess=preprocess, augment=augment, ranges=ranges)

for (ds in datasets) {
  create_directories(ds$path)
  df <- read.csv(sprintf('%s/input/%s.csv', ds$path, ds$path), header=FALSE, row.names=1)
  indices <- round(seq(1, nrow(df), length.out=ifelse(is.na(ds$n), nrow(df), ds$n)))
  params$sw_size=ds$sw_size
  
  for (i in indices) {
    filename <- sprintf('%s/%s-%s', ds$path, rownames(df)[i], 'knn')
    ts <- get_time_series(df[i, ], ds$test_size)
    run(ts, filename, ts_knn(), test_size=ds$test_size, params=params, cv=TRUE)
  }
}
