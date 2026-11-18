source('./models/wf_experiment.R')
source('~/models/ts_te-lstm.R')
library(purrr)

get_dim <- function(sw_size) {
  m <- max(sw_size)
  if (m <= 8) return(c(16))
  if (m <= 16) return(c(32))
  if (m <= 28) return(c(64))
  #return(c(16,32,64,128,256))
}

datasets <- list(
  list(path='bioenergy',    n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='climate',      n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='emissions',    n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='fertilizers',  n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='gdp',          n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='inflation',    n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='pesticides',   n=NA, test_size= 5, sw_size=c(6,8)),
  list(path='m1-yearly',    n=10, test_size= 6, sw_size=c(6,8)),
  list(path='m3-yearly',    n=10, test_size= 6, sw_size=c(6,8)),
  list(path='m4-yearly',    n=10, test_size= 6, sw_size=c(6,8)),
  list(path='m1-quarterly', n=10, test_size= 8, sw_size=c(8,12)),
  list(path='m3-quarterly', n=10, test_size= 8, sw_size=c(8,12)),
  list(path='m3-other',     n=10, test_size= 8, sw_size=c(8,12)),
  list(path='m4-quarterly', n=10, test_size= 8, sw_size=c(8,12)),
  list(path='m1-monthly',   n=10, test_size=18, sw_size=c(6,12)),
  list(path='m3-monthly',   n=10, test_size=18, sw_size=c(6,12)),
  list(path='m4-monthly',   n=10, test_size=18, sw_size=c(6,12)),
  list(path='m4-weekly',    n=10, test_size=13, sw_size=c(14,28)),
  list(path='m4-daily',     n=10, test_size=14, sw_size=c(14,28)),
  list(path='m4-hourly',    n=10, test_size=48, sw_size=c(14,28))
)

preprocess <- list(ts_norm_diff())
augment <- list(ts_aug_none())
ranges <- list(epochs=1000, transform='pos')
params <- list(preprocess=preprocess, augment=augment, ranges=ranges)

for (ds in datasets) {
  create_directories(ds$path)
  df <- read.csv(sprintf('%s/input/%s.csv', ds$path, ds$path), header=FALSE, row.names=1)
  indices <- round(seq(1, nrow(df), length.out=ifelse(is.na(ds$n), nrow(df), ds$n)))
  params$sw_size=ds$sw_size
  params$ranges$dim=get_dim(ds$sw_size)
  
  for (i in indices) {
    filename <- sprintf('%s/%s-%s', ds$path, rownames(df)[i], 'lstmpos')
    ts <- get_time_series(df[i, ], ds$test_size)
    run(ts, filename, ts_telstm(), test_size=ds$test_size, params=params, cv=TRUE)
  }
}

