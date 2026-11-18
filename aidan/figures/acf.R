library(forecast)
library(tspredit)
library(daltoolbox)

to_vector <- function(mat) {
  c(mat[1, ], mat[-1, ncol(mat)])
}

datasets <- c('bioenergy', 'climate', 'emissions', 'fertilizers', 'gdp', 'pesticides')
lags <- c(1)  # lag-1 autocorrelation for annual data

augments <- list(
  none      = ts_aug_none(),
  awareness = ts_aug_awareness(),
  jitter    = ts_aug_jitter(),
  flip      = ts_aug_flip(),
  shrink    = ts_aug_shrink(),
  stretch   = ts_aug_stretch()
)

summary_rows <- list()

for (aug_name in names(augments)) {
  
  acf_before_list <- list()
  acf_after_list  <- list()
  idx <- 1
  augment <- augments[[aug_name]]
  
  for (ds in datasets) {
    df <- getExportedValue('tspredbench', ds)
    
    for (ts in names(df)) {
      serie <- as.numeric(df[[ts]])
      
      if (length(serie) < max(lags)+1) next
      # ACF before
      acf_b <- as.numeric(Acf(serie, lag.max = max(lags), plot = FALSE)$acf[lags+1])
      # Fit and apply augmentation
      xw <- ts_data(serie, 8)
      aug <- fit(augment, xw)
      xa  <- transform(aug, xw)
      # ACF after
      serie_aug <- to_vector(xa)
      acf_a <- as.numeric(Acf(serie_aug, lag.max = max(lags), plot = FALSE)$acf[lags+1])
      acf_before_list[[idx]] <- acf_b
      acf_after_list[[idx]]  <- acf_a
      idx <- idx + 1
    }
  }
  
  acf_before <- do.call(rbind, acf_before_list)
  acf_after  <- do.call(rbind, acf_after_list)
  colnames(acf_before) <- paste0('lag_', lags)
  colnames(acf_after)  <- paste0('lag_', lags)
  
  row <- data.frame(
    augment      = aug_name,
    median_before = median(acf_before[,1], na.rm = TRUE),
    iqr_before    = IQR(acf_before[,1], na.rm = TRUE),
    median_after  = median(acf_after[,1],  na.rm = TRUE),
    iqr_after     = IQR(acf_after[,1],  na.rm = TRUE),
    median_diff   = median(acf_after[,1] - acf_before[,1], na.rm = TRUE)
  )
  summary_rows[[aug_name]] <- row
}

summary_df <- do.call(rbind, summary_rows)
rownames(summary_df) <- NULL
write.csv(
  summary_df,
  file = 'figures/summary_acf.csv',
  row.names = FALSE
)