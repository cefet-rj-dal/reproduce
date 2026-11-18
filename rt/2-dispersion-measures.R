##
## 2 - Instantaneous Dispersion Measures (Volatility Proxies)
## ----------------------------------------------------------
## Provides rolling measures used to normalize RT: sd, var, EWMA,
## MAD, mean absolute deviation, range, IQR, GARCH sigma, realized vol,
## and a wavelet-based energy proxy.
##
##--- STANDARD DEVIATION
library("zoo")
vol_sd <- function(serie){
  rollapply(serie, 30, sd, by = 1, partial=TRUE)
}

##--- VARIANCE
vol_var <- function(serie){
  rollapply(serie, 30, var, by = 1, partial=TRUE)
}

##--- EWMA
library(pracma)
vol_ewma <- function(serie, lambda = 0.94) {
  n <- length(serie)
  ewma <- numeric(n)
  ewma[1] <- sd(serie, na.rm = TRUE)
  for (i in 2:n) {
    ewma[i] <- lambda * ewma[i - 1] + (1 - lambda) * serie[i - 1]^2
  }
  return(sqrt(ewma))
}

##--- MEDIAN ABSOLUTE DEVIATION (MAD)
vol_mad <- function(serie){
  rollapply(serie, 30, function(x) mad(x, constant = 1), partial = TRUE)
}

##--- MEAN ABSOLUTE DEVIATION
vol_meanad <- function(serie){
  rollapply(serie, 30, function(x) mean(abs(x - mean(x))), partial = TRUE)
}

##--- RANGE
vol_range <- function(serie){
  rollapply(serie, 30, function(x) diff(range(x)), by = 1, partial = TRUE)
}

##--- IQR
vol_iqr <- function(serie){
  rollapply(serie, 30, function(x) IQR(x), partial = TRUE)
}

##--- GARCH
library(rugarch)
vol_garch <- function(serie) {
  spec <- ugarchspec(variance.model = list(model = "sGARCH"),
                     mean.model = list(armaOrder = c(0, 0)))
  fit <- ugarchfit(spec = spec, data = serie)
  garch <- sigma(fit)
  garch <- data.frame(date=index(garch), coredata(garch))
  garch <- garch$coredata.garch
  
  return(garch)
}

##--- REALIZED VOLATILITY
vol_realized <- function(serie) {
  fc <- function(serie) {
    sqrt(sum(serie^2, na.rm = TRUE))
  }
   rollapply(serie, 30,fc , by = 1, partial=TRUE)
}

##--- WAVELET TRANSFORM
library(waveslim)

vol_wavelet <- function(serie, wavelet = "haar", max_level = NULL, adjust_scale = TRUE) {
  if (is.null(max_level)) {
    max_level <- floor(log2(length(serie)))
  }

  modwt_result <- modwt(serie, wf = wavelet, n.levels = max_level)
  
  detail_names <- names(modwt_result)[grepl("^d", names(modwt_result))]
  details <- modwt_result[detail_names]
  
  variances <- sapply(details, var)
  
  best_level <- which.max(variances)
  
  selected_details <- details[1:best_level]
  
  energy_matrix <- sapply(selected_details, function(d) abs(d))
  
  if (is.null(dim(energy_matrix))) {
    vol_instant <- energy_matrix
  } else {
    vol_instant <- rowSums(energy_matrix)
  }
  
  if (adjust_scale) {
    vol_instant <- vol_instant * (sd(serie)^2 / mean(vol_instant))
  }
  
  return(vol_instant)
}
