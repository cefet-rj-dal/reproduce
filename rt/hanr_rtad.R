##
## RTAD (Resilient Transformation Anomaly Detector) for Harbinger
## --------------------------------------------------------------
## - Detects anomalies by applying the RT transform and thresholding
## - Uses CEEMD to decompose, selects IMFs via roughness/curvature, detrends
## - Normalizes by local dispersion (rollapply with sigma function)
## - Feeds transformed series to Harbinger’s distance + outlier checks

library(hht)
library(daltoolbox)
library(zoo)
library(harbinger)

## Roughness function used in IMF selection
fc_rug <- function(x){
  firstD = diff(x)
  normFirstD = (firstD - mean(firstD)) / sd(firstD)
  roughness = (diff(normFirstD) ** 2) / 4
  return(mean(roughness))
}

## Sum IMFs in a range
fc_somaIMF <- function(ceemd.result, inicio, fim){
  soma_imf <- rep(0, length(ceemd.result[["original.signal"]]))
  for (k in inicio:fim){
    soma_imf <- soma_imf + ceemd.result[["imf"]][,k]
  }
  return(soma_imf)
}

hanr_rtad <- function(noise = 0.001, trials = 5, ws = 30, sigma = sd) {
  obj <- harbinger()
  obj$noise <- noise
  obj$trials <- trials
  obj$ws <- ws
  obj$sigma <- sigma
  
  class(obj) <- append("hanr_red", class(obj))
  return(obj)
}

detect.hanr_red <- function(obj, serie, ...) {
  set.seed(123)
  
  if (is.null(serie))
    stop("No data was provided for computation", call. = FALSE)
  
  obj <- obj$har_store_refs(obj, serie)
  
  id <- 1:length(obj$serie)
  san_size <-  length(obj$serie)
  
  ## calculate IMFs
  suppressWarnings(ceemd.result <- hht::CEEMD(obj$serie, id, verbose = FALSE, obj$noise, obj$trials))
  
  model_an <- ceemd.result
  
  if (model_an$nimf < 4){
    soma_an <- obj$serie - model_an$residue
  }else{
    ## calculate roughness for each imf
    vec <- vector()
    for (n in 1:model_an$nimf){
      vec[n] <- fc_rug(model_an[["imf"]][,n])
    }
    
    ## Maximum curvature
    res <- daltoolbox::transform(daltoolbox::fit_curvature_max(), vec)
    div <- res$x
    
    ## sum IMFs
    soma_an <- fc_somaIMF(model_an, 1, div)
  }
  
  # diff
  diff_soma <- c(NA, diff(soma_an))
  diff_soma[1] <- diff_soma[2]
  
  
  ## dtrend
  isEmpty <- function(x) {
    return(length(x)==0)
  }
  
  if(isEmpty(model_an$residue)){
    d_serie <- obj$serie
  }else{
    d_serie <- obj$serie-model_an$residue
  }
  
  ##Calcula volatilidade instantÃ¢nea
  dm <-  rollapply(d_serie, obj$ws, obj$sigma, by = 1, partial=TRUE)
  
  ## Criando vetor de anomalias
  RED_transform <- diff_soma/dm

  res <- obj$har_distance(RED_transform)
  
  anomalies <- obj$har_outliers(res)
  anomalies <- obj$har_outliers_check(anomalies, res)
  
  detection <- obj$har_restore_refs(obj, anomalies = anomalies, res = res)
  
  return(detection)
}
