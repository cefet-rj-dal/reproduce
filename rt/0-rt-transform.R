library(hht)
library(daltoolbox)
library(zoo)
library(harbinger)

##
## 0 - RT Transform (Resilient Transformation)
## ------------------------------------------
## Purpose:
## - Decompose the input series via CEEMD
## - Select high-frequency IMFs using roughness + curvature split
## - Detrend and normalize by local dispersion (rolling sigma)
## - Return the RT-transformed series to highlight anomalies under heteroscedasticity
##
## Notes:
## - ws controls the window for dispersion; sigma may be sd/var/MAD etc.
## - Set a seed for reproducibility.
##
## Roughness function for IMF selection
fc_rug <- function(x){
  firstD = diff(x)
  normFirstD = (firstD - mean(firstD)) / sd(firstD)
  roughness = (diff(normFirstD) ** 2) / 4
  return(mean(roughness))
}

## Sum a range of IMFs
fc_somaIMF <- function(ceemd.result, inicio, fim){
  soma_imf <- rep(0, length(ceemd.result[["original.signal"]]))
  for (k in inicio:fim){
    soma_imf <- soma_imf + ceemd.result[["imf"]][,k]
  }
  return(soma_imf)
}

# RT transformation
fc_RT <- function(serie, noise = 0.001, trials = 5, ws = 30, sigma = sd){
  set.seed(123)
  
  id <- 1:length(serie)
  san_size <-  length(serie)
  
  ## calculate IMFs
  noise <- 0.001
  trials <- 5
  model_an <- hht::CEEMD(serie, id, verbose = FALSE, noise, trials)
  
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

    soma_an <- fc_somaIMF(model_an, 1, div)
  }
  
  # diff
  diff_soma <- c(NA, diff(soma_an))
  diff_soma[1] <- diff_soma[2]
  
  
  # detrend
  isEmpty <- function(x) {
    return(length(x)==0)
  }
  
  if(isEmpty(model_an$residue)){
    d_serie <- serie
  }else{
    d_serie <- serie-model_an$residue
  }

  ## calculate dispersion measure
  dm <- rollapply(d_serie, ws, sigma, by = 1, partial=TRUE)

  ## creating transformed serie 
  RED_transform <- diff_soma/dm
  
  return(RED_transform)
}

