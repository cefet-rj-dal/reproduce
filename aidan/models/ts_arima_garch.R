library(rugarch)
#'@title ARIMA-GARCH
#'@description Creates a time series prediction object that
#' uses the AutoRegressive Integrated Moving Average (ARIMA) for the mean
#' and GARCH(1,1) with constant mean for residual correction.
#' It wraps the forecast and rugarch libraries.
#'@return returns a `ts_arima_garch` object.
#'@examples
#'data(sin_data)
#'ts <- ts_data(sin_data$y, 0)
#'ts_head(ts, 3)
#'
#'samp <- ts_sample(ts, test_size = 5)
#'io_train <- ts_projection(samp$train)
#'io_test <- ts_projection(samp$test)
#'
#'model <- ts_arima_garch()
#'model <- fit(model, x=io_train$input, y=io_train$output)
#'
#'prediction <- predict(model, x=io_test$input[1,], steps_ahead=5)
#'prediction <- as.vector(prediction)
#'output <- as.vector(io_test$output)
#'
#'ev_test <- evaluate(model, output, prediction)
#'ev_test
#'@export
ts_arima_garch <- function() {
  obj <- ts_reg()
  class(obj) <- append("ts_arima_garch", class(obj))
  return(obj)
}

#'@import forecast
#'@import rugarch
#'@export
fit.ts_arima_garch <- function(obj, x, y = NULL, ...) {
  # Ajustar ARIMA
  obj$model <- forecast::auto.arima(x, allowdrift = TRUE, allowmean = TRUE)
  order <- obj$model$arma[c(1, 6, 2, 3, 7, 4, 5)]
  obj$p <- order[1]
  obj$d <- order[2]
  obj$q <- order[3]
  obj$drift <- (NCOL(obj$model$xreg) == 1) && is.element("drift", names(obj$model$coef))
  
  arima_residuals <- as.numeric(residuals(obj$model))
  obj$garch_model <- NULL
  
  suppressWarnings({
    garch_spec <- rugarch::ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
      distribution.model = "norm",
      fixed.pars = list(alpha1 = 0.1, beta1 = 0.85)
    )
    
    obj$garch_model <- rugarch::ugarchfit(
      spec = garch_spec,
      data = arima_residuals,
      solver = "solnp"
    )
  })
  
  if (obj$garch_model@fit$convergence != 0) {
    obj$garch_model <- NULL
  }
  
  params <- list(p = obj$p, d = obj$d, q = obj$q, drift = obj$drift)
  if (!is.null(obj$garch_model)) {
    garch_params <- as.list(obj$garch_model@fit$coef)
    names(garch_params) <- paste0("garch_", names(garch_params))
    params <- c(params, garch_params)
  }
  
  attr(obj, "params") <- params
  
  return(obj)
}

#'@import forecast
#'@import rugarch
#'@export
predict.ts_arima_garch <- function(object, x, y = NULL, steps_ahead = NULL, ...) {
  
  if (!is.null(x) && (length(object$model$x) == length(x)) && (sum(object$model$x - x) == 0)){
    pred_mean <- object$model$x - object$model$residuals
    if (is.null(steps_ahead))
      steps_ahead <- length(pred_mean)
  } else {
    if (is.null(steps_ahead))
      steps_ahead <- length(x)
    
    if ((steps_ahead == 1) && (length(x) != 1)) {
      pred_mean <- NULL
      model <- object$model
      i <- 1
      y_temp <- model$x
      
      while (i <= length(x)) {
        pred_mean <- c(pred_mean, forecast::forecast(model, h = 1)$mean)
        y_temp <- c(y_temp, x[i])
        
        model <- tryCatch(
          {
            forecast::Arima(y_temp, order = c(object$p, object$d, object$q), 
                            include.drift = object$drift)
          },
          error = function(cond) {
            forecast::auto.arima(y_temp, allowdrift = TRUE, allowmean = TRUE)
          }
        )
        i <- i + 1
      }
    } else {
      pred_mean <- forecast::forecast(object$model, h = steps_ahead)$mean
    }
  }
  
  pred <- pred_mean
  
  if (!is.null(object$garch_model) && inherits(object$garch_model, "uGARCHfit")) {
    garch_forecast <- rugarch::ugarchforecast(
      fitORspec = object$garch_model,
      n.ahead = steps_ahead
    )
    pred_residual <- as.vector(rugarch::fitted(garch_forecast))
    pred <- pred_mean + pred_residual
  }
  
  return(pred)
}
