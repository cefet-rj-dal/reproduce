#'@title Simple Exponential Smoothing
#'@description Creates a time series prediction object that
#' uses Simple Exponential Smoothing (SES).
#' It wraps the forecast library.
#'@return returns a `ts_ses` object.
#'@examples
#'data(sin_data)
#'ts <- ts_data(sin_data$y, 0)
#'ts_head(ts, 3)
#'
#'samp <- ts_sample(ts, test_size = 5)
#'io_train <- ts_projection(samp$train)
#'io_test <- ts_projection(samp$test)
#'
#'model <- ts_ses()
#'model <- fit(model, x=io_train$input, y=io_train$output)
#'
#'prediction <- predict(model, x=io_test$input[1,], steps_ahead=5)
#'prediction <- as.vector(prediction)
#'output <- as.vector(io_test$output)
#'
#'ev_test <- evaluate(model, output, prediction)
#'ev_test
#'@export
ts_ses <- function() {
  obj <- ts_reg()
  class(obj) <- append("ts_ses", class(obj))
  return(obj)
}

#'@import forecast
#'@export
fit.ts_ses <- function(obj, x, y = NULL, ...) {
  
  obj$model <- forecast::ses(as.numeric(x), h = 1)
  obj$alpha <- obj$model$model$par["alpha"]
  params <- list(alpha = obj$alpha)
  attr(obj, "params") <- params
  
  return(obj)
}

#'@import forecast
#'@export
predict.ts_ses <- function(object, x, y = NULL, steps_ahead = NULL, ...) {
  
  if (!is.null(x) && (length(object$model$x) == length(x)) && (sum(object$model$x - x) == 0)){
    pred <- fitted(object$model)
    if (is.null(steps_ahead))
      steps_ahead <- length(pred)
  } else {
    if (is.null(steps_ahead))
      steps_ahead <- length(x)
    
    if ((steps_ahead == 1) && (length(x) != 1)) {
      pred <- NULL
      y_temp <- object$model$x
      
      for (i in 1:length(x)) {
        ses_model <- forecast::ses(as.numeric(y_temp), h = 1)
        pred <- c(pred, ses_model$mean[1])
        y_temp <- c(y_temp, as.numeric(x[i]))
      }
    } else {
      level <- object$model$model$state[length(object$model$model$state)]
      pred <- rep(level, steps_ahead)
    }
  }
  
  return(pred)
}
