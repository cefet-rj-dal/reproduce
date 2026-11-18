## TSPredIT workflow helpers
# Purpose: Integrated tuning of preprocessing and prediction for time series.
# This file defines utilities to create experiment directories, configure
# experiments, train models with optional tuning, and evaluate predictions.

library(daltoolbox)
library(tspredit)
library(stringi)
library(dplyr)
library(stringr)

create_directories <- function() {
  # Ensure folders for hyperparameters, plots, and results exist
  if (!file.exists('hyper')) dir.create('hyper')
  if (!file.exists('graphics')) dir.create('graphics')
  if (!file.exists('results')) dir.create('results')
}

run_ml <- function(x, filename, base_model, train_size, test_size, params = list()) {
  # High-level runner: configures, trains, and evaluates a model for increasing
  # rolling-origin (RO) windows of size 1..test_size.
  myexp <- wf_experiment(filename, base_model, train_size)
  myexp <- set_params(myexp, params)
  myexp <- train(myexp, x)

  results <- NULL
  for (j in 1:test_size) {
    result <- test(myexp, x, test_pos = train_size + 1, test_size = j, steps_ahead = 1, ro = TRUE)
    results <- rbind(results, result)
  }
  filename <- sprintf("results/%s.Rdata", filename)
  save(results, file=filename)
}

describe <- function(obj) {
  # Helper: provide a short class-based description for logging/results
  if (is.null(obj)) return("")
  return(as.character(class(obj)[1]))
}

# class wf_experiment

wf_experiment <- function(filename, base_model, train_size,
           sw_size = 0,
           input_size = c(1),
           filter = ts_fil_none(),
           preprocess = list(ts_norm_none()),
           augment = list(ts_aug_none()),
           ranges = list()) {
  # Factory: assemble an experiment object with preprocessing, augmentation,
  # filter, and (optionally) tuning ranges for the base model.
  obj <- dal_transform()
  obj$filename <- filename
  obj$base_model <- base_model
  obj$sw_size <- sw_size
  obj$input_size <- input_size
  obj$train_size <- train_size
  obj$filter <- filter
  obj$preprocess <- preprocess
  obj$augment <- augment
  obj$ranges <- ranges

  class(obj) <- append("wf_experiment", class(obj))
  return(obj)
}

train <- function(obj, x) {
  # Build training description strings
  obj$desc_input_size <- stri_paste(as.character(obj$input_size), collapse = '_')
  obj$desc_preprocess <- stri_paste(sapply(obj$preprocess, function(x) { as.character(describe(x)) }), collapse='_')
  obj$desc_augment <- stri_paste(sapply(obj$augment, function(x) { as.character(describe(x)) }), collapse='_')

  # Take training portion and remove missing values
  x <- x[1:obj$train_size]
  x <- na.omit(x)

  # Keep labels and last observed index (if named)
  obj$train <- x[obj$sw_size:length(x)]
  obj$obs <- as.character(obj$train)
  if (length(names(obj$train)) > 0)
    obj$obs <- names(obj$train)[length(obj$train)]

  # Apply optional filter and make sliding windows -> inputs/outputs
  xw <- ts_data(as.vector(x), obj$sw_size)
  xw <- transform(obj$filter, xw)
  xy <- ts_projection(xw)

  # Either perform integrated tuning (maintune) with windows or direct fit
  if (obj$sw_size != 0) {
    mytune <- ts_maintune(obj$input_size, obj$base_model, preprocess = obj$preprocess, augment = obj$augment)
    obj$model <- fit(mytune, xy$input, xy$output, obj$ranges)
    obj$input_size <- obj$model$input_size
    obj$preprocess <- obj$model$preprocess
    obj$augment <- attr(obj$model, "augment")
  } else {
    obj$model <- fit(obj$base_model, x=xy$input, y=xy$output)
    obj$input_size <- 0
    obj$preprocess <- ts_norm_none()
    obj$augment <- ts_aug_none()
  }

  # Compute in-sample adjustments and evaluation
  xw <- ts_data(as.vector(x), obj$sw_size)
  xy <- ts_projection(xw)

  obj$adjust <- as.vector(predict(obj$model, xy$input))
  obj$ev_adjust <- evaluate(obj$model, as.vector(xy$output), obj$adjust)

  # Persist tuned hyperparameters and params if available
  hyperparameters <- attr(obj$model, "hyperparameters")
  if (!is.null(hyperparameters))
    save(hyperparameters, file=tolower(sprintf("hyper/%s-hparams.rdata", obj$filename)))
  params <- attr(obj$model, "params")
  if (!is.null(params))
    save(params, file=tolower(sprintf("hyper/%s-params.rdata", obj$filename)))

  return(obj)
}

test <- function(obj, x, test_pos, test_size, steps_ahead = 1, ro = TRUE) {
  # Build test window and inputs
  obj$test <- x[test_pos:(test_pos+test_size-1)]
  if (obj$sw_size != 0)
    xwt <- ts_data(as.vector(x[(test_pos-obj$sw_size+1):(test_pos+test_size-1)]), obj$sw_size)
  else
    xwt <- ts_data(as.vector(x[test_pos:(test_pos+test_size-1)]), obj$sw_size)

  xyt <- ts_projection(xwt)

  # Predict 1-step or multi-step-ahead
  output <- as.vector(xyt$output)
  if (steps_ahead == 1)  {
    obj$prediction <- predict(obj$model, x=xyt$input, steps_ahead=steps_ahead)
  } else {
    obj$prediction <- predict(obj$model, x=xyt$input[1,], steps_ahead=steps_ahead)
    output <- output[1:steps_ahead]
  }
  obj$prediction <- as.vector(obj$prediction)

  # Evaluate prediction and prepare result row
  obj$ev_prediction <- evaluate(obj$model, output, obj$prediction)
  smape_train <- 100*obj$ev_adjust$smape
  smape_test <- 100*obj$ev_prediction$smape

  result <- data.frame(name = obj$filename,
                       method = describe(obj$base_model),
                       model = describe(obj$model),
                       filter = describe(obj$filter),
                       preprocess = describe(obj$preprocess),
                       input_size = obj$input_size,
                       window_size = obj$sw_size,
                       augment = describe(obj$augment),
                       obs = obj$obs,
                       test_size = test_size,
                       steps_ahead = steps_ahead,
                       smape_train = smape_train,
                       smape_test = smape_test)

  # Save informative plots for RO or SA scenarios
  if (ro) {
    imagename <- tolower(sprintf("%s-ro%d.jpg", obj$filename, test_size))
  } else {
    imagename <- tolower(sprintf("%s-sa%d.jpg", obj$filename, steps_ahead))
  }
  save_image(obj, imagename)
  return(result)
}

save_image <- function(obj, imagename) {
  # Persist a simple prediction plot to the graphics/ folder
  jpeg(sprintf("graphics/%s", imagename), width = 640, height = 480)
  yvalues <- c(obj$train, obj$test)
  xlabels <- 1:length(yvalues)
  if(!is.null(names(yvalues))) xlabels <- as.integer(names(yvalues))
  grf <- plot_ts_pred(x = xlabels, y = yvalues, yadj=obj$adjust, ypre=obj$prediction)
  plot(grf)
  dev.off()
}

