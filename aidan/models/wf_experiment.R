#-----------------------------------------------
#------------------ LIBRARIES ------------------
library(daltoolbox)
library(tspredit)
library(tspredbench)
library(daltoolboxdp)
library(stringi)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)
library(devtools)
set.seed(120770)
#-----------------------------------------------


#-----------------------------------------------
#-------------- GENERAL FUNCTIONS --------------
create_directories <- function(path) {
  dir_name <- c('results', 'hyper', 'graphics')
  for (name in sprintf('%s/%s', path, dir_name)) {
    if (!file.exists(name))
      dir.create(name, recursive = TRUE)
  }
}


describe <- function(obj) {
  if (is.null(obj))
    return('')
  else
    return(as.character(class(obj)[1]))
}


get_preprocess <- function(preprocess) {
  func_map <- list( ts_norm_gminmax  = ts_norm_gminmax,
                    ts_norm_swminmax = ts_norm_swminmax,
                    ts_norm_an       = ts_norm_an,
                    ts_norm_ean      = ts_norm_ean,
                    ts_norm_diff     = ts_norm_diff )
  return(func_map[[preprocess]]) 
}


get_combinations <- function(params=NULL) {
  combinations <- expand.grid(preprocess=params$preprocess, augment=params$augment,stringsAsFactors=FALSE)
  result <- list()
  for (i in 1:nrow(combinations)) {
    item <- list(
      preprocess = combinations$preprocess[i],
      augment    = combinations$augment[i],
      sw_size    = params$sw_size,
      ranges     = params$ranges
    )
    result <- append(result, list(item))
  }
  return(result)
}


get_names <- function(filename, params) {
  dn <- gsub('_', '-', gsub('ts_', '', describe(params$preprocess[[1]])))
  da <- gsub('_', '-', gsub('ts_', '', describe(params$augment[[1]])))
  filename <- paste(filename, dn, da, sep='_')
  return (filename)
}


save_error <- function(filename, e) {
  print('error')
  error_dir <- sprintf('error/%s', sub('/.*', '', filename))
  if (!dir.exists(error_dir)) dir.create(error_dir, recursive=TRUE)
  error_file <- sprintf('error/%s.txt', filename)
  writeLines(as.character(e), error_file)
}


wf_experiment <- function( filename, base_model,
                           sw_size = c(0),
                           input_size = c(0),
                           preprocess = list(ts_norm_none()),
                           augment = list(ts_aug_none()),
                           ranges = list() ) {
  obj <- dal_transform()
  obj$filename <- filename
  obj$base_model <- base_model
  obj$sw_size <- sw_size
  obj$input_size <- input_size
  obj$preprocess <- preprocess
  obj$augment <- augment
  obj$ranges <- ranges
  class(obj) <- append('wf_experiment', class(obj))
  return(obj)
}


compute_performance <- function(model, true, pred) {
  test_size <- length(pred)
  mse <- smape <- r2 <- NULL
  for (j in 1:test_size) {
    performance <- evaluate(model, true[1:j], pred[1:j])
    mse <- c(mse, performance$mse)
    smape <- c(smape, performance$smape*100)
    r2 <- c(r2, performance$R2)
  }
  return(list(mse=mse, smape=smape, r2=r2))
}


save_image <- function(obj, strategy) {
  jpeg(sprintf('%s_%s.jpg', sub('/', '/graphics/', obj$filename), strategy), width = 640, height = 480)
  yvalues <- c(obj$train, obj$test)
  xlabels <- 1:length(yvalues)
  if(!is.null(names(yvalues)))
    xlabels <- as.integer(names(yvalues))
  grf <- plot_ts_pred(x = xlabels, y = yvalues, yadj=obj$adjust, ypre=obj$prediction)
  plot(grf)
  dev.off()
}
#-----------------------------------------------


#-----------------------------------------------
#------------------- MODELS --------------------
run <- function( x, filename, base_model, test_size,
                 params=list(sw_size=c(0), preprocess=list(ts_norm_none())), cv=TRUE ) {
  
  if (cv) {
    tryCatch({
      run_ml(x, filename, base_model, test_size, params)
    }, error = function(e) {
      save_error(filename, e)
    })
  } else {
    cases <- get_combinations(params)
    for (i in 1:length(cases)) {
      name <- get_names(filename, cases[[i]])
      tryCatch({
        run_ml(x, name, base_model, test_size, cases[[i]])
      }, error = function(e) {
        save_error(name, e)
      })
    }
  }
}


run_ml <- function( x, filename, base_model, test_size,
                    params=list(sw_size=c(0), preprocess=list(ts_norm_none())) ) {
  
  print(filename)
  
  # Fit
  train_size <- length(x)-test_size
  model <- wf_experiment(filename, base_model)
  best_model <- train_ml(model, x[1:train_size], params)
  
  results <- NULL
  
  # Rolling Origin
  for (j in 1:test_size) {
    result <- test_ml(best_model, x, test_pos=train_size+1, test_size=j, steps_ahead=1)
    results <- rbind(results, result$df)
  }
  save_image(result$model, 'ro')
  
  # Steps Ahead
  result <- test_ml(best_model, x, test_pos=train_size+1, test_size=test_size, steps_ahead=test_size)
  results <- rbind(results, result$df)
  save_image(result$model, 'sa')
  
  # Results
  save(results, file=sprintf('%s.rdata', sub('/', '/results/', filename)))
  
}


#-----------------------------------------------
#------------------- TRAIN ---------------------
train_ml <- function(obj, x, params) {
  
  best_model <- hyperparameters <- NULL
  best_error <- 200
  x <- na.omit(x)
  
  for (sw in params$sw_size) {
    
    # Windowing
    obj$sw_size <- sw
    obj$train <- x[obj$sw_size:length(x)]
    obj$obs <- as.character(obj$train)
    if (length(names(obj$train)) > 0)
      obj$obs <- names(obj$train)[length(obj$train)]
    xw <- ts_data(as.vector(x), obj$sw_size)
    xw <- na.omit(xw)
    xy <- ts_projection(xw)
    
    for (dn in params$preprocess) {
      
      # Set parameters
      input_size <- sw
      if (as.character(describe(dn)) == 'ts_norm_diff') input_size <- input_size - 1
      if (as.character(describe(obj$base_model)) == 'ts_conv1d') input_size <- input_size - 1
      if (as.character(describe(obj$base_model)) == 'ts_lstm') input_size <- input_size - 1
      ranges <- list(input_size=input_size, preprocess=list(dn), augment=params$augment, ranges=params$ranges)
      obj <- set_params(obj, ranges)
      
      # Tuning
      if (obj$sw_size != 0) {
        tune <- ts_integtune(obj$input_size, obj$base_model, folds=10, ranges=obj$ranges, preprocess=obj$preprocess, augment=obj$augment)
        tryCatch({
          obj$model <- fit(tune, xy$input, xy$output, obj$ranges)
          obj$input_size <- obj$model$input_size
          obj$preprocess <- obj$model$preprocess
          obj$augment <- attr(obj$model, 'augment')
          attr(obj$model, 'hyperparameters')$window_size <- obj$sw_size
          hyperparameters <- rbind(hyperparameters, attr(obj$model, 'hyperparameters'))
        }, error = function(e) {
          return(NULL)
        })
      } else {
        obj$model <- fit(obj$base_model, x=xy$input, y=xy$output)
        obj$input_size <- 0
        obj$preprocess <- ts_norm_none()
        obj$augment <- ts_aug_none()
      }
      
      # Evaluation
      if (!is.null(obj$model)) {
        obj$adjust <- as.vector(predict(obj$model, xy$input))
        obj$ev_adjust <- evaluate(obj$model, as.vector(xy$output), obj$adjust)
        error <- obj$ev_adjust$smape*100
        if (!is.na(error) && error < best_error) {
          best_error <- error
          best_model <- obj
        }
      }
    }
  }
  
  #Save parameters
  params <- attr(best_model$model, 'params')
  if (!is.null(hyperparameters)) {
    save(hyperparameters, file=sprintf('%s_hparams.rdata', sub('/', '/hyper/', obj$filename)))
    params$window_size <- best_model$sw_size
  }
  write.csv(params, file=sprintf('%s_params.csv', sub('/', '/hyper/', obj$filename)), row.names=FALSE)
  
  #Return optimized model
  return(best_model)
}


#-----------------------------------------------
#------------------ PREDICT --------------------
test_ml <- function(obj, x, test_pos, test_size, steps_ahead=1) {
  
  x <- na.omit(x)
  
  # Windowing
  obj$test <- x[test_pos:(test_pos+test_size-1)]
  if (obj$sw_size != 0) {
    xwt <- ts_data(as.vector(x[(test_pos-obj$sw_size+1):(test_pos+test_size-1)]), obj$sw_size)
  } else
    xwt <- ts_data(as.vector(x[test_pos:(test_pos+test_size-1)]), obj$sw_size)
  xwt <- na.omit(xwt)
  xyt <- ts_projection(xwt)    
  output <- as.vector(xyt$output)
  
  # Testing
  if (steps_ahead == 1)  {
    strategy <- 'ro'
    obj$prediction <- predict(obj$model, x=xyt$input, steps_ahead=steps_ahead)
    index <- test_size
  } else {
    strategy <- 'sa'
    obj$prediction <- predict(obj$model, x=xyt$input[1,], steps_ahead=steps_ahead)
    index <- 1:test_size
    output <- output[index]
  }
  
  # Evaluation
  obj$prediction <- as.vector(obj$prediction)
  obj$ev_prediction <- compute_performance(obj$model, obj$test, obj$prediction)
  
  # Results
  experiment <- strsplit(obj$filename, '_')[[1]]
  encoder <- obj$model$encoder
  result <- data.frame( instance = sub('^[^_]*_[^_]*_', '', obj$filename),
                        dataset = sub('/.*', '', experiment[2]),
                        ts = sub('^.*?/', '', experiment[1]),
                        model = sub('.*_(.*)', '\\1', describe(obj$base_model)),
                        preprocess = sub('.*_(.*)', '\\1', describe(obj$preprocess)),
                        augment = sub('.*_(.*)', '\\1', describe(obj$augment)),
                        window_size = obj$sw_size,
                        test_size = index,
                        strategy = strategy,
                        true = output[index],
                        pred = obj$prediction[index],
                        smape = obj$ev_prediction$smape[index] )
  
  return(list(df=result, model=obj))
}
#-----------------------------------------------
