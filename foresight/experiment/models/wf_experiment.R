#-----------------------------------------------
#------------------ LIBRARIES ------------------
library(daltoolbox)
library(tspredit)
library(daltoolboxdp)
library(stringi)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)
library(devtools)
#-----------------------------------------------


#-----------------------------------------------
#-------------- GENERAL FUNCTIONS --------------
create_directories <- function(path) {
  dir_name <- c('results') #c('graphics', 'hyper', 'results')
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


get_time_series <- function(ts, test_size) {
  ts <- na.omit(as.vector(unlist(ts)))
  ts_train <- ts[1:(length(ts)-test_size)]
  ts_test <- tail(ts, test_size)
  return(c(ts_train, ts_test))
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
  combinations <- expand.grid(sw_size=params$sw_size, preprocess=params$preprocess, augment=params$augment)
  ml_combinations <- do.call(expand.grid, params$ranges)
  result <- list()
  for (i in 1:nrow(combinations)) {
    for (j in 1:nrow(ml_combinations)) {
      ranges <- list()
      for (param in names(ml_combinations))
        ranges[[param]] <- ml_combinations[[param]][j]
      params <- list()
      for (param in names(combinations))
        params[[param]] <- combinations[[param]][i]
      params$ranges <- ranges
      result <- append(result, list(params))
    }
  }
  return(result)
}


get_names <- function(filename, params) {
  sw <- sprintf('sw-%s', params$sw_size)
  dn <- gsub('_', '-', gsub('ts_', '', describe(params$preprocess[[1]])))
  da <- gsub('_', '-', gsub('ts_', '', describe(params$augment[[1]])))
  ml <- ''
  for (p in names(params$ranges)) {
    ml <- ifelse( ml == '',
                  sprintf('%s-%s', gsub('[^a-zA-Z0-9]', '', p), params$ranges[[p]]),
                  paste(ml, sprintf('%s-%s', gsub('[^a-zA-Z0-9]', '', p), params$ranges[[p]]), sep='_') )
  }
  filename <- paste(filename, sw, dn, da, ml, sep='_')
  return (filename)
}


get_object_stat <- function(object, names, default = NULL) {
  if (is.null(object))
    return(default)
  for (name in names) {
    value <- tryCatch(object[[name]], error = function(e) NULL)
    if (!is.null(value))
      return(value)
    value <- tryCatch(reticulate::py_get_attr(object, name), error = function(e) NULL)
    if (!is.null(value))
      return(tryCatch(reticulate::py_to_r(value), error = function(e) value))
  }
  default
}


parse_id <- function(x) {
  t(sapply(x, function(s) {
    p <- strsplit(s, '/')[[1]]
    a <- strsplit(tail(p, 1), '-')[[1]]
    out <- if (length(a) == 3) a else c(p[1], a)
    length(out) <- 3
    out
  }))
}


sync_model_metadata <- function(obj) {
  metadata_map <- list(
    bits = c('bits'),
    ram_size = c('ram_size'),
    thermometer = c('thermometer'),
    wisard = c('wisard'),
    embedding = c('transform'),
    dim = c('dim'),
    nan_freq = c('nan_freq')
  )
  targets <- list(
    obj$model,
    tryCatch(obj$model$model, error = function(e) NULL),
    tryCatch(obj$model$encoder, error = function(e) NULL)#
  )
  for (field in names(metadata_map)) {
    value <- NA
    for (target in targets) {
      value <- get_object_stat(target, metadata_map[[field]], default = NULL)
      if (!is.null(value))
        break
    }
    if (is.null(value))
      value <- NA
    obj[[field]] <- value
    obj$model <- tryCatch({
      obj$model[[field]] <- value
      obj$model
    }, error = function(e) obj$model)
  }
  obj
}


wf_experiment <- function( filename, base_model,
                           sw_size = c(0),
                           input_size = c(0),
                           seed = NA,
                           preprocess = list(ts_norm_none()),
                           augment = list(ts_aug_none()),
                           ranges = list() ) {
  obj <- dal_transform()
  obj$filename <- filename
  obj$base_model <- base_model
  obj$dataset <- sub('/.*', '', filename)
  obj$sw_size <- sw_size
  obj$input_size <- input_size
  obj$seed <- seed
  obj$preprocess <- preprocess
  obj$augment <- augment
  obj$ranges <- ranges
  obj$time <- Sys.time()
  class(obj) <- append('wf_experiment', class(obj))
  return(obj)
}


get_required_seeds <- function(filename) {
  model <- tolower(parse_id(filename)[3])
  if (grepl('^arima', model) || grepl('^ses', model)) {
    return(c(42))
  }
  c(42, 0, 1337, 100, 12345)
}


apply_seed_to_model <- function(base_model, seed) {
  if (is.null(base_model) || is.na(seed)) return(base_model)
  tryCatch({ base_model$seed <- as.integer(seed) }, error = function(e) NULL)
  tryCatch({ base_model$random_seed <- as.integer(seed) }, error = function(e) NULL)
  base_model
}


read_results_if_exists <- function(filename) {
  path <- sprintf('%s.rdata', sub('/','/results/',filename))
  if (!file.exists(path)) return(NULL)
  env <- new.env(parent=emptyenv())
  tryCatch({
    load(path, envir = env)
    if (exists("results", envir = env, inherits = FALSE)) {
      return(get("results", envir = env, inherits = FALSE))
    }
    NULL
  }, error = function(e) NULL)
}


save_results <- function(filename, results) {
  save(results, file=sprintf('%s.rdata', sub('/', '/results/', filename)))
}


compute_performance <- function(model, true, pred) {
  test_size <- length(pred)
  mse <- smape <- r2 <- NULL
  for (j in 1:test_size) {
    performance <- evaluate(model, true[1:j], pred[1:j])
    mse <- c(mse, performance$mse)
    smape <- c(smape, 100*performance$smape)
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


save_error <- function(filename, e) {
  message('erro')
  error_dir <- sprintf('error/%s', sub('/.*', '', filename))
  if (!dir.exists(error_dir)) dir.create(error_dir, recursive=TRUE)
  error_file <- sprintf('error/%s.txt', filename)
  writeLines(as.character(e), error_file)
}
#-----------------------------------------------


#-----------------------------------------------
#----------------- MLM & ARIMA -----------------
run <- function( x, filename, base_model, test_size,
                 params=list(sw_size=c(0), preprocess=list(ts_norm_none())), cv=TRUE ) {
  
  if (cv) {
    required_seeds <- get_required_seeds(filename)
    existing <- read_results_if_exists(filename)
    existing_seeds <- c()
    if (!is.null(existing) && "seed" %in% colnames(existing))
      existing_seeds <- unique(existing$seed)
    missing_seeds <- setdiff(required_seeds, existing_seeds)
    
    if (length(missing_seeds) > 0) {
      new_results <- NULL
      for (seed in missing_seeds) {
        tryCatch({
          df_seed <- run_ml(x, filename, base_model, test_size, params, seed=seed, save=FALSE)
          new_results <- rbind(new_results, df_seed)
        }, error = function(e) {
          save_error(sprintf('%s_seed-%s', filename, seed), e)
        })
      }
      combined <- rbind(existing, new_results)
      save_results(filename, combined)
    }
  } else {
    cases <- get_combinations(params)
    for (i in 1:length(cases)) {
      name <- get_names(filename, cases[[i]])
      required_seeds <- get_required_seeds(name)
      existing <- read_results_if_exists(name)
      existing_seeds <- c()
      if (!is.null(existing) && "seed" %in% colnames(existing))
        existing_seeds <- unique(existing$seed)
      missing_seeds <- setdiff(required_seeds, existing_seeds)
      
      if (length(missing_seeds) > 0) {
        new_results <- NULL
        for (seed in missing_seeds) {
          tryCatch({
            df_seed <- run_ml(x, name, base_model, test_size, cases[[i]], seed=seed, save=FALSE)
            new_results <- rbind(new_results, df_seed)
          }, error = function(e) {
            save_error(sprintf('%s_seed-%s', name, seed), e)
          })
        }
        combined <- rbind(existing, new_results)
        save_results(name, combined)
      }
    }
  }
}


#-----------------------------------------------
#----------------- MLM & ARIMA -----------------
run_ml <- function( x, filename, base_model, test_size,
                    params=list(sw_size=c(0), preprocess=list(ts_norm_none())),
                    seed = NA,
                    save = TRUE ) {
  
  message(filename)
  
  if (!is.na(seed)) set.seed(as.integer(seed))
  base_model <- apply_seed_to_model(base_model, seed)
  
  # Fit
  train_size <- length(x)-test_size
  model <- wf_experiment(filename, base_model, seed=seed)
  best_model <- train_ml(model, x[1:train_size], params)
  if (is.null(best_model)) {
    #params$preprocess <- list(ts_norm_swminmax())
    #params$sw_size <- c(15)
    #best_model <- train_ml(model, x[1:train_size], params)
  }
  
  # Predict
  results <- NULL
  #rolling origin
  for (j in 1:test_size) {
    result <- test_ml(best_model, x, test_pos=train_size+1, test_size=j, steps_ahead=1)
    results <- rbind(results, result$df)
  }
  #save_image(result$model, 'ro')
  #steps ahead
  result <- test_ml(best_model, x, test_pos=train_size+1, test_size=test_size, steps_ahead=test_size)
  results <- rbind(results, result$df)
  #save_image(result$model, 'sa')
  
  # Results
  if (save) save_results(filename, results)
  return(results)
}


#-------------------- Train --------------------
train_ml <- function(obj, x, params) {
  # Initialization
  best_model <- hyperparameters <- NULL
  best_error <- Inf
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
    for (dn in list(ts_norm_swminmax())) {
    #for (dn in params$preprocess) { #for (dn in list(ts_norm_swminmax())) {
      # Set parameters
      input_size <- sw - 1 #sw_size[sw_size <= sw]
      if (as.character(describe(dn)) == 'ts_norm_diff') input_size <- input_size - 1
      if (grepl('conv1d|lstm', as.character(describe(obj$base_model)))) input_size <- input_size - 1
      ranges <- list(input_size=input_size, preprocess=list(dn), augment=params$augment, ranges=params$ranges)
      obj <- set_params(obj, ranges)
      
      # Tuning
      if (obj$sw_size != 0) {
        tune <- ts_integtune(obj$input_size, obj$base_model, folds=3, ranges=obj$ranges, preprocess=obj$preprocess, augment=obj$augment)
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
      if (!is.null(obj$model)) {
        # Evaluation
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
  
  #if (is.null(best_model)) {
  #  sw <- min(params$sw_size)-1
  #  if (sw > 2) {
  #    params$sw_size <- c(sw)
  #    best_model <- train_ml(obj, x, params)
  #  }
  #}
  
  #Save parameters
  params <- attr(best_model$model, 'params')
  if (!is.null(hyperparameters)) {
    #save(hyperparameters, file=sprintf('%s_hparams.rdata', sub('/', '/hyper/', obj$filename)))
    params$window_size <- best_model$sw_size
  }
  #write.csv(params, file=sprintf('%s_params.csv', sub('/', '/hyper/', obj$filename)), row.names=FALSE)
  
  #Return optimized model
  return(best_model)
}


#------------------- Predict -------------------
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
  obj <- sync_model_metadata(obj)
  result <- data.frame( instance = sub('^[^_]*_', '', obj$filename),
                        seed = obj$seed,
                        dataset = parse_id(experiment[1])[1],
                        ts = parse_id(experiment[1])[2],
                        model = parse_id(experiment[1])[3],
                        #preprocess = sub('.*_(.*)', '\\1', describe(obj$preprocess)),
                        #augment = sub('.*_(.*)', '\\1', describe(obj$augment)),
                        window_size = obj$sw_size,
                        wisard = obj$wisard,
                        #thermometer = obj$thermometer,
                        bits = obj$bits,
                        ram_size = obj$ram_size,
                        embedding = obj$embedding,
                        dim = obj$dim,
                        test_size = index,
                        strategy = strategy,
                        true = output[index],
                        pred = obj$prediction[index],
                        mse = obj$ev_prediction$mse[index],
                        smape = obj$ev_prediction$smape[index],
                        nan_freq = obj$nan_freq,
                        time_sec = as.numeric(Sys.time()-obj$time) )
  
  return(list(df=result, model=obj))
}
#-----------------------------------------------


#-----------------------------------------------
#--------------------- LLM ---------------------
run_llm <- function( x, filename, base_model, test_size,
                     samples=1000, params='' ) {
  
  required_seeds <- get_required_seeds(filename)
  existing <- read_results_if_exists(filename)
  existing_seeds <- c()
  if (!is.null(existing) && "seed" %in% colnames(existing))
    existing_seeds <- unique(existing$seed)
  missing_seeds <- setdiff(required_seeds, existing_seeds)
  
  if (length(missing_seeds) == 0) return(invisible(NULL))
  
  new_results <- NULL
  for (seed in missing_seeds) {
    tryCatch({
      df_seed <- run_llm_seed(x, filename, base_model, test_size, samples=samples, params=params, seed=seed)
      new_results <- rbind(new_results, df_seed)
    }, error = function(e) {
      save_error(sprintf('%s_seed-%s', filename, seed), e)
    })
  }
  
  combined <- rbind(existing, new_results)
  save_results(filename, combined)
}


#------------------ LLM (1 seed) ---------------
run_llm_seed <- function( x, filename, base_model, test_size,
                          samples=10000, params='', seed = NA ) {
  message(filename)
  
  if (!is.na(seed)) set.seed(as.integer(seed))
  base_model <- apply_seed_to_model(base_model, seed)
  
  # Create
  train_size <- length(x) - test_size
  llm <- wf_experiment(filename, base_model, seed=seed)
  llm <- train_llm(llm, x[1:train_size])
  
  # Predict
  results <- NULL
  #rolling origin
  result <- test_llm(llm, x, test_pos=train_size+1, test_size=test_size, steps_ahead=1, samples=samples, params=params)
  results <- rbind(results, result$df)
  #steps ahead
  result <- test_llm(llm, x, test_pos=train_size+1, test_size=test_size, steps_ahead=test_size, samples=samples, params=params)
  results <- rbind(results, result$df)
  
  results
}

#--------------------- Fit ---------------------
train_llm <- function(obj, x) {
  x <- na.omit(x)
  obj$model <- fit(obj$base_model, x)
  obj$input_size <- 0
  obj$preprocess <- ts_norm_none()
  obj$augment <- ts_aug_none()
  return(obj)
}


#------------------- Predict -------------------
test_llm <- function(obj, x, test_pos, test_size, steps_ahead=1, samples, params) {
  x <- na.omit(x)
  obj$train <- x[1:(test_pos - 1)]
  
  # Testing
  if (steps_ahead == 1) {
    strategy <- 'ro'
    predictions <- NULL
    for (j in 1:test_size) {
      obj$test <- x[test_pos:(test_pos + j - 1)]
      pred <- predict(obj$model, x=obj$train, y=obj$test, steps_ahead, samples=samples, params)
      predictions <- c(predictions, pred)
    }
    predictions <- data.frame(predictions)
  } else {
    strategy <- 'sa'
    obj$test <- x[test_pos:(test_pos + test_size - 1)]
    predictions <- predict(obj$model, x=obj$train, y=obj$test, steps_ahead, samples=samples, params)
  }
  
  # Evaluation
  obj$prediction <- as.vector(apply(predictions, 2, mean))
  obj$ev_prediction <- compute_performance(obj$model, obj$test, obj$prediction)
  
  # Results
  experiment <- strsplit(obj$filename, '_')[[1]]
  obj <- sync_model_metadata(obj)
  result <- data.frame( instance = sub('^[^_]*_', '', obj$filename),
                        seed = obj$seed,
                        dataset = parse_id(experiment[1])[1],
                        ts = parse_id(experiment[1])[2],
                        model = parse_id(experiment[1])[3],
                        #preprocess = sub('.*_(.*)', '\\1', describe(obj$preprocess)),
                        #augment = sub('.*_(.*)', '\\1', describe(obj$augment)),
                        window_size = obj$sw_size,
                        wisard = obj$wisard,
                        #thermometer = obj$thermometer,
                        bits = obj$bits,
                        ram_size = obj$ram_size,
                        embedding = obj$embedding,
                        dim = obj$dim,
                        test_size = seq(1, test_size),
                        strategy = strategy,
                        true = obj$test,
                        pred = obj$prediction,
                        mse = obj$ev_prediction$mse,
                        smape = obj$ev_prediction$smape,
                        nan_freq = obj$nan_freq,
                        time_sec = as.numeric(Sys.time()-obj$time) )
  
  return(list(df=result, model=obj))
}
#-----------------------------------------------
