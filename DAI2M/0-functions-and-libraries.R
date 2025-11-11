##############################################################################################################################
# D‑AI2‑M Utilities: Libraries and Functions
# ---------------------------------------------------------------------------------------------------------------
# Centralizes libraries and helper functions used across experiments:
# - ARIMA baseline training/evaluation
# - PRE+MLM pipelines with tuning and forecasting
# - Plot saving and results integration utilities
#
# Notes
# - Dataset schemas and result column names remain unchanged for compatibility.
# - Messages/comments have been translated to English for clarity.
##############################################################################################################################
##############################################################################################################################
# 1) LIBRARIES ------------------------------------------------------------------------------------------------------------- #
##############################################################################################################################
library("daltoolbox")
library("forecast")
library("nnet")
library("elmNNRcpp")
library("e1071")
library(stats)
library(ggplot2)
library(zoo)
library(corrplot)
library(reticulate)
library(lubridate)
library(tspredit)



##############################################################################################################################
# 2) FUNCTIONS ------------------------------------------------------------------------------------------------------------- #
# Each function contains step‑wise comments describing inputs/outputs where helpful.
##############################################################################################################################

# -------------------------------------------------------------------------------------------------------------------------- #
#   2.1) Train and evaluate ARIMA reference models
F_ARIMA <- function(df, estado, etanol, meses_teste, titulo, seed=1, remove_anos_finais=0){
  set.seed(seed)
  
  # Select producing state
  TS_ORIGINAL <- df
  TS_ORIGINAL <- TS_ORIGINAL[TS_ORIGINAL$Estado_Sigla == estado, ]
  
  # Exclude final period according to the Rolling Forecast Origin strategy
  TS_ORIGINAL <- TS_ORIGINAL[1:(nrow(TS_ORIGINAL) - (12 * remove_anos_finais)),]
  
  # Select ethanol type and assemble the target vector x
  if(etanol == "hydrous"){
    x <- TS_ORIGINAL$PROD_ETANOL_HIDRATADO
  }else if(etanol=="anhydrous"){
    x <- TS_ORIGINAL$PROD_ETANOL_ANIDRO
  }else{print("PRODUCT NOT SPECIFIED CORRECTLY")}
  
  # Create time series object (no sliding window for ARIMA)
  sw_size <- 0
  ts <- ts_data(x, sw_size)
  
  # Split into train/test
  samp <- ts_sample(ts, meses_teste)
  
  # Train ARIMA
  model <- ts_arima()
  io_train <- ts_projection(samp$train)
  model <- fit(model, x=io_train$input, y=io_train$output)
  
  # Evaluate in-sample fit (adjust)
  adjust <- predict(model, io_train$input)
  ev_adjust <- evaluate(model, io_train$output, adjust)
  
  # Forecast on test
  steps_ahead <- meses_teste
  io_test <- ts_projection(samp$test)
  # Realiza a predição com base no modelo previamente treinado
  prediction <- predict(model, x=io_test$input, steps_ahead=steps_ahead)
  prediction <- as.vector(prediction)
  
  # Plotting result in a jpg file
  yvalues <- c(io_train$output, io_test$output)
  Date <- as.Date(TS_ORIGINAL$Data)
  Date <- tail(Date, n=length(yvalues))
  save_image(yvalues = yvalues, adjust=adjust, prediction=prediction, 
             date = Date, state = estado, title = titulo, product = etanol) 
  
  # Calculating Training and Testing R2 metrics
  R2_Treino <- 1 - sum((io_train$output - adjust)^2) / sum((io_train$output - mean(io_train$output))^2)
print(paste("Train R2 =", R2_Treino))
  
  df  <- data.frame(real = as.vector(io_test$output), previsto = prediction)
# Example (optional): absolute percentage error
# df$ape <- abs((df$real - df$previsto)/df$real)
  
  R2_Teste <- 1 - (sum(abs(df$real-df$previsto)^2) / sum((df$real - mean(df$real))^2))
print(paste("Test R2 =", R2_Teste))
  
  # Assembling the function output's dataset
  saida <- data.frame(Estado = estado, Produto = etanol, Ano_Teste = max(year(Date)),Modelo = "ARIMA", preprocess = NA,
                      R2_Treino= R2_Treino, R2_Teste = R2_Teste, 
                      Ordem = paste0("ARIMA(", model$p, "," , model$d , "," , model$q , ")"), best_sw = NA,
                      input_size = NA,
                      nhid = NA,
                      actfun = NA,
                      kernel = NA,
                      epsilon = NA,
                      cost = NA,
                      size = NA,
                      decay = NA,
                      maxit = NA,
                      epochs = NA)
  
  return(saida)
}

# -------------------------------------------------------------------------------------------------------------------------- #
#   2.2) Function for training and evaluating PRE+MLM models
F_TSReg <- function(df, estado, etanol, meses_teste, sw_par, input_size, base_model, ranges, titulo, seed=1, 
                    remove_anos_finais=0,
                    wavelet=FALSE, wavelet_filter){
  set.seed(seed)
  # Printing training start date and time
  print(paste("Início do treinamento:", format(Sys.time(), format = "%B %d, %Y %H:%M:%S")))
  
  # Selecting the producing state
  TS_ORIGINAL <- df
  TS_ORIGINAL <- TS_ORIGINAL[TS_ORIGINAL$Estado_Sigla == estado, ]
  
  # Excluding the final dataset period according to Rolling Forecast Origin strategy
  TS_ORIGINAL <- TS_ORIGINAL[1:(nrow(TS_ORIGINAL) - (12 * remove_anos_finais)),]
  
  # Selecting the Correct Type of Ethanol and assembling the "x" vector
  if(etanol == "hydrous"){
    x <- TS_ORIGINAL$PROD_ETANOL_HIDRATADO
  }else if(etanol=="anhydrous"){
    x <- TS_ORIGINAL$PROD_ETANOL_ANIDRO
  }else{print("PRODUCT NOT SPECIFIED CORRECTLY")}
  
  # Perform pre-processing based on the inverse Wavelet transformation.
  #if(wavelet==TRUE){
    # Applies Wavelet preprocessing only to the portion of the data to be used in model training
  #  x_Wavelet <- head(x, n = length(x) - meses_teste)
    # Executes the wavelet transform function
  #  wavelet_data <- F_WAVELET(x_Wavelet)
    # Subwrites the preprocessed values in the variable "x".
  #  x[1:(length(x) - meses_teste)] <- wavelet_data
  #}
  
  
  # Perform Wavelet filter. ########## TEST ##########
  if(wavelet==TRUE){
    source("https://raw.githubusercontent.com/cefet-rj-dal/tspredit/main/R/ts_fil_wavelet.R")
    # Applies Wavelet filter only to the portion of the data to be used in model training
    x_Wavelet <- head(x, n = length(x) - meses_teste)
    # Run Wavelet filter
    filter <- ts_fil_wavelet(filter = c("haar", "d4", "la8", "bl14", "c6"))#ts_fil_wavelet()
    filter <- fit(filter, x_Wavelet)
    y <- transform(filter, x_Wavelet)
    # Subwrites the preprocessed values in the variable "x".
    plot(plot_ts_pred(y=x_Wavelet, yadj=y))
    x[1:(length(x) - meses_teste)] <- y
  }
  
  
  # Model adjustment, including optimization of the sliding window hyperparameter
  best_R2  <-  0
  for (sw_size in sw_par){
    # Creating the TS
    ts <- ts_data(x, sw_size)
    
    # Segregation of TS in training and testing
    samp <- ts_sample(ts, meses_teste)
    
    # Model tunning
    tune <- ts_tune(input_size=input_size, base_model = base_model)
    # Model Training
    io_train <- ts_projection(samp$train)
    #ranges <- ranges   # EXCLUIR!
    model <- fit(tune, x=io_train$input, y=io_train$output, ranges)
    
    # Evaluation of adjustment
    adjust <- predict(model, io_train$input)
    ev_adjust <- evaluate(model, io_train$output, adjust)
    
    # Calculating Training R2 metric
    R2_Treino <- 1 - sum((as.vector(io_train$output) - adjust)^2) / sum((as.vector(io_train$output) - mean(as.vector(io_train$output)))^2)
print(paste("Train R2 for sw_size =", sw_size, "=", R2_Treino))
    
    # Registering the model with the best test R2
    if (best_R2 == 0) {
      best_R2 <- R2_Treino
      best_sw = sw_size
      best_model = model
    } else {
      if (R2_Treino > best_R2) {
        best_R2 <- R2_Treino
        best_sw = sw_size
        best_model = model
      }
    }
  }
  print(paste("best_sw =", best_sw))
  
  # Selecting for use the model with the best R2 training metric
  model  <-  best_model
  
  # Making and evaluating predictions with the adjusted model
  adjust <- predict(model, io_train$input)
  ev_adjust <- evaluate(model, io_train$output, adjust)
  steps_ahead <- meses_teste
  io_test <- ts_projection(samp$test)
  prediction <- predict(model, x=io_test$input[1,], steps_ahead = steps_ahead)
  prediction <- as.vector(prediction)
  output <- as.vector(io_test$output)
  if (steps_ahead > 1)
    output <- output[1:steps_ahead]
  
  ev_test <- evaluate(model, output, prediction)
  
  # Plotting result in a jpg file
  yvalues <- c(io_train$output, io_test$output)
  Date <- as.Date(TS_ORIGINAL$Data)
  Date <- tail(Date, n=length(yvalues))
  save_image(yvalues = yvalues, adjust=adjust, prediction=prediction, 
             date = Date, state = estado, title = titulo, product = etanol)

  # Calculating Training and Testing R2 metrics
  R2_Treino <- 1 - sum((as.vector(io_train$output) - adjust)^2) / sum((as.vector(io_train$output) - mean(as.vector(io_train$output)))^2)
  print(paste("Train R2 =", R2_Treino))
  
  df  <- data.frame(real = as.vector(io_test$output), previsto = prediction)

  R2_Teste <- 1 - (sum(abs(df$real-df$previsto)^2) / sum((df$real - mean(df$real))^2))
  
  print(paste("Test R2 =", R2_Teste))
  
  print(df)
  
  # Printing training end date and time
  print(paste("Final do treinamento:", format(Sys.time(), format = "%B %d, %Y %H:%M:%S")))
  
  # Assembling the function output's dataset
  saida <- data.frame(Estado = estado, Produto = etanol, Ano_Teste = max(year(Date)), Modelo = titulo, preprocess = class(model$preprocess)[1],
                      R2_Treino= R2_Treino, R2_Teste = R2_Teste, Ordem = NA,
                      best_sw = best_sw,
                      input_size = model$input_size, 
                      nhid = if(!is.null(model$nhid)){model$nhid} else {NA},
                      actfun = if(!is.null(model$actfun)){model$actfun} else {NA},
                      kernel = if(!is.null(model$kernel)){model$kernel} else {NA},
                      epsilon = if(!is.null(model$epsilon)){model$epsilon} else {NA},
                      cost = if(!is.null(model$cost)){model$cost} else {NA},
                      size = if(!is.null(model$size)){model$size} else {NA},
                      decay = if(!is.null(model$decay)){model$decay} else {NA},
                      maxit = if(!is.null(model$maxit)){model$maxit} else {NA},
                      epochs = if(!is.null(model$epochs)){model$epochs} else {NA})
  
  return(saida)
}

# -------------------------------------------------------------------------------------------------------------------------- #
#   2.3) Function for training and evaluating all PRE+MLM models in a given scenario. This function uses the 
#        Rolling Forecast Origin strategy
F_PRE_MLM_RO <- function(state, product, AnoTesteInicial, PRE_MLM, wavelet=FALSE){
  resultado <- data.frame()
  scenario = paste0(state, "_", product)
  create_directories(scenario)

  for(AnoTeste in (AnoTesteInicial-4):AnoTesteInicial){
    remove_anos_finais = max(year(dataset$Data)) - AnoTeste
    nome_modelo = 1
    for (modelo in PRE_MLM){
      TipoMLM = names(PRE_MLM)[nome_modelo]
      print("=================================================================================")
      print(paste0(TipoMLM, " - Etanol ", product, " - ", state, " - Teste em ", AnoTeste))
      modelo_avaliado <- F_TSReg(df = dataset, estado=state, etanol=product,
                                   meses_teste = 12, sw_par=sw_par,
                                   input_size = input_size,
                                   base_model = modelo$base_model,
                                   ranges = modelo$ranges,
                                   titulo = TipoMLM,
                                   seed = 1,
                                   remove_anos_finais=remove_anos_finais,
                                   wavelet=wavelet)
      print(modelo_avaliado)
      resultado <- rbind(resultado, modelo_avaliado)
      nome_modelo = nome_modelo + 1
      }
    }
  if(wavelet==TRUE){
    #filename <- sprintf("results/results_%s_wavelet.RDS", scenario)
    filename <- sprintf("results/results_%s_wavelet_filter.RDS", scenario) #Linha temporária
  }else{
    filename <- sprintf("results/results_%s.RDS", scenario)
  }
  
  saveRDS(resultado, filename)
  #return(resultado)
}

# -------------------------------------------------------------------------------------------------------------------------- #
#   2.4) Function for creating directories
create_directories <- function(scenario) {
  #dir_name <- sprintf("%s/%s", "hyper", scenario)
  #if (!file.exists(dir_name))
  #  dir.create(dir_name, recursive = TRUE)
  dir_name <- sprintf("%s/%s", "graphics", scenario)
  if (!file.exists(dir_name))
    dir.create(dir_name, recursive = TRUE)
  dir_name <- sprintf("%s", "results")
  if (!file.exists(dir_name))
    dir.create(dir_name, recursive = TRUE)
}

# -------------------------------------------------------------------------------------------------------------------------- #
#   2.5) Function to save model graph results to jpg files
save_image <- function(yvalues, adjust, prediction, date, state, title, product) {
  # 1. Filename
  scenario = paste0(state, "_", product)
  title = paste0(title, " - ", state, " - ", product, " - Test Year: ", max(year(date)))
  jpeg(sprintf("graphics/%s/%s.jpg", scenario, title), width = 880, height = 480)
  # 2. Create the plot
  grf <- plot_ts_pred(x = date, y=yvalues, yadj=adjust, ypre=prediction, color_adjust = "blue", color_prediction = "red") +
    theme(text = element_text(size=18)) +
    labs(title = title)
  plot(grf)
  # 3. Close the file
  dev.off()
}

# -------------------------------------------------------------------------------------------------------------------------- #
#   2.6) Function to integrate all .RDS results files into a single .csv file
integrateAndSaveResults <- function(subdir) {
  # List all .RDS files in the results subdirectory
  rdsFiles <- list.files(path = subdir, pattern = "\\.RDS$", full.names = TRUE)
  
  # Initialize an empty dataframe to store the combined data
  combinedDF <- NULL
  
  # Read each .RDS file and combine them
  for (file in rdsFiles) {
    tempDF <- readRDS(file)
    if(is.null(combinedDF)) {
      combinedDF <- tempDF
    } else {
      combinedDF <- rbind(combinedDF, tempDF)
    }
  }
  
  # Save the combined dataframe to a .csv file
  write.csv(combinedDF, file = "results/IntegratedResults.csv", row.names = FALSE)
}

