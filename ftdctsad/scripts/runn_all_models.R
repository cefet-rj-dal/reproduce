library("daltoolbox")
library("harbinger")

source("evaluate_har.R")
hutils <- harutils()

#### Função Evaluate ####
evaluate_har <- function(detection, event) {
  detection[is.na(detection)] <- FALSE
  detection <- as.logical(unlist(detection))
  event <- as.logical(unlist(event))
  
  TP <- sum(detection & event)
  FP <- sum(detection & !event)
  FN <- sum(!detection & event)
  TN <- sum(!detection & !event)
  
  confMatrix <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
                       dimnames = list(c("Detection = TRUE", "Detection = FALSE"),
                                       c("Event = TRUE", "Event = FALSE")))
  
  total <- TP + FP + FN + TN
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
  specificity <- ifelse((FP + TN) > 0, TN / (FP + TN), 0)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  accuracy <- (TP + TN) / total
  balanced_accuracy <- (sensitivity + specificity) / 2
  F1 <- ifelse((precision + sensitivity) > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0)
  
  metrics <- list(
    TP = TP, FP = FP, FN = FN, TN = TN,
    confMatrix = confMatrix,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    F1 = F1,
    balanced_accuracy = balanced_accuracy
  )
  
  return(metrics)
}

#### Inicialização ####
resultados_finais <- data.frame(
  dataset = c("test"),
  sample = c("test"),
  modelo = c("arima"),
  f1 = c(1),
  erro = c(FALSE),
  causa_erro = c('TESTE')
)

dict_dados <- new.env()

### Carregamento dos Datasets ###
caminho_arquivo <- "nab_sampleML.RData"
if (file.exists(caminho_arquivo)) {
  load(caminho_arquivo)
  if (exists("nab_sampleML")) {
    for (i in 1:length(nab_sampleML)) {
      nab_sampleML[[i]]$series <- as.numeric(nab_sampleML[[i]]$value)
    }
    dict_dados$nab_sampleML <- nab_sampleML
  } else {
    message("Objeto nab_sampleML não encontrado no arquivo.")
  }
} else {
  message("Arquivo nab_sampleML.RData não encontrado.")
}

### Aplicar técnica ###
for (nome_dataset in ls(dict_dados)) {
  dados_dataset <- dict_dados[[nome_dataset]]
  print(nome_dataset)
  
  for (nome_sample in names(dados_dataset)) {
    print(nome_sample)
    dataset <- dados_dataset[[nome_sample]]
    
    dataset$event[is.na(dataset$event)] <- FALSE
    dataset$series[is.na(dataset$series)] <- mean(dataset$series, na.rm = TRUE)
    dataset$event <- as.logical(dataset$event)
    dataset$series <- as.numeric(dataset$series)
    
    if (length(unique(dataset$series)) <= 1) {
      message("Erro: Dados insuficientes ou constantes em ", nome_sample)
      next
    }
    
    tamanho <- length(unique(dataset$event))
    variedade_serie <- length(unique(dataset$series))
    
    if (tamanho != 0 && variedade_serie > 1) {
      distance_group <- list("l1", "l2")
      outliers_group <- list("boxplot", "gaussian", "ratio")
      check_group <- list("firstgroup", "highgroup")
      
      #### Modelos ####
      modelos <- list(
        svm = hanr_ml(ts_svm(ts_norm_gminmax(), input_size = 4, kernel = "radial")),
        lstm = han_autoencoder(3, 2, lae_encode_decode, num_epochs = 1500),
        conv = hanr_ml(ts_conv1d(ts_norm_gminmax(), input_size = 4, epochs = 1000)),
        elm = hanr_ml(ts_elm(ts_norm_gminmax(), input_size = 4, nhid = 3, actfun = "purelin"))
      )
      
      for (modelo_nome in names(modelos)) {
        model <- modelos[[modelo_nome]]
        
        for (distance in distance_group) {
          for (outliers in outliers_group) {
            for (check in check_group) {
              model$har_distance <- if (distance == "l1") hutils$har_distance_l1 else hutils$har_distance_l2
              model$har_outliers <- switch(outliers,
                                           "boxplot" = hutils$har_outliers_boxplot,
                                           "gaussian" = hutils$har_outliers_gaussian,
                                           "ratio" = hutils$har_outliers_ratio)
              model$har_outliers_check <- if (check == "firstgroup") hutils$har_outliers_checks_firstgroup else hutils$har_outliers_checks_highgroup
              
              tryCatch({
                if (modelo_nome == "elm" && length(unique(dataset$series)) <= 2) {
                  stop("Dados insuficientes para o modelo ELM.")
                }
                
                model <- fit(model, dataset$series)
                detection <- detect(model, dataset$series)
                
                if (is.null(detection$event) || length(detection$event) == 0) {
                  stop("Erro: O modelo não retornou eventos detectados para ", nome_sample)
                }
                
                detection_event <- as.logical(detection$event)
                detection_event[is.na(detection_event)] <- FALSE
                dataset_event <- as.logical(dataset$event)
                dataset_event[is.na(dataset_event)] <- FALSE
                
                if (length(detection_event) != length(dataset_event)) {
                  stop("Erro: Tamanhos diferentes entre detection$event e dataset$event para ", nome_sample)
                }
                
                evaluation <- evaluate_har(detection_event, dataset_event)
                print(paste("F1 Score para ", modelo_nome, ": ", evaluation$F1))
                
                caminho <- paste("data_anomalia_expandido/rds5/", modelo_nome, distance, outliers, check, nome_dataset, nome_sample, ".RDS", sep = "_")
                evaluation$distance <- distance
                evaluation$outliers <- outliers
                evaluation$check <- check
                evaluation$dataset <- nome_dataset
                evaluation$sample <- nome_sample
                evaluation$modelo <- modelo_nome
                saveRDS(evaluation, file = caminho)
              }, error = function(e) {
                message("Erro no modelo ", modelo_nome, " para ", nome_sample, ": ", e$message)
              })
            }
          }
        }
      }
    } else {
      novas_linhas <- data.frame(dataset = nome_dataset, sample = nome_sample, modelo = "", f1 = 0, erro = TRUE, causa_erro = "No data")
      resultados_finais <- rbind(resultados_finais, novas_linhas)
    }
  }
}
print("Execução finalizada e arquivos salvos.")
