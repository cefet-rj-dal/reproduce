# Pacotes necessários
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(harbinger)
library(united)
library(lmtest)
hutils <- harutils()

safe_get <- function(lst, i) {
  if (i > 0 && i <= length(lst)) {
    lst[[i]]
  } else {
    NULL
  }
}

## ------------------------------------------------------------
## 1) Preparação dos métodos (modelos) ----
## ------------------------------------------------------------
metodos <- list(
  hanr_arima(), # arima
  hanr_fbiad(),  # fbiad
  hanr_garch(), #garch
  hanr_rtad(), #rtad
  hanr_fft(), #fft
  hanr_remd(), #remd
  hanr_wavelet(), #wavelet
  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_mlp(ts_norm_gminmax()), 
                  ranges = list(size = 1:10, decay = seq(0, 1, 1/9), maxit=2000)), sw_size = 30), # mlp_hyper  
  
  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_svm(ts_norm_gminmax()),
                  ranges = list(kernel=c("radial"), epsilon=seq(0, 1, 0.1), cost=seq(20, 100, 20))), sw_size = 30), # svm_hyper
  
  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_lstm(ts_norm_gminmax()), 
                  ranges = list(epochs = c(2000))), sw_size = 30) # lstm_hyper
)

names(metodos) <- c("arima", "fbiad", "garch", "rtad","fft", "remd", "wavelet", "autoencoder", "mlp","svm", "lstm")

## ------------------------------------------------------------
## 2) Preparação dos dados ----
## ------------------------------------------------------------
nome_base <- "A4Benchmark"
data(A4Benchmark)  # carrega a base 'A4Benchmark' no ambiente

# Fazemos teste de heteroscedasticidade e consideramos apenas as séries heteroscedásticas
series_ts <- list()
n <- length(A4Benchmark)

for (i in seq_len(n)){
  data <- A4Benchmark[[i]]
  names(data) <- c("idx","value","event","type")
  
  model <- lm(value ~ idx, data=data)
  bp <- bptest(model) 
  
  if (bp$p.value < 0.05) {
    serie_nome <- names(A4Benchmark)[i]
    series_ts[[serie_nome]] <- data
  }
}

#series_ts

## Garante diretório de resultados
dir.create("~/Tese/results", showWarnings = FALSE, recursive = TRUE)

## ------------------------------------------------------------
## 3) Detecção detalhada (com cache por método) ----
## ------------------------------------------------------------
detalhes_todos <- list()

for (j in seq_along(metodos)) {
  modelo_atual   <- metodos[[j]]
  modelo_atual$har_outliers <- hutils$har_outliers_gaussian
  modelo_atual$har_distance <- hutils$har_distance_l2
  modelo_atual$har_outliers_check <-  hutils$har_outliers_checks_firstgroup 
  
  nome_modelo    <- names(metodos)[j]
  detalhes_modelo <- list()
  print(nome_modelo)
  
  arq_cache <- file.path("~/Tese/results", sprintf("exp_detail_%s.RData", nome_modelo))
  
  if (file.exists(arq_cache)) {
    load(file = arq_cache) 
  }
  
  for (i in seq_along(series_ts)) {
    dados_serie <- series_ts[[i]]
    nome_serie  <- names(series_ts)[i]
    print(i)
    
    result <- safe_get(detalhes_modelo, i)
    
    if (is.null(result)) {
      
      detalhes_modelo[[i]] <- tryCatch({
        ## 3.1 Ajuste (fit)
        inicio_tempo <- Sys.time()
        set.seed(9)
        modelo_ajustado <- fit(modelo_atual, dados_serie$value)
        tempo_ajuste <- as.double(Sys.time() - inicio_tempo, units = "secs")
        
        ## 3.2 Detecção (detect)
        inicio_tempo <- Sys.time()
        resultado_detec <- detect(modelo_ajustado, dados_serie$value)
        tempo_deteccao <- as.double(Sys.time() - inicio_tempo, units = "secs")
        
        ## Transformação da série com RT
        inicio_tempo <- Sys.time()
        dados_serie_RT <- fc_RT(dados_serie$value)
        tempo_transformacao <- as.double(Sys.time() - inicio_tempo, units = "secs")
        
        ## 3.1 Ajuste da série transformada (fit)
        params <- attr(modelo_ajustado, "params")
        mymodel <- modelo_atual
        mymodel <- set_params(mymodel, params)
        
        inicio_tempo <- Sys.time()
        set.seed(9)
        modelo_ajustado_RT <- fit(mymodel, dados_serie_RT)
        tempo_ajuste_RT <- as.double(Sys.time() - inicio_tempo, units = "secs")
        
        ## 3.2 Detecção da série transformada (detect)
        inicio_tempo <- Sys.time()
        resultado_detec_RT <- detect(modelo_ajustado_RT, dados_serie_RT)
        tempo_deteccao_RT <- as.double(Sys.time() - inicio_tempo, units = "secs")
        
        ## 3.3 Empacota resultado desta série
        result <- list(
          md             = modelo_ajustado,
          rs             = resultado_detec,
          rs_RT          = resultado_detec_RT,
          dataref        = i,
          modelname      = nome_modelo,
          datasetname    = nome_base,
          seriesname     = nome_serie,
          time_fit       = tempo_ajuste,
          time_detect    = tempo_deteccao,
          time_RT        = tempo_transformacao,
          time_fit_RT    = tempo_ajuste_RT,
          time_detect_RT = tempo_deteccao_RT
        )

        result
      }, error = function(e) {
        message(sprintf("Erro em %s - %s: %s", nome_modelo, nome_serie, e$message))
        ## devolve o índice que falhou
        NULL
      })
    }
    ## 3.4 Salva cache incremental
    save(detalhes_modelo, file = arq_cache, compress = "xz")
  }
  
  ## Acumula os detalhes deste método no agregado geral
  detalhes_todos <- c(detalhes_todos, detalhes_modelo)
}

## ------------------------------------------------------------
## 4) Sumário de desempenho (tempo e métricas) ----
## ------------------------------------------------------------
linhas_resumo <- vector("list", length(detalhes_todos))
for (k in c(1:length(detalhes_todos))) {
  exp_k   <- detalhes_todos[[k]]
  dados_k <- series_ts[[exp_k$dataref]]
  dados_k$event <- F
  dados_k$event[dados_k$type=="anomaly"] <- T
  
  # Avaliação "soft" com janela deslizante (ajuste sw_size conforme o caso)
  avaliacao <- evaluate(har_eval(), exp_k$rs$event, dados_k$event)
  avaliacao_RT <- evaluate(har_eval(), exp_k$rs_RT$event, dados_k$event)
  avaliacao_soft <- evaluate(har_eval_soft(sw_size = 5), exp_k$rs$event, dados_k$event)
  avaliacao_RT_soft <- evaluate(har_eval_soft(sw_size = 5), exp_k$rs_RT$event, dados_k$event)
  
  # Linha do resumo para esta série e método
  linhas_resumo[[k]] <- data.frame(
    method         = exp_k$modelname,
    dataset        = exp_k$datasetname,
    series         = exp_k$seriesname,
    time_fit       = exp_k$time_fit,
    time_detect    = exp_k$time_detect,
    time_detect_RT = exp_k$time_detect_RT,
    time_RT        = exp_k$time_RT,
    precision      = avaliacao$precision,
    recall         = avaliacao$recall,
    f1             = avaliacao$F1,
    precision_soft      = avaliacao_soft$precision,
    recall_soft         = avaliacao_soft$recall,
    f1_soft             = avaliacao_soft$F1,
    precision_RT   = avaliacao_RT$precision,
    recall_RT      = avaliacao_RT$recall,
    f1_RT          = avaliacao_RT$F1,   
    precision_RT_soft   = avaliacao_RT_soft$precision,
    recall_RT_soft      = avaliacao_RT_soft$recall,
    f1_RT_soft          = avaliacao_RT_soft$F1,   
    stringsAsFactors = FALSE
  )
}

resumo_experimentos <- do.call(rbind, linhas_resumo)

