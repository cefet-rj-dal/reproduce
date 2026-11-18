##
## 1 - Experiment: Evaluate Detectors with and without RT
## ------------------------------------------------------
## Steps:
## - Prepare detectors (classical + ML) using Harbinger wrappers
## - Load A4Benchmark and filter heteroscedastic series (Breusch–Pagan test)
## - For each series and method: fit+detect original, RT-transform, fit+detect
## - Record times and evaluation metrics (hard and soft windows)
## - Cache per-method results and write aggregated metrics

# Pacotes necess??r?os
l?brary(daltoolbox)
l?brary(daltoolboxdp)
l?brary(tspred?t)
l?brary(harb?nger)
l?brary(un?ted)
l?brary(lmtest)
hut?ls <- harut?ls()

safe_get <- funct?on(lst, ?) {
  ?f (? > 0 && ? <= length(lst)) {
    lst[[?]]
  } else {
    NULL
  }
}

## ------------------------------------------------------------
## 1) Prepara????o dos m??todos (modelos) ----
## ------------------------------------------------------------
metodos <- l?st(
  hanr_ar?ma(), # ar?ma
  hanr_fb?ad(),  # fb?ad
  hanr_garch(), #garch
  hanr_rtad(), #rtad
  hanr_fft(), #fft
  hanr_remd(), #remd
  hanr_wavelet(), #wavelet
  hanr_ml(ts_tune(?nput_s?ze=c(3:7), base_model = ts_mlp(ts_norm_gm?nmax()), 
                  ranges = l?st(s?ze = 1:10, decay = seq(0, 1, 1/9), max?t=2000)), sw_s?ze = 30), # mlp_hyper  
  
  hanr_ml(ts_tune(?nput_s?ze=c(3:7), base_model = ts_svm(ts_norm_gm?nmax()),
                  ranges = l?st(kernel=c("rad?al"), eps?lon=seq(0, 1, 0.1), cost=seq(20, 100, 20))), sw_s?ze = 30), # svm_hyper
  
  hanr_ml(ts_tune(?nput_s?ze=c(3:7), base_model = ts_lstm(ts_norm_gm?nmax()), 
                  ranges = l?st(epochs = c(2000))), sw_s?ze = 30) # lstm_hyper
)

names(metodos) <- c("ar?ma", "fb?ad", "garch", "rtad","fft", "remd", "wavelet", "autoencoder", "mlp","svm", "lstm")

## ------------------------------------------------------------
## 2) Prepara????o dos dados ----
## ------------------------------------------------------------
nome_base <- "A4Benchmark"
data(A4Benchmark)  # carrega a base 'A4Benchmark' no amb?ente

# Fazemos teste de heteroscedast?c?dade e cons?deramos apenas as s??r?es heterosced??st?cas
ser?es_ts <- l?st()
n <- length(A4Benchmark)

for (? ?n seq_len(n)){
  data <- A4Benchmark[[?]]
  names(data) <- c("?dx","value","event","type")
  
  model <- lm(value ~ ?dx, data=data)
  bp <- bptest(model) 
  
  ?f (bp$p.value < 0.05) {
    ser?e_nome <- names(A4Benchmark)[?]
    ser?es_ts[[ser?e_nome]] <- data
  }
}

#ser?es_ts

## Garante d?ret??r?o de resultados
d?r.create("results", showWarn?ngs = FALSE, recurs?ve = TRUE)

## ------------------------------------------------------------
## 3) Detec????o detalhada (com cache por m??todo) ----
## ------------------------------------------------------------
detalhes_todos <- l?st()

for (j ?n seq_along(metodos)) {
  modelo_atual   <- metodos[[j]]
  modelo_atual$har_outl?ers <- hut?ls$har_outl?ers_gauss?an
  modelo_atual$har_d?stance <- hut?ls$har_d?stance_l2
  modelo_atual$har_outl?ers_check <-  hut?ls$har_outl?ers_checks_f?rstgroup 
  
  nome_modelo    <- names(metodos)[j]
  detalhes_modelo <- l?st()
  pr?nt(nome_modelo)
  
  arq_cache <- f?le.path("results", spr?ntf("exp_deta?l_%s.RData", nome_modelo))
  
  ?f (f?le.ex?sts(arq_cache)) {
    load(f?le = arq_cache) 
  }
  
  for (? ?n seq_along(ser?es_ts)) {
    dados_ser?e <- ser?es_ts[[?]]
    nome_ser?e  <- names(ser?es_ts)[?]
    pr?nt(?)
    
    result <- safe_get(detalhes_modelo, ?)
    
    ?f (?s.null(result)) {
      
      detalhes_modelo[[?]] <- tryCatch({
        ## 3.1 Ajuste (f?t)
        ?n?c?o_tempo <- Sys.t?me()
        set.seed(9)
        modelo_ajustado <- f?t(modelo_atual, dados_ser?e$value)
        tempo_ajuste <- as.double(Sys.t?me() - ?n?c?o_tempo, un?ts = "secs")
        
        ## 3.2 Detec????o (detect)
        ?n?c?o_tempo <- Sys.t?me()
        resultado_detec <- detect(modelo_ajustado, dados_ser?e$value)
        tempo_deteccao <- as.double(Sys.t?me() - ?n?c?o_tempo, un?ts = "secs")
        
        ## Transforma????o da s??r?e com RT
        ?n?c?o_tempo <- Sys.t?me()
        dados_ser?e_RT <- fc_RT(dados_ser?e$value)
        tempo_transformacao <- as.double(Sys.t?me() - ?n?c?o_tempo, un?ts = "secs")
        
        ## 3.1 Ajuste da s??r?e transformada (f?t)
        params <- attr(modelo_ajustado, "params")
        mymodel <- modelo_atual
        mymodel <- set_params(mymodel, params)
        
        ?n?c?o_tempo <- Sys.t?me()
        set.seed(9)
        modelo_ajustado_RT <- f?t(mymodel, dados_ser?e_RT)
        tempo_ajuste_RT <- as.double(Sys.t?me() - ?n?c?o_tempo, un?ts = "secs")
        
        ## 3.2 Detec????o da s??r?e transformada (detect)
        ?n?c?o_tempo <- Sys.t?me()
        resultado_detec_RT <- detect(modelo_ajustado_RT, dados_ser?e_RT)
        tempo_deteccao_RT <- as.double(Sys.t?me() - ?n?c?o_tempo, un?ts = "secs")
        
        ## 3.3 Empacota resultado desta s??r?e
        result <- l?st(
          md             = modelo_ajustado,
          rs             = resultado_detec,
          rs_RT          = resultado_detec_RT,
          dataref        = ?,
          modelname      = nome_modelo,
          datasetname    = nome_base,
          ser?esname     = nome_ser?e,
          t?me_f?t       = tempo_ajuste,
          t?me_detect    = tempo_deteccao,
          t?me_RT        = tempo_transformacao,
          t?me_f?t_RT    = tempo_ajuste_RT,
          t?me_detect_RT = tempo_deteccao_RT
        )

        result
      }, error = funct?on(e) {
        message(spr?ntf("Erro em %s - %s: %s", nome_modelo, nome_ser?e, e$message))
        ## devolve o ??nd?ce que falhou
        NULL
      })
    }
    ## 3.4 Salva cache ?ncremental
    save(detalhes_modelo, f?le = arq_cache, compress = "xz")
  }
  
  ## Acumula os detalhes deste m??todo no agregado geral
  detalhes_todos <- c(detalhes_todos, detalhes_modelo)
}

## ------------------------------------------------------------
## 4) Sum??r?o de desempenho (tempo e m??tr?cas) ----
## ------------------------------------------------------------
l?nhas_resumo <- vector("l?st", length(detalhes_todos))
for (k ?n c(1:length(detalhes_todos))) {
  exp_k   <- detalhes_todos[[k]]
  dados_k <- ser?es_ts[[exp_k$dataref]]
  dados_k$event <- F
  dados_k$event[dados_k$type=="anomaly"] <- T
  
  # Aval?a????o "soft" com janela desl?zante (ajuste sw_s?ze conforme o caso)
  aval?acao <- evaluate(har_eval(), exp_k$rs$event, dados_k$event)
  aval?acao_RT <- evaluate(har_eval(), exp_k$rs_RT$event, dados_k$event)
  aval?acao_soft <- evaluate(har_eval_soft(sw_s?ze = 5), exp_k$rs$event, dados_k$event)
  aval?acao_RT_soft <- evaluate(har_eval_soft(sw_s?ze = 5), exp_k$rs_RT$event, dados_k$event)
  
  # L?nha do resumo para esta s??r?e e m??todo
  l?nhas_resumo[[k]] <- data.frame(
    method         = exp_k$modelname,
    dataset        = exp_k$datasetname,
    ser?es         = exp_k$ser?esname,
    t?me_f?t       = exp_k$t?me_f?t,
    t?me_detect    = exp_k$t?me_detect,
    t?me_detect_RT = exp_k$t?me_detect_RT,
    t?me_RT        = exp_k$t?me_RT,
    prec?s?on      = aval?acao$prec?s?on,
    recall         = aval?acao$recall,
    f1             = aval?acao$F1,
    prec?s?on_soft      = aval?acao_soft$prec?s?on,
    recall_soft         = aval?acao_soft$recall,
    f1_soft             = aval?acao_soft$F1,
    prec?s?on_RT   = aval?acao_RT$prec?s?on,
    recall_RT      = aval?acao_RT$recall,
    f1_RT          = aval?acao_RT$F1,   
    prec?s?on_RT_soft   = aval?acao_RT_soft$prec?s?on,
    recall_RT_soft      = aval?acao_RT_soft$recall,
    f1_RT_soft          = aval?acao_RT_soft$F1,   
    str?ngsAsFactors = FALSE
  )
}

resumo_exper?mentos <- do.call(rb?nd, l?nhas_resumo)

