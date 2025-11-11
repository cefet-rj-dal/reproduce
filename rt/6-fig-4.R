library(dplyr)
library(rstatix)
library(tidyverse)
library(readr)
library(ggplot2)
library(gridExtra)

# Load data
metrics <- read_csv("~/Tese/Artigo/metrics_hetero.csv")

# Filter and select relevant columns
df_hetero <- subset(metrics, select=c("method", "series", "f1", "precision", "recall"))

# Calculating effect size
analisa_conjunto <- function(df) {
  metodos <- unique(df$method)
  metodos <- setdiff(metodos, "rtad")
  
  resultados <- data.frame()
  
  for (m in metodos) {
    series_comuns <- intersect(
      filter(df, method == "rtad")$series,
      filter(df, method == m)$series
    )
    
    extrair_metricas <- function(metrica_nome) {
      red <- filter(df, method == "rtad", series %in% series_comuns) %>% arrange(series) %>% pull(metrica_nome)
      outro <- filter(df, method == m, series %in% series_comuns) %>% arrange(series) %>% pull(metrica_nome)
      validos <- !is.na(red) & !is.na(outro)
      list(red = red[validos], outro = outro[validos])
    }
    
    for (metrica in c("f1", "precision", "recall")) {
      dados <- extrair_metricas(metrica)
      
      # Effect size
      p_valor <- wilcox.test(dados$red, dados$outro, paired = TRUE)$p.value
      r <- wilcox_effsize(
        data.frame(valor = c(dados$red, dados$outro),
                   metodo = rep(c("rtad", m), each = length(dados$red))),
        valor ~ metodo, paired = TRUE
      )$effsize
      
      # Median
      med_rtad  <- median(filter(df, method == "rtad")[[metrica]], na.rm = TRUE)
      med_outro <- median(filter(df, method == m)[[metrica]], na.rm = TRUE)
      mostrar_valor <- med_rtad >= med_outro
      
      resultados <- rbind(resultados, data.frame(
        metodo = m,
        metrica = metrica,
        effect_size = r,
        p_valor = p_valor,
        mostrar_valor = mostrar_valor
      ))
    }
  }
  
  return(resultados)
}

# run
resultados <- analisa_conjunto(df_hetero) %>% mutate(grupo = "hetero")

# labels
resultados <- resultados %>%
  mutate(label = case_when(
    mostrar_valor & p_valor < 0.05 ~ paste0("underline(", format(round(effect_size, 2), nsmall = 2), ")"),
    mostrar_valor & p_valor >= 0.05 ~ format(round(effect_size, 2), nsmall = 2),
    !mostrar_valor & p_valor < 0.05 ~ paste0("underline(", format(round(effect_size, 2), nsmall = 2), "*'*')"),
    TRUE ~ paste0(format(round(effect_size, 2), nsmall = 2), "*'*'")
  ))

# range
min_effect <- min(resultados$effect_size, na.rm = TRUE)
max_effect <- max(resultados$effect_size, na.rm = TRUE)

# generate plots
criar_grafico <- function(grupo_nome, titulo) {
  ggplot(filter(resultados, grupo == grupo_nome), aes(x = metrica, y = metodo, fill = effect_size)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 4, parse = TRUE) +
    scale_fill_gradient2(
      low = "white", mid = "grey90", high = "grey40",
      midpoint = 0.3,
      limits = c(min_effect, max_effect)
    ) +
    labs(title = titulo,
         x = "Metric",
         y = "Method") +
    theme_minimal(base_size = 15)
}

grafico_hetero <- criar_grafico("hetero", "Heteroscedastic series")

# show
plot(grafico_hetero)
