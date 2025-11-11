# packages
library(dplyr)
library(ggplot2)
library(readr)
library(rstatix)
library(tidyr)

# read CSV
dados <- read_csv("~/Tese/Artigo/metrics_hetero.csv")
#dados <- read_csv("~/Tese/Artigo/metrics_homo.csv")
dados <- subset(dados, method!="rtad")

sem_rt <- subset(dados, select = c("method", "series", "f1", "precision", "recall"))
com_rt <- subset(dados, select = c("method", "series", "f1_RT", "precision_RT", "recall_RT"))
names(com_rt) <- c("method", "series","f1", "precision", "recall")

# ordering
com_rt <- com_rt %>% arrange(method, series)
sem_rt <- sem_rt %>% arrange(method, series)

# metrics
metricas <- c("f1", "precision", "recall")

# strutured data
tabela_final <- data.frame()

# Loop by method
for (m in unique(com_rt$method)) {
  linha <- list(Metodo = m)
  
  for (metrica in metricas) {
    x <- com_rt %>% filter(method == m) %>% pull(metrica)
    y <- sem_rt %>% filter(method == m) %>% pull(metrica)
    
    # Wilcoxon test
    wilcox <- wilcox.test(x, y, alternative = "two.sided", paired = TRUE, exact = TRUE)
    
    # Effect size (magnitude only, 'r' is always positive)
    df_temp <- data.frame(
      id = 1:length(x),
      x = x,
      y = y
    ) %>%
      pivot_longer(cols = c(x, y), names_to = "grupo", values_to = "valor")
    
    # Assegurar que 'grupo' é um fator com níveis bem definidos e na ordem correta
    # para a consistência da interpretação se você fosse usar outras funções rstatix
    df_temp$grupo <- factor(df_temp$grupo, levels = c("x", "y")) 
    
    r <- wilcox_effsize(df_temp, valor ~ grupo, paired = TRUE, id = "id")$effsize # Adicionado id = "id" para robustez
    
    # interpretation of absolute effect size
    interpret_abs_r <- case_when(
      abs(r) < 0.1 ~ "negligible",
      abs(r) < 0.3 ~ "small",
      abs(r) < 0.5 ~ "medium",
      TRUE ~ "large"
    )
    
    rt_status <- ""
    if (wilcox$p.value <= 0.05) { # Apenas considerar se estatisticamente significativo
      median_diff <- median(x - y, na.rm = TRUE) # Mediana das diferenças (com RT - sem RT)
      
      if (median_diff > 0) {
        rt_status <- "RT Enhanced" # Métrica com RT é geralmente maior/melhor
      } else if (median_diff < 0) {
        rt_status <- "RT Degraded" # Métrica com RT é geralmente menor/pior
      } else { 
        rt_status <- "Significant (neutral effect)" # Mediana das diferenças zero, mas p-valor significativo
      }
    } else {
      rt_status <- "Non-significant"
    }
    # ------------------------------------------------
    
    # add colunms
#    linha[[paste0(metrica, "_pval")]] <- round(wilcox$p.value, 4)
    linha[[paste0(metrica, "_rt_status")]] <- rt_status
    linha[[paste0(metrica, "_effsize")]] <- round(r, 3)
    linha[[paste0(metrica, "_interp")]] <- interpret_abs_r
  }
  
  tabela_final <- bind_rows(tabela_final, linha)
}

# show table
print(tabela_final)
