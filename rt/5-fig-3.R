# Pacotes necessários
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)

# 1. Leitura dos dados
dados <- read_csv("~/Tese/Artigo/metrics_hetero.csv")

# 2. Pré-processamento dos dados
# Ordena os métodos com base na mediana do f1-score (do maior para o menor)
dados <- dados %>%
  mutate(method = factor(method, levels = names(sort(tapply(f1, method, median, na.rm = TRUE), decreasing = TRUE))))

# Destaca o método 'rtad' com uma cor diferente para fácil identificação
dados$destacar <- ifelse(dados$method == "rtad", "destacado", "outros")

# 3. Criação dos gráficos individuais

# Gráfico para F1-score
p_f1 <- ggplot(dados, aes(x = method, y = f1, fill = destacar)) +
  geom_boxplot() +
  scale_fill_manual(values = c("destacado" = "gray95", "outros" = "gray70")) +
  theme_minimal() +
  labs(title = "F1-score",
       x = "Method",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold")) # Ajuste o tamanho do título para a grade

# Gráfico para Precision
p_precision <- ggplot(dados, aes(x = method, y = precision, fill = destacar)) +
  geom_boxplot() +
  scale_fill_manual(values = c("destacado" = "gray95", "outros" = "gray70")) +
  theme_minimal() +
  labs(title = "Precision",
       x = "Method",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"))

# Gráfico para Recall
p_recall <- ggplot(dados, aes(x = method, y = recall, fill = destacar)) +
  geom_boxplot() +
  scale_fill_manual(values = c("destacado" = "gray95", "outros" = "gray70")) +
  theme_minimal() +
  labs(title = "Recall",
       x = "Method",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"))

# 4. Montagem da visualização em grade
final_plot <- (p_recall + p_precision) / p_f1

# Exibir o gráfico final
print(final_plot)
