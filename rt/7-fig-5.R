# Packages
library(ggplot2)
library(dplyr)

# Data
dados <- read_csv("~/Tese/Artigo/metrics_dispersion-measures.csv")

# Median calc
dados <- dados %>%
  group_by(method) %>%
  mutate(media_F1 = mean(F1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(method = reorder(method, -media_F1))  # Ordem decrescente da media

# Prepate data
resumo <- dados %>%
  group_by(method) %>%
  summarise(
    media = mean(F1, na.rm = TRUE),
    mediana = median(F1, na.rm = TRUE),
    q1 = quantile(F1, 0.25, na.rm = TRUE),
    q3 = quantile(F1, 0.75, na.rm = TRUE)
  ) %>%
  mutate(method = reorder(method, -mediana))  # Ordena pela mediana

# plot
ggplot(resumo, aes(x = method, y = media)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "gray40") +
  theme_minimal() +
  labs(title = "",
       x = "Dispersion measure",
       y = "F1-score (mean and IQR)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

