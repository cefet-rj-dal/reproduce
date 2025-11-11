# packages
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# reading data
dados <- read_delim("~/Tese/Artigo/thresholds_hetero_hard_RTAD.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

dados <- dados %>%
  mutate(across(c(F1, precision, recall), ~as.numeric(gsub(",", ".", .))))

# order by median
dados <- dados %>%
  mutate(method = factor(method, levels = names(sort(tapply(F1, method, mean, na.rm = TRUE), decreasing = TRUE)))) %>%
  group_by(method) %>%
  mutate(mediana_F1 = median(F1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(method = reorder(method, -mediana_F1))

# long shape
dados_long <- dados %>%
  pivot_longer(cols = c(F1, precision, recall),
               names_to = "metrica",
               values_to = "valor")

# renaming
dados_long$method <- gsub("gaussiano", "gaussian", dados_long$method)
dados_long$method <- gsub("razao", "ratio", dados_long$method)

# mean by metric
medias <- dados_long %>%
  group_by(metrica) %>%
  summarise(media = mean(valor, na.rm = TRUE))

# create plot
ggplot(dados_long, aes(x = method, y = valor)) +
  geom_jitter(aes(color = valor), width = 0.2, alpha = 0.7, size = 1.5) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4,
               fatten = 0, color = "black") +
  facet_wrap(~metrica, scales = "free_y") +
  scale_color_gradient(low = "gray90", high = "gray10") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  labs(title = "Evaluation metrics by thresholding technique",
       x = "Threshold definition technique",
       y = "Metrics value",
       color = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
