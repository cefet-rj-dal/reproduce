library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)

is_pareto <- function(x, y) {
  n <- length(x)
  p <- rep(TRUE, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      
      if (
        x[j] <= x[i] &&
        y[j] <= y[i] &&
        (x[j] < x[i] || y[j] < y[i])
      ) {
        p[i] <- FALSE
        break
      }
    }
  }
  p
}

prepare_plot_data <- function(df, dataset_name) {
  df %>%
    mutate(
      dataset = dataset_name,
      pareto = is_pareto(energy, smape),
      point_fill = case_when(
        model == "FS+MSRFF" ~ "FS+MSRFF",
        pareto ~ "Frontier",
        TRUE ~ "Other"
      ),
      label_color = case_when(
        model == "FS+MSRFF" ~ "#D95F02",
        pareto ~ "gray20",
        TRUE ~ "gray50"
      ),
      label_face = ifelse(model == "FS+MSRFF", "bold", "plain")
    )
}

# Ler os dados
df_summary <- read_csv("pareto_data.csv", show_col_types = FALSE)
df_ro <- df_summary %>% filter(strategy == "RO")
df_sa <- df_summary %>% filter(strategy == "SA")

df_all <- bind_rows(
  prepare_plot_data(df_ro, "ro"),
  prepare_plot_data(df_sa, "sa")
) %>%
  mutate(dataset = factor(dataset, levels = c("ro", "sa")))

# Fronteira de Pareto
pareto_front <- df_all %>%
  filter(pareto) %>%
  group_by(dataset) %>%
  arrange(energy, .by_group = TRUE) %>%
  ungroup()

# Intervalos de confiança
df_all <- df_all %>%
  mutate(
    energy_half_ci = energy_ci95 / 2,
    smape_half_ci = smape_ci95 / 2
  )

# Barras horizontais do IC de energia
ci_x <- df_all %>%
  transmute(
    dataset,
    smape,
    x = pmax(energy - energy_half_ci, 0.001),
    xend = energy + energy_half_ci
  )

# Extremidades esquerdas
ci_x_left <- df_all %>%
  transmute(
    dataset,
    smape,
    x = pmax(energy - energy_half_ci, 0.001),
    xend = pmax(energy - energy_half_ci, 0.001),
    y = smape - 0.06,
    yend = smape + 0.06
  )

# Extremidades direitas
ci_x_right <- df_all %>%
  transmute(
    dataset,
    smape,
    x = energy + energy_half_ci,
    xend = energy + energy_half_ci,
    y = smape - 0.06,
    yend = smape + 0.06
  )

# Gráfico
g <- ggplot(df_all, aes(energy, smape)) +
  
  # Fronteira de Pareto
  geom_path(
    data = pareto_front,
    aes(group = dataset),
    linewidth = 0.5,
    linetype = "dotted",
    colour = "black"
  ) +
  
  # IC horizontal: energia
  geom_segment(
    data = ci_x,
    aes(x = x, xend = xend, y = smape, yend = smape),
    inherit.aes = FALSE,
    colour = "gray75",
    alpha = 0.6,
    linewidth = 0.15
  ) +
  
  geom_segment(
    data = ci_x_left,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    colour = "gray75",
    alpha = 0.6,
    linewidth = 0.155
  ) +
  
  geom_segment(
    data = ci_x_right,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    colour = "gray75",
    alpha = 0.6,
    linewidth = 0.155
  ) +
  
  # IC vertical: sMAPE
  geom_errorbar(
    aes(
      ymin = smape - smape_half_ci,
      ymax = smape + smape_half_ci
    ),
    width = 0.025,
    colour = "gray75",
    alpha = 0.6,
    linewidth = 0.15
  ) +
  
  # Pontos sem contorno
  geom_point(
    aes(color = point_fill),
    shape = 16,
    size = 3.5
  ) +
  
  # Rótulos dos modelos
  geom_text_repel(
    aes(
      label = model,
      color = label_color,
      fontface = label_face
    ),
    size = 5.0,
    box.padding = 0.65,
    point.padding = 0.45,
    force = 3.0,
    force_pull = 0.1,
    max.time = 1000,
    min.segment.length = 0,
    segment.size = 0.18,
    segment.color = "gray50",
    max.overlaps = Inf,
    seed = 42,
    show.legend = FALSE
  ) +
  
  # Cores dos pontos
  scale_color_manual(
    values = c(
      "Other" = "gray40",
      "Frontier" = "black",
      "FS+MSRFF" = "#D95F02",
      "#D95F02" = "#D95F02",
      "gray20" = "gray20",
      "gray40" = "gray40"
    ),
    guide = "none"
  ) +
  
  scale_x_log10(
    breaks = c(20, 40, 80, 150, 250, 450, 700)
  ) +
  scale_y_log10() +
  
  facet_wrap(
    ~dataset,
    nrow = 1,
    scales = "free_y",
    labeller = as_labeller(c(
      ro = "Rolling Origin",
      sa = "Steps Ahead"
    ))
  ) +
  
  labs(
    x = "\nEnergy Consumption (J)",
    y = expression(bold("sMAPE (%)"))
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(base_size = 18) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.spacing.x = grid::unit(1.4, "cm"),
    plot.margin = margin(10, 22, 10, 10),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14)
  )

# Salvar o gráfico
ggsave(
  "pareto.pdf",
  g,
  width = 14.5,
  height = 6.5
)