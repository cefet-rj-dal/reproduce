library(ggplot2)
library(ggrepel)
library(dplyr)

pretty_model <- function(x) {
  dplyr::case_when(
    x == "foresight" ~ "ForeSight",
    x == "regwisard" ~ "CReW",
    x == "knn" ~ "kNN",
    x == "chronos" ~ "Chronos",
    x == "conv1d" ~ "Conv1D",
    TRUE ~ toupper(x)
  )
}

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
      model_label = pretty_model(model),
      highlight = ifelse(model == "foresight", "ForeSight", "Other"),
      label_face = ifelse(model == "foresight", "bold", "plain"),
      pareto = is_pareto(smape, energy)
    )
}

df_ro <- data.frame(
  model = c("arima", "chronos", "conv1d", "elm", "foresight", "knn", "lstm", "mlp", "regwisard", "rfr", "svr"),
  smape = c(5.329, 5.525, 8.879, 6.425, 5.027, 5.980, 6.358, 4.946, 6.167, 5.750, 5.614),
  energy = c(66.218, 448.843, 672.215, 140.942, 23.835, 81.356, 181.613, 219.980, 19.870, 146.198, 185.404),
  smape_ci95 = c(0.168, 0.176, 0.166, 0.179, 0.152, 0.160, 0.169, 0.127, 0.159, 0.158, 0.168),
  energy_ci95 = c(14.746, 36.452, 32.644, 2.432, 2.151, 14.124, 3.327, 14.718, 1.197, 2.888, 10.634)
)

df_sa <- data.frame(
  model = c("arima", "chronos", "conv1d", "elm", "foresight", "knn", "lstm", "mlp", "regwisard", "rfr", "svr"),
  smape = c(5.608, 6.394, 7.838, 6.408, 5.272, 5.862, 6.140, 5.134, 5.991, 5.693, 5.662),
  energy = c(66.591, 399.197, 676.981, 146.372, 30.980, 82.378, 189.504, 220.461, 24.859, 153.050, 186.008),
  smape_ci95 = c(0.170, 0.200, 0.191, 0.174, 0.151, 0.164, 0.165, 0.132, 0.156, 0.162, 0.171),
  energy_ci95 = c(14.749, 19.390, 32.689, 2.549, 2.445, 14.174, 3.484, 14.724, 1.416, 3.028, 10.649)
)

df_all <- bind_rows(
  prepare_plot_data(df_ro, "ro"),
  prepare_plot_data(df_sa, "sa")
) %>%
  mutate(dataset = factor(dataset, levels = c("ro", "sa")))

pareto_front <- df_all %>%
  group_by(dataset) %>%
  filter(pareto) %>%
  arrange(energy, .by_group = TRUE) %>%
  ungroup()

xlim <- range(c(df_all$energy - df_all$energy_ci95, df_all$energy + df_all$energy_ci95))
ci_h <- 0.06

ci_x <- df_all %>%
  transmute(
    dataset,
    smape,
    x = energy - energy_ci95,
    xend = energy + energy_ci95
  )

ci_x_left <- df_all %>%
  transmute(
    dataset,
    smape,
    x = energy - energy_ci95,
    xend = energy - energy_ci95,
    y = smape - ci_h,
    yend = smape + ci_h
  )

ci_x_right <- df_all %>%
  transmute(
    dataset,
    smape,
    x = energy + energy_ci95,
    xend = energy + energy_ci95,
    y = smape - ci_h,
    yend = smape + ci_h
  )

g <- ggplot(df_all, aes(energy, smape)) +
  geom_path(data = pareto_front, aes(group = dataset), linewidth = 0.5, linetype = "dotted", colour = "gray30") +
  geom_segment(data = ci_x, aes(x = x, xend = xend, y = smape, yend = smape), inherit.aes = FALSE, colour = "gray75", alpha = 0.7, linewidth = 0.25) +
  geom_segment(data = ci_x_left, aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE, colour = "gray75", alpha = 0.7, linewidth = 0.4) +
  geom_segment(data = ci_x_right, aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE, colour = "gray75", alpha = 0.7, linewidth = 0.4) +
  geom_errorbar(aes(ymin = smape - smape_ci95, ymax = smape + smape_ci95), width = 0.04, colour = "gray75", alpha = 0.7, linewidth = 0.25) +
  geom_point(aes(fill = highlight), shape = 21, size = 4.3, colour = "gray20", stroke = 0.35) +
  geom_text_repel(
    data = df_all %>% filter(model != "foresight"),
    aes(label = model_label, fontface = label_face),
    size = 4, box.padding = 0.5, point.padding = 0.4,
    segment.size = 0.25, max.overlaps = Inf, seed = 42
  ) +
  geom_text_repel(
    data = df_all %>% filter(model == "foresight"),
    aes(label = model_label, fontface = label_face),
    size = 4, box.padding = 0.2, point.padding = 0.15,
    nudge_y = -0.2, direction = "y", segment.size = 0,
    segment.color = NA, max.overlaps = Inf, seed = 42
  ) +
  scale_fill_manual(values = c("Other" = "grey20", "ForeSight" = "#D95F02"), guide = "none") +
  scale_x_log10(breaks = c(20, 40, 80, 150, 250, 450, 700)) +
  scale_y_continuous(breaks = 5:9, limits = c(NA, 9)) +
  coord_cartesian(xlim = c(max(1, xlim[1] * 0.95), xlim[2] * 1.05)) +
  facet_wrap(
    ~dataset,
    nrow = 1,
    labeller = as_labeller(c(ro = "Rolling Origin", sa = "Steps Ahead"))
  ) +
  labs(x = "Consumo Energético (J)", y = expression(bold("sMAPE (%)"))) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.spacing.x = unit(1.2, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold.italic", size = 14)
  )

ggsave("pareto.pdf", g, width = 12.5, height = 5.5)
