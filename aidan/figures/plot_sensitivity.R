library(dplyr)
library(ggplot2)
library(cowplot)

obj <- load('figures/sensitivity.rdata')
df <- get(obj[1])

df_plot <- df %>%
  group_by(strategy, n, instance) %>%
  summarise(smape_mean = mean(smape), .groups = 'drop')

colors <- c(aidan = '#8DB3E2', baseline = '#F6B26B', naive = '#93C47D')
instance_labels <- c(aidan = 'AIDAN', baseline = 'Baseline', naive = 'Naive')

make_plot <- function(data, strategy_label) {
  ggplot(data, aes(x = factor(n), y = smape_mean, fill = instance)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.9) +
    geom_text(aes(label = sprintf('%.2f', smape_mean)),
              position = position_dodge(width = 0.9),
              vjust = 0.45,
              hjust = 1.1,
              size = 6,
              angle = 90,
              color = 'black',
              fontface = 'bold') +
    scale_fill_manual(values = colors, labels = instance_labels) +
    scale_y_continuous(breaks = seq(0, 26.8, by=5)) +
    labs(
      title = strategy_label,
      x = '# Training Observations',
      y = 'SMAPE (%)'
    ) +
    coord_cartesian(ylim = c(0, 26.8)) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 0.5, face = 'bold'),
      legend.position = 'none',
      axis.text.x = element_text(size = 16, color = 'black'),
      axis.text.y = element_text(size = 16, color = 'black'),
      axis.title = element_text(size = 18, color = 'black', face = 'bold')
    )
}

p_ro <- make_plot(df_plot %>% filter(strategy == 'ro'), 'Rolling Origin')
p_sa <- make_plot(df_plot %>% filter(strategy == 'sa'), 'Steps Ahead')

legend_df <- data.frame(instance = c('aidan', 'baseline', 'naive'), val = c(1,1,1))
legend_plot <- ggplot(legend_df, aes(x = instance, y = val, fill = instance)) +
  geom_col() +
  scale_fill_manual(values = colors, labels = instance_labels, name = '') +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.text = element_text(size = 20, color = 'black')
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
legend_grob <- get_legend(legend_plot)

top_row <- plot_grid(p_ro, p_sa, ncol = 2, align = 'hv', labels = NULL, rel_widths = c(1,1))
final <- plot_grid(top_row, legend_grob, ncol = 1, rel_heights = c(1, 0.12))

ggsave('figures/fig_smape_sensitivity.pdf', plot = final, width = 16, height = 6, dpi = 300)
