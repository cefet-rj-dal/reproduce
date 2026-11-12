library(ggplot2)
library(dplyr)
library(readr)

ranking_plot <- function(csv_file="rank_data.csv", steps="RO", file=NULL) {
  
  # Ler os dados
  df <- readr::read_csv(csv_file, show_col_types = FALSE)
  
  # Filtrar o cenário
  df <- df |>
    dplyr::filter(steps == !!steps) |>
    dplyr::arrange(mean_rank)

  # Calcular os limites do intervalo de confiança
  summary_rank <- df |>
    dplyr::mutate(
      lower = mean_rank - ci_rank,
      upper = mean_rank + ci_rank,
      model = factor(model, levels = model)
    )
  
  # Faixa do melhor modelo
  best_lower <- summary_rank$lower[1]
  best_upper <- summary_rank$upper[1]
  best_band <- tibble::tibble(
    xmin = best_lower,
    xmax = best_upper
  )
  
  # Identificar intervalos que se sobrepõem ao melhor modelo
  summary_rank <- summary_rank |>
    dplyr::mutate(
      overlap_best = (lower <= best_upper) &
        (upper >= best_lower)
    )
  
  # Construir o gráfico
  p <- ggplot(
    summary_rank,
    aes(x = mean_rank, y = model)
  ) +
    geom_rect(
      data = best_band,
      inherit.aes = FALSE,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "#1f77b4",
      alpha = 0.10
    ) +
    geom_errorbarh(
      aes(xmin = lower, xmax = upper),
      height = 0.16,
      linewidth = 0.7,
      color = "grey25"
    ) +
    geom_point(
      aes(color = overlap_best),
      size = 3.0
    ) +
    scale_color_manual(
      values = c(
        `TRUE` = "#d62728",
        `FALSE` = "grey20"
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      "Avg. Rank",
      breaks = seq(7, 17, by = 2),
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    coord_cartesian(
      xlim = c(6, NA),
      clip = "on"
    ) +
    ylab(NULL) +
    ggtitle(NULL) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(
        size = 16,
        face = "plain"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        color = "grey88",
        linewidth = 0.4
      ),
      axis.line = element_line(
        linewidth = 0.4,
        color = "black"
      )
    )
  
  # Salvar o gráfico, se um arquivo for informado
  if (!is.null(file)) {
    ggsave(
      filename = file,
      plot = p,
      width = 8,
      height = max(4, 0.35 * nrow(summary_rank) + 1),
      dpi = 300
    )
  }
  
  return(invisible(p))
}


# Gerar os gráficos de RO e SA
current_path <- getwd()

p_ro <- ranking_plot(
  csv_file = file.path(current_path, "rank_data.csv"),
  steps = "RO",
  file = file.path(current_path, "rank_ro.pdf")
)

p_sa <- ranking_plot(
  csv_file = file.path(current_path, "rank_data.csv"),
  steps = "SA",
  file = file.path(current_path, "rank_sa.pdf")
)
