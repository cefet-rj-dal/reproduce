library(dplyr)
library(ggplot2)
library(scales)
library(tibble)

time_wisard <- function() {
  df_plot <- tibble::tribble(
    ~model, ~dim, ~mean_time, ~sd_time,
    "ARIMA",16,1.1067,0.000,
    "ARIMA",32,1.1067,0.000,
    "ARIMA",64,1.1067,0.000,
    "ARIMA",128,1.1067,0.000,
    "ARIMA",256,1.1067,0.000,
    "Chronos",16,7.067,0.0128,
    "Chronos",32,7.067,0.0128,
    "Chronos",64,7.067,0.0128,
    "Chronos",128,7.067,0.0128,
    "Chronos",256,7.067,0.0128,
    "Conv1D",16,11.2433,0.3738,
    "Conv1D",32,11.2433,0.3738,
    "Conv1D",64,11.2433,0.3738,
    "Conv1D",128,11.2433,0.3738,
    "Conv1D",256,11.2433,0.3738,
    "CReW",16,0.283,0.0005,
    "CReW",32,0.283,0.0005,
    "CReW",64,0.283,0.0005,
    "CReW",128,0.283,0.0005,
    "CReW",256,0.283,0.0005,
    "ELM",16,2.3943,0.0247,
    "ELM",32,2.3943,0.0247,
    "ELM",64,2.3943,0.0247,
    "ELM",128,2.3943,0.0247,
    "ELM",256,2.3943,0.0247,
    "FS+ConvSPE",16,0.2734,0.0006,
    "FS+ConvSPE",32,0.3292,0.0006,
    "FS+ConvSPE",64,0.4418,0.0012,
    "FS+ConvSPE",128,0.6601,0.0009,
    "FS+ConvSPE",256,1.1073,0.0018,
    "FS+FE",16,0.2612,0.0005,
    "FS+FE",32,0.3113,0.0011,
    "FS+FE",64,0.4295,0.002,
    "FS+FE",128,0.6669,0.0015,
    "FS+FE",256,1.1731,0.0024,
    "FS+MSRFF",16,0.2627,0.0005,
    "FS+MSRFF",32,0.3122,0.0007,
    "FS+MSRFF",64,0.4222,0.0013,
    "FS+MSRFF",128,0.6385,0.0011,
    "FS+MSRFF",256,1.0872,0.002,
    "FS+PE",16,0.2577,0.0005,
    "FS+PE",32,0.3011,0.0007,
    "FS+PE",64,0.4016,0.0009,
    "FS+PE",128,0.5958,0.0006,
    "FS+PE",256,0.9837,0.0019,
    "FS+RFF",16,0.2586,0.0005,
    "FS+RFF",32,0.3071,0.0005,
    "FS+RFF",64,0.4159,0.0012,
    "FS+RFF",128,0.6314,0.0015,
    "FS+RFF",256,1.0845,0.0018,
    "FS+TRep",16,1.0401,0.0023,
    "FS+TRep",32,1.1186,0.0022,
    "FS+TRep",64,1.2976,0.0017,
    "FS+TRep",128,1.6579,0.0031,
    "FS+TRep",256,2.4588,0.0041,
    "FS+TS2Vec",16,0.6967,0.0019,
    "FS+TS2Vec",32,0.7414,0.0015,
    "FS+TS2Vec",64,0.8519,0.0027,
    "FS+TS2Vec",128,1.0615,0.0037,
    "FS+TS2Vec",256,1.6803,0.0064,
    "kNN",16,1.3645,0.0079,
    "kNN",32,1.3645,0.0079,
    "kNN",64,1.3645,0.0079,
    "kNN",128,1.3645,0.0079,
    "kNN",256,1.3645,0.0079,
    "LSTM",16,3.1926,0.0416,
    "LSTM",32,3.1926,0.0416,
    "LSTM",64,3.1926,0.0416,
    "LSTM",128,3.1926,0.0416,
    "LSTM",256,3.1926,0.0416,
    "MLP",16,3.6703,0.006,
    "MLP",32,3.6703,0.006,
    "MLP",64,3.6703,0.006,
    "MLP",128,3.6703,0.006,
    "MLP",256,3.6703,0.006,
    "ReW",16,0.2665,0.0007,
    "ReW",32,0.2665,0.0007,
    "ReW",64,0.2665,0.0007,
    "ReW",128,0.2665,0.0007,
    "ReW",256,0.2665,0.0007,
    "RFR",16,2.4937,0.0325,
    "RFR",32,2.4937,0.0325,
    "RFR",64,2.4937,0.0325,
    "RFR",128,2.4937,0.0325,
    "RFR",256,2.4937,0.0325,
    "SVR",16,3.0951,0.0061,
    "SVR",32,3.0951,0.0061,
    "SVR",64,3.0951,0.0061,
    "SVR",128,3.0951,0.0061,
    "SVR",256,3.0951,0.0061
  )
  
  model_levels <- c(
    "ARIMA", "Chronos", "Conv1D", "CReW", "ELM",
    "FS+ConvSPE", "FS+FE", "FS+MSRFF", "FS+PE",
    "FS+RFF", "FS+TRep", "FS+TS2Vec",
    "kNN", "LSTM", "MLP", "ReW", "RFR", "SVR"
  )
  
  # Exclude missing or unexpected model labels to prevent an NA legend entry.
  df_plot <- df_plot |>
    filter(!is.na(model), model %in% model_levels) |>
    mutate(
      model = factor(model, levels = model_levels),
      linetype = case_when(
        as.character(model) %in% c("ReW", "CReW") ~ "dotted",
        grepl("^FS", as.character(model))         ~ "solid",
        TRUE                                      ~ "dotted"
      )
    )
  
  model_colors <- c(
    "ARIMA"      = "#D62728",
    "Chronos"    = "#E69F00",
    "Conv1D"     = "#009E73",
    "CReW"       = "#8B4513",
    "ELM"        = "#CC79A7",
    "FS+ConvSPE" = "#56B4E9",
    "FS+FE"      = "#D55E00",
    "FS+MSRFF"   = "#B8860B",
    "FS+PE"      = "#332288",
    "FS+RFF"     = "#A9A9A9",
    "FS+TRep"    = "#44AA99",
    "FS+TS2Vec"  = "#AA4499",
    "kNN"        = "#000000",
    "LSTM"       = "#0072B2",
    "MLP"        = "#DDCC77",
    "ReW"        = "#117733", 
    "RFR"        = "#661100",
    "SVR"        = "#999933"
  )
  
  model_linetypes <- ifelse(
    model_levels %in% c("ReW", "CReW"),
    "dashed",
    ifelse(grepl("^FS", model_levels), "solid", "11")
  )
  names(model_linetypes) <- model_levels
  
  # Label non-FS models to the right of their final points, with fixed vertical offsets.
  label_gap <- 8
  end_labels <- df_plot |>
    filter(!grepl("^FS", as.character(model))) |>
    group_by(model) |>
    slice_max(order_by = dim, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      label = as.character(model),
      label_x = max(df_plot$dim, na.rm = TRUE) + label_gap,
      label_y = mean_time + case_when(
        label == "MLP"   ~  0.10,
        label == "LSTM"  ~  0.15,
        label == "SVR"   ~ -0.15,
        label == "RFR"   ~  0.11,
        label == "ELM"   ~ -0.11,
        label == "CReW"  ~  0.01,
        label == "ReW"   ~ -0.01,
        label == "ARIMA" ~  0.00,
        TRUE ~ 0
      )
    )
  
  ggplot(
    df_plot,
    aes(
      x = dim,
      y = mean_time,
      color = model,
      group = model,
      linetype = linetype
    )
  ) +
    geom_line(
      aes(linewidth = grepl("^FS", as.character(model)))
    ) +
    scale_linewidth_manual(
      values = c("TRUE" = 1.4, "FALSE" = 0.9),
      guide = "none"
    ) +
    geom_text(
      data = end_labels,
      aes(x = label_x, y = label_y, label = label, color = model),
      inherit.aes = FALSE,
      hjust = 0,
      size = 6.0,
      fontface = "plain",
      show.legend = FALSE
    ) +
    scale_x_continuous(
      trans = "log2",
      breaks = c(16, 32, 64, 128, 256),
      expand = expansion(mult = c(0.01, 0.075))
    ) +
    scale_y_log10(
      breaks = c(0.25, 0.5, 1, 2, 4, 8, 12),
      labels = label_number(decimal.mark = ".", big.mark = ",")
    ) +
    scale_color_manual(
      values = model_colors,
      # Only FS variants remain in the legend; other models are labeled on the plot.
      breaks = model_levels[grepl("^FS", model_levels)],
      drop = TRUE,
      na.translate = FALSE
    ) +
    scale_linetype_manual(
      values = c("solid" = "solid", "dashed" = "dashed", "dotted" = "11"),
      guide = "none"
    ) +
    guides(
      color = guide_legend(
        title = "ForeSight variants: ",
        nrow = 1,
        byrow = TRUE,
        order = 1,
        keywidth = unit(1.0, "cm"),
        override.aes = list(
          linetype = rep("solid", sum(grepl("^FS", model_levels))),
          linewidth = rep(1.4, sum(grepl("^FS", model_levels)))
        )
      )
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = expression(bold("Embedding Dimension")),
      y = "Execution Time (s)"
    ) +
    theme_bw(base_size = 17) +
    theme(
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18, face = "bold"),
      legend.position = "top",
      legend.background = element_rect(
        fill = alpha("white", 0.85),
        colour = NA
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 22, 8, 8),
      legend.spacing.y = unit(0, "pt"),
      legend.margin = margin(0, 0, 0, 0),
    )
}

p <- time_wisard()
ggsave(
  filename = "time.pdf",
  plot = p,
  device = cairo_pdf,
  width = 16,
  height = 8
)
