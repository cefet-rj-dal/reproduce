library(dplyr)
library(readr)
library(scales)
library(ggplot2)


plot_execution_time <- function(df, n_bins = 7) {
  df_series <- df %>%
    filter(strategy == "sa", model %in% c("arima", "mlp", "foresight")) %>%
    #filter(strategy == "sa", model %in% c("regwisard", "arima", "foresight", "knn")) %>%
    group_by(model, ts, size) %>%
    summarise(time_sec = mean(time_sec, na.rm = TRUE), .groups = "drop")
  df_plot <- df_series %>%
    mutate(model = recode( model,
                           arima = "ARIMA",
                           mlp = "MLP",
                           foresight = "ForeSight",
                           regwisard = "CReW",
                           knn = "kNN" ),
           size_bin = ntile(size, n_bins)) %>%
    group_by(model, size_bin) %>%
    summarise(size_mean = mean(size), time_mean = mean(time_sec), sd_time = sd(time_sec), n = n(), .groups = "drop") %>%
    mutate( se = sd_time / sqrt(n),
            ci_low = time_mean - se, ci_high = time_mean + se )
  ggplot(df_plot, aes(x = size_mean, y = time_mean, color = model, fill = model, group = model)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.20, color = NA) +
    geom_line(linewidth = 1.2) +
    scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(x = "Comprimento da Série", y = "Tempo de Execução (s)", color = NULL, fill = NULL) +
    theme_bw(base_size = 14) +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.position = c(0.03, 0.97),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = alpha("white", 0.85), colour = "grey80"),
      panel.grid.minor = element_blank()
    )
}


time_wisard <- function() {
  df_plot <- tibble::tribble(
    ~model, ~dim, ~mean_time, ~sd_time,
    "FS+Conv", 16, 0.3243, 0.007,
    "FS+Conv", 32, 0.3852, 0.006,
    "FS+Conv", 64, 0.5086, 0.014,
    "FS+Conv", 128, 0.7470, 0.012,
    "FS+Conv", 256, 1.2342, 0.015,
    "FS+Fourier", 16, 0.3115, 0.004,
    "FS+Fourier", 32, 0.3662, 0.014,
    "FS+Fourier", 64, 0.4947, 0.024,
    "FS+Fourier", 128, 0.7531, 0.016,
    "FS+Fourier", 256, 1.2988, 0.022,
    "FS+RFF", 16, 0.3088, 0.008,
    "FS+RFF", 32, 0.3619, 0.005,
    "FS+RFF", 64, 0.4811, 0.013,
    "FS+RFF", 128, 0.7169, 0.017,
    "FS+RFF", 256, 1.2091, 0.017,
    "FS+MSRFF", 16, 0.3078, 0.002,
    "FS+MSRFF", 32, 0.3603, 0.008,
    "FS+MSRFF", 64, 0.4813, 0.009,
    "FS+MSRFF", 128, 0.7157, 0.014,
    "FS+MSRFF", 256, 1.2005, 0.038,
    "FS+Pos", 16, 0.3080, 0.005,
    "FS+Pos", 32, 0.3556, 0.007,
    "FS+Pos", 64, 0.4665, 0.011,
    "FS+Pos", 128, 0.6810, 0.006,
    "FS+Pos", 256, 1.1075, 0.019,
    "FS+TS2Vec", 16, 0.7650, 0.019,
    "FS+TS2Vec", 32, 0.8147, 0.014,
    "FS+TS2Vec", 64, 0.9365, 0.028,
    "FS+TS2Vec", 128, 1.1677, 0.036,
    "FS+TS2Vec", 256, 1.6287, 0.063,
    "FS+TRep", 16, 1.1360, 0.024,
    "FS+TRep", 32, 1.2187, 0.023,
    "FS+TRep", 64, 1.4109, 0.0017,
    "FS+TRep", 128, 1.7922, 0.030,
    "FS+TRep", 256, 2.5375, 0.039,
    "CReW", 16, 0.3158, 0.004,
    "CReW", 32, 0.3158, 0.004,
    "CReW", 64, 0.3158, 0.004,
    "CReW", 128, 0.3158, 0.004,
    "CReW", 256, 0.3158, 0.004,
    "ReW", 16, 0.2990, 0.005,
    "ReW", 32, 0.2990, 0.005,
    "ReW", 64, 0.2990, 0.005,
    "ReW", 128, 0.2990, 0.005,
    "ReW", 256, 0.2990, 0.005
  ) %>%
    mutate(
      ymin = pmax(0, mean_time - sd_time),
      ymax = mean_time + sd_time,
      linetype = "solid",
      model = factor(model, levels = c("FS+Conv", "FS+Fourier", "FS+RFF", "FS+MSRFF", "FS+Pos", "FS+TS2Vec", "FS+TRep", "CReW", "ReW"))
    )
  cols <- c("#1F77B4", "#FF7F0E", "#8C564B", "#E377C2", "#2CA02C", "#9467BD", "#17BECF")
  cores <- c("FS+Conv"=cols[1], "FS+Fourier"=cols[2], "FS+RFF"=cols[3], "FS+MSRFF"=cols[4], "FS+Pos"=cols[5], "FS+TS2Vec"=cols[6], "FS+TRep"=cols[7], "CReW"="#000000", "ReW"="#D62728")
  ggplot(df_plot, aes(x = dim, y = mean_time, color = model, fill = model, group = model, linetype = linetype)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15, colour = NA, show.legend = FALSE) +
    geom_line(linewidth = 0.6) +
    # geom_point(data = subset(df_plot, !model %in% c("CReW", "ReW")), size = 2) +
    scale_x_continuous(trans = "log2", breaks = c(16, 32, 64, 128, 256)) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", big.mark = ".")) +
    scale_linetype_identity() +
    scale_color_manual(values = cores) +
    scale_fill_manual(values = cores) +
    guides(color = guide_legend(ncol = 2), fill = "none") +
    labs(x = expression(bold("Dimensão do " * bolditalic("Embedding"))), y = "Tempo de Execução (s)", color = NULL) +
    theme_bw(base_size = 14) +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = alpha("white", 0.85), colour = "grey80"),
      panel.grid.minor = element_blank()
    )
}


smape_wisard <- function() {
  df_sa <- tibble::tribble(
    ~model, ~dim, ~mean_smape, ~sd_smape,
    "FS+Conv", 16, 13.9308, 0.0542,
    "FS+Conv", 32, 14.1040, 0.0372,
    "FS+Conv", 64, 13.9628, 0.0170,
    "FS+Conv", 128, 13.9811, 0.0508,
    "FS+Conv", 256, 13.9424, 0.0286,
    "FS+Fourier", 16, 14.5486, 0.0794,
    "FS+Fourier", 32, 14.2976, 0.0294,
    "FS+Fourier", 64, 14.1899, 0.0282,
    "FS+Fourier", 128, 14.1482, 0.0333,
    "FS+Fourier", 256, 14.0941, 0.0291,
    "FS+RFF", 16, 13.6085, 0.0256,
    "FS+RFF", 32, 13.5032, 0.0398,
    "FS+RFF", 64, 13.4659, 0.0415,
    "FS+RFF", 128, 13.4384, 0.0234,
    "FS+RFF", 256, 13.4595, 0.0315,
    "FS+MSRFF", 16, 12.9034, 0.0159,
    "FS+MSRFF", 32, 12.8955, 0.0352,
    "FS+MSRFF", 64, 12.8793, 0.0179,
    "FS+MSRFF", 128, 12.9014, 0.0102,
    "FS+MSRFF", 256, 12.5724, 0.0143,
    "FS+Pos", 16, 13.5867, 0.0935,
    "FS+Pos", 32, 13.4699, 0.0542,
    "FS+Pos", 64, 13.4453, 0.0315,
    "FS+Pos", 128, 13.3887, 0.0256,
    "FS+Pos", 256, 13.3775, 0.0209,
    "FS+TS2Vec", 16, 13.7326, 0.0457,
    "FS+TS2Vec", 32, 13.9671, 0.0529,
    "FS+TS2Vec", 64, 13.9753, 0.0276,
    "FS+TS2Vec", 128, 13.9836, 0.0144,
    "FS+TS2Vec", 256, 13.4473, 0.0311,
    "FS+TRep", 16, 14.3890, 0.0289,
    "FS+TRep", 32, 14.1213, 0.0396,
    "FS+TRep", 64, 14.2013, 0.0460,
    "FS+TRep", 128, 14.0485, 0.0230,
    "FS+TRep", 256, 13.7759, 0.0081
  ) %>%
    mutate(
      model = factor(model, levels = c("FS+Conv", "FS+Fourier", "FS+RFF", "FS+MSRFF", "FS+Pos", "FS+TS2Vec", "FS+TRep"))
    )
  df_ro <- tibble::tribble(
    ~model, ~dim, ~mean_smape, ~sd_smape,
    "FS+Conv", 16, 8.2595, 0.0115,
    "FS+Conv", 32, 8.3231, 0.0205,
    "FS+Conv", 64, 8.2597, 0.0088,
    "FS+Conv", 128, 8.2399, 0.0042,
    "FS+Conv", 256, 8.1372, 0.0077,
    "FS+Fourier", 16, 8.5084, 0.0176,
    "FS+Fourier", 32, 8.3618, 0.0074,
    "FS+Fourier", 64, 8.2982, 0.0098,
    "FS+Fourier", 128, 8.2499, 0.0075,
    "FS+Fourier", 256, 8.2499, 0.0053,
    "FS+RFF", 16, 8.0897, 0.0145,
    "FS+RFF", 32, 8.0402, 0.0096,
    "FS+RFF", 64, 8.0157, 0.0055,
    "FS+RFF", 128, 7.9854, 0.0121,
    "FS+RFF", 256, 7.9769, 0.0116,
    "FS+MSRFF", 16, 7.9137, 0.0093,
    "FS+MSRFF", 32, 7.9446, 0.0114,
    "FS+MSRFF", 64, 7.9359, 0.0045,
    "FS+MSRFF", 128, 7.9350, 0.0034,
    "FS+MSRFF", 256, 7.9345, 0.0060,
    "FS+Pos", 16, 8.0956, 0.0168,
    "FS+Pos", 32, 8.0263, 0.0111,
    "FS+Pos", 64, 7.9897, 0.0061,
    "FS+Pos", 128, 7.9702, 0.0127,
    "FS+Pos", 256, 7.9524, 0.0063,
    "FS+TS2Vec", 16, 8.1292, 0.0063,
    "FS+TS2Vec", 32, 7.9807, 0.0117,
    "FS+TS2Vec", 64, 8.0096, 0.0048,
    "FS+TS2Vec", 128, 7.9698, 0.0028,
    "FS+TS2Vec", 256, 8.0477, 0.0195,
    "FS+TRep", 16, 8.1535, 0.0255,
    "FS+TRep", 32, 8.1479, 0.0175,
    "FS+TRep", 64, 8.1794, 0.0101,
    "FS+TRep", 128, 8.1255, 0.0068,
    "FS+TRep", 256, 8.1060, 0.0062
  ) %>%
    mutate(
      model = factor(model, levels = c("FS+Conv", "FS+Fourier", "FS+RFF", "FS+MSRFF", "FS+Pos", "FS+TS2Vec", "FS+TRep"))
    )
  df_plot <- bind_rows(
    mutate(df_sa, source = "SA"),
    mutate(df_ro, source = "RO")
  )
  cols <- c("#1F77B4", "#FF7F0E", "#8C564B", "#E377C2", "#2CA02C", "#9467BD",  "#17BECF")
  ggplot(df_plot, aes(x = dim, y = mean_smape, color = model, group = interaction(model, source), linetype = source)) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(trans = "log2", breaks = c(16, 32, 64, 128, 256)) +
    scale_y_continuous(
      labels = label_number(decimal.mark = ",", big.mark = "."),
      limits = c(0, 15)
    ) +
    scale_color_manual(
      values = c(
        "FS+Conv"    = cols[1],
        "FS+Fourier" = cols[2],
        "FS+RFF"     = cols[3],
        "FS+MSRFF"   = cols[4],
        "FS+Pos"     = cols[5],
        "FS+TS2Vec"  = cols[6],
        "FS+TRep"    = cols[7]
      )
    ) +
    scale_linetype_manual(
      name = expression(bold("Legenda:")),
      values = c(SA = "solid", RO = "solid"),
      labels = c(SA = expression(italic("Steps Ahead")), RO = expression(italic("Rolling Origin")))
    ) +
    guides(
      linetype = guide_legend(
        nrow = 1,
        ncol = 2,
        byrow = TRUE,
        order = 1,
        direction = "horizontal",
        title.position = "left",
        label.position = "right"
      ),
      color = guide_legend(nrow = 1, ncol = 7, byrow = TRUE, order = 2)
    ) +
    labs(x = expression(bold("Dimensão do " * bolditalic("Embedding"))), y = "SMAPE (%)", color = NULL) +
    theme_bw(base_size = 14) +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 12, face = "plain"),
      legend.text = element_text(size = 12),
      legend.position = c(0.5, 0.02),
      legend.justification = c(0.5, 0),
      legend.background = element_rect(fill = alpha("white", 0.85)),
      legend.box = "vertical",
      legend.box.spacing = unit(0, "pt"),
      legend.spacing.y = unit(10, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


smape <- read_delim("smape_series.csv", delim = ";", locale = locale(decimal_mark = ","), show_col_types = FALSE)
ts_sizes <- read_csv("ts.csv", col_names = c("ts", "size"), show_col_types = FALSE) %>%
  mutate(ts = as.character(ts), size = as.numeric(size)) %>%
  distinct(ts, .keep_all = TRUE)
df <- smape %>% mutate(ts = as.character(ts)) %>% left_join(ts_sizes, by = "ts")
p <- plot_execution_time(df)
ggsave(filename = "scalability.pdf", plot = p, device = cairo_pdf, width = 10, height = 6)


#p <- time_wisard()
#ggsave(filename = "time_wisard.pdf", plot = p, device = cairo_pdf, width = 10,  height = 6)


#p <- smape_wisard()
#ggsave(filename = "smape_wisard.pdf", plot = p, device = cairo_pdf, width = 10, height = 6)
