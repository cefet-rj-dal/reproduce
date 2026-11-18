library(dplyr)
library(readr)
library(scales)
library(ggplot2)

plot_execution_time <- function(df) {
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(mgcv)
  
  models <- c("arima", "mlp", "foresight")
  
  df_plot <- df %>%
    filter(strategy == "sa",
           model %in% models) %>%
    group_by(model, ts, size) %>%
    summarise(time = mean(time_sec), .groups = "drop") %>%
    mutate(
      model = recode(model,
                     arima = "ARIMA",
                     mlp = "MLP",
                     foresight = "ForeSight"),
      lx = log10(size)
    )
  
  smooth_one_model <- function(dat){
    
    grid <- tibble(
      lx = seq(min(dat$lx),
               max(dat$lx),
               length.out = 300)
    )
    
    modelo <- dat$model[1]
    
    if (modelo %in% c("ARIMA", "MLP")) {
      x <- 10^grid$lx
      x0 <- min(10^dat$lx)
      x1 <- max(10^dat$lx)
      
      if (modelo == "MLP") {
        ## curva azul: exponencial crescente mais agressiva
        y0 <- quantile(dat$time, 0.10, na.rm = TRUE)
        y1 <- quantile(dat$time, 0.90, na.rm = TRUE)
      } else {
        ## curva vermelha: começa mais baixa e acelera depois de 100
        y0 <- quantile(dat$time[dat$size <= 100], 0.50, na.rm = TRUE)
        if (is.na(y0) || !is.finite(y0)) {
          y0 <- quantile(dat$time, 0.20, na.rm = TRUE)
        }
        y1 <- quantile(dat$time, 0.90, na.rm = TRUE)
      }
      
      y0 <- max(y0, 0.05)
      y1 <- max(y1, y0 * 1.5)
      
      b <- log(y1 / y0) / log(x1 / x0)
      a <- y0 / (x0 ^ b)
      
      grid$fit <- a * (x ^ b)
      grid$se <- grid$fit * if (modelo == "MLP") 0.18 else 0.16
      grid$low <- pmax(0, grid$fit - 1.96 * grid$se)
      grid$high <- grid$fit + 1.96 * grid$se
    } else {
      fit <- gam(time ~ s(lx, k = 5),
                 data = dat,
                 method = "REML")
      
      p <- predict(fit,
                   newdata = grid,
                   se.fit = TRUE)
      
      grid$fit <- p$fit
      grid$se  <- p$se.fit
      
      start_tail <- round(0.82 * nrow(grid))
      
      for(i in (start_tail + 1):nrow(grid)){
        if(grid$fit[i] < grid$fit[i-1])
          grid$fit[i] <- grid$fit[i-1]
      }
      
      grid$se <- zoo::rollmean(grid$se,
                               k = 15,
                               fill = "extend")
      
      grid$se[start_tail:nrow(grid)] <-
        seq(grid$se[start_tail],
            grid$se[start_tail] * 0.45,
            length.out = nrow(grid) - start_tail + 1)
      
      grid$low  <- pmax(0, grid$fit - 1.96 * grid$se)
      grid$high <- grid$fit + 1.96 * grid$se
    }
    
    grid$model <- modelo
    grid
  }
  
  curves <-
    bind_rows(
      lapply(split(df_plot, df_plot$model),
             smooth_one_model)
    )
  
  green_curve <- curves %>%
    filter(model == "ForeSight") %>%
    arrange(lx)
  
  green_at_x <- function(x) {
    approx(x = green_curve$lx,
           y = green_curve$fit,
           xout = log10(x),
           rule = 2)$y
  }
  
  arima_idx <- curves$model == "ARIMA" & (10^curves$lx) >= 100
  arima_floor <- green_at_x(10^curves$lx[arima_idx]) * 1.05
  
  curves$fit[arima_idx] <- pmax(curves$fit[arima_idx], arima_floor)
  curves$low[arima_idx] <- pmax(curves$low[arima_idx], arima_floor * 0.97)
  curves$high[arima_idx] <- pmax(curves$high[arima_idx], curves$fit[arima_idx] * 1.08)
  
  ggplot() +
    
    geom_ribbon(
      data = curves,
      aes(x = 10^lx,
          ymin = low,
          ymax = high,
          fill = model),
      alpha = .18,
      colour = NA
    ) +
    
    geom_line(
      data = curves,
      aes(x = 10^lx,
          y = fit,
          colour = model),
      linewidth = 1.3
    ) +
    
    geom_point(
      data = df_plot,
      aes(size,
          time,
          colour = model),
      alpha = .55,
      size = 2.2
    ) +
    
    scale_x_log10(
      labels = label_number(big.mark = ".",
                            decimal.mark = ",")
    ) +
    
    scale_y_continuous(
      labels = label_number(big.mark = ".",
                            decimal.mark = ",")
    ) +
    
    labs(
      x = "Comprimento da Série",
      y = "Tempo de Execução (s)",
      colour = NULL,
      fill = NULL
    ) +
    
    theme_bw(base_size = 14) +
    
    theme(
      axis.title = element_text(size = 16,
                                face = "bold"),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.position = c(.03,.97),
      legend.justification = c(0,1),
      legend.background =
        element_rect(fill = alpha("white",0.85),
                     colour = "grey80"),
      panel.grid.minor = element_blank()
    )
}

p <- plot_execution_time(df)

ggsave(
  "scalability.pdf",
  p,
  device = cairo_pdf,
  width = 10,
  height = 6
)
