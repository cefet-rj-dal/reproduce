## Preprocessing and augmentation analysis plots
# Purpose: Load saved result .RData objects, compute summaries, and generate
# grouped bar charts comparing preprocessing and augmentation strategies.

library(daltoolbox)
library(ggplot2)
library(dplyr)
library(reshape)
library(RColorBrewer)

colors <- brewer.pal(7, 'Set1')

# Consistent font size across charts
font <- theme(text = element_text(size=16))


load("saved/result_mlp_prep_is.RData")
load("saved/result_mlp_diff_is.RData")
load("saved/result_mlp_an_is.RData")
load("saved/result_mlp_gminmax_is.RData")
load("saved/result_mlp_swminmax_is.RData")
load("saved/result_mlp_swminmax_jitter.RData")
load("saved/result_mlp_swminmax_stretch.RData")

load("saved/result_elm_an_is.RData")
load("saved/result_rfr_an_is.RData")
load("saved/result_svm_an_is.RData")
load("saved/result_conv1d_an_is.RData")
load("saved/result_lstm_an_is.RData")

adjust_data <- function(data) {
  # Human-friendly labels for plots
  if (!is.null(data$name))
    data$name <- factor(data$name, levels=c("brazil_k2o", "brazil_n", "brazil_p2o5"), labels=c("K2O", "N", "P2O5"))
  if (!is.null(data$method))
    data$method <- factor(data$method, levels=c("ts_conv1d", "ts_elm", "ts_mlp", "ts_rf", "ts_svm", "ts_tlstm"),
                          labels=c("conv1d", "elm", "mlp", "rfr", "svm", "lstm"))
  data$preprocess <- factor(data$preprocess, levels=c("ts_swminmax", "ts_diff", "ts_an", "ts_gminmax"),
                            labels=c("sw min-max", "diff", "an", "min-max"))
  data$augment <- factor(data$augment, levels=c("ts_augment", "jitter", "stretch"),
                         labels=c("none", "jitter", "stretch"))
  return(data)
}

data <- NULL
data <- rbind(data, result_mlp_diff_is)
data <- rbind(data, result_mlp_an_is)
data <- rbind(data, result_mlp_gminmax_is)
data <- rbind(data, result_mlp_swminmax_is)
data <- data |>  dplyr::filter(test_size == 4) |> dplyr::arrange(name, method, model, test_size, smape_test)
data <- adjust_data(data) |> group_by(name, preprocess) |> summarize(test=mean(smape_test))
levels(data$preprocess) <- c("swminmax", "diff", "an", "gminmax")

prep_data <- cast(data, name ~ preprocess, mean)
# Preview and plot
head(prep_data)
grf <- plot_groupedbar(prep_data, colors=colors[1:4]) + font
breaks <- seq(0, 14, by = 1)
labels <- as.character(breaks)
labels[breaks %% 2 == 1] <- ""
grf <- grf + scale_y_continuous(breaks = breaks, labels = labels)
plot(grf)
ggsave("preprocess.png", width = 15, height = 10, units = "cm")


data <- NULL
data <- rbind(data, result_mlp_swminmax_is)
data <- rbind(data, result_mlp_swminmax_jitter)
data <- rbind(data, result_mlp_swminmax_stretch)
data <- data |>  dplyr::filter(test_size == 4) |> dplyr::arrange(name, method, model, test_size, smape_test)
data <- adjust_data(data) |> group_by(name, augment) |> summarize(test=mean(smape_test))
aug_data <- cast(data, name ~ augment, mean)
# Preview and plot
head(aug_data)
grf <- plot_groupedbar(aug_data, colors=colors[1:4]) + font
breaks <- seq(0, 15, by = 1)
labels <- as.character(breaks)
labels[breaks %% 2 == 1] <- ""
grf <- grf + scale_y_continuous(breaks = breaks, labels = labels)
plot(grf)
ggsave("augment.png", width = 15, height = 10, units = "cm")


data <- NULL
data <- rbind(data, result_mlp_an_is)
data <- rbind(data, result_elm_an_is)
data <- rbind(data, result_rfr_an_is)
data <- rbind(data, result_svm_an_is)
data <- rbind(data, result_conv1d_an_is)
data <- rbind(data, result_lstm_an_is)
data <- data |>  dplyr::filter(test_size == 4 & name == 'brazil_p2o5') |> dplyr::arrange(name, method, model, test_size, smape_test)
method_data <- adjust_data(data) |> group_by(method) |> summarize(test=mean(smape_test)) |> dplyr::select(x = method, value = test)
grf <- plot_bar(method_data, colors=colors[c(1:5,7)]) + font
breaks <- seq(0, 20, by = 1)
labels <- as.character(breaks)
labels[breaks %% 2 == 1] <- ""
grf <- grf + scale_y_continuous(breaks = breaks, labels = labels)
plot(grf)
ggsave("method.png", width = 15, height = 10, units = "cm")


