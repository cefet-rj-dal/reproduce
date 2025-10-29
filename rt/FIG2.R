# Load libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(readr)

# Load data
metrics <- read_csv("~/Tese/Artigo/metrics_homo.csv")

# Filter and select relevant columns
data <- subset(metrics, method!="rtad", select=c("method", "series", "f1", "f1_RT", "precision", "precision_RT", "recall", "recall_RT"))

# Reshape data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(precision, recall, f1, precision_RT, recall_RT, f1_RT),
    names_to = c("metric", "type"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    type = ifelse(is.na(type), "original", toupper(type)),
    value = as.numeric(value) # <--- ENSURES 'value' IS NUMERIC
  ) %>%
  filter(!is.na(value)) # Filter out NAs that might have appeared during conversion

calculate_metric_comparison <- function(df_long, current_metric) {
  # Pivot to compare 'original' and 'RT' side by side for each series
  comparison_data <- df_long %>%
    filter(metric == current_metric) %>%
    select(method, series, type, value) %>%
    pivot_wider(
      names_from = type,
      values_from = value,
      names_prefix = paste0(current_metric, "_"),
      values_fn = mean
    )
  
  original_col_name <- paste0(current_metric, "_original")
  rt_col_name <- paste0(current_metric, "_RT")
  
  resumo_comparacao <- comparison_data %>%
    mutate(
      # Check if comparison is possible (both values exist)
      can_compare = !is.na(.data[[rt_col_name]]) & !is.na(.data[[original_col_name]]),
      
      # Calculate improvement, deterioration, and tie
      improved = ifelse(can_compare, .data[[rt_col_name]] > .data[[original_col_name]], FALSE),
      deteriorated = ifelse(can_compare, .data[[rt_col_name]] < .data[[original_col_name]], FALSE),
      # Tie if can_compare is TRUE and neither improved nor deteriorated
      tied = ifelse(can_compare & !improved & !deteriorated, TRUE, FALSE)
    ) %>%
    group_by(method) %>%
    summarise(
      improved_count = sum(improved, na.rm = TRUE),
      deteriorated_count = sum(deteriorated, na.rm = TRUE),
      tied_count = sum(tied, na.rm = TRUE),
      total_series_compared = sum(can_compare, na.rm = TRUE), # Total series where comparison was valid
      .groups = "drop"
    ) %>%
    mutate(
      metric = current_metric,
      # Avoid division by zero if no series to compare
      percent_improved = ifelse(total_series_compared == 0, 0, round(100 * improved_count / total_series_compared, 1)),
      percent_deteriorated = ifelse(total_series_compared == 0, 0, round(100 * deteriorated_count / total_series_compared, 1)),
      percent_tied = ifelse(total_series_compared == 0, 0, round(100 * tied_count / total_series_compared, 1))
    )
  
  return(resumo_comparacao)
}

# Calculate and combine comparison summaries for all metrics
metrics_to_compare <- c("f1", "precision", "recall")
all_metrics_comparison_list <- lapply(metrics_to_compare, function(m) calculate_metric_comparison(data_long, m))
all_metrics_comparison_df <- do.call(rbind, all_metrics_comparison_list)

# Prepare data for stacked bar plot
stacked_bar_data <- all_metrics_comparison_df %>%
  pivot_longer(
    cols = c(percent_improved, percent_deteriorated, percent_tied),
    names_to = "comparison_type",
    values_to = "percentage"
  ) %>%
  mutate(
    comparison_type = case_when(
      comparison_type == "percent_improved" ~ "RT Improved",
      comparison_type == "percent_deteriorated" ~ "RT Deteriorated",
      comparison_type == "percent_tied" ~ "RT Tied"
    )
  )

# Define the order of categories for stacking and facet labels
stacked_bar_data$comparison_type <- factor(stacked_bar_data$comparison_type,
                                           levels = c("RT Deteriorated", "RT Tied", "RT Improved")) # Logical order

stacked_bar_data$metric <- factor(stacked_bar_data$metric,
                                  levels = c("f1", "precision", "recall"),
                                  labels = c("F1-score", "Precision", "Recall"))

# Ensure alphabetical order of methods on the Y-axis
stacked_bar_data$method <- factor(stacked_bar_data$method, levels = sort(unique(stacked_bar_data$method)))

stacked_bar_plot <- ggplot(stacked_bar_data, aes(y = method, x = percentage, fill = comparison_type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ metric, ncol = 3) + # Facets by metric
  labs(
    title = "RT Impact",
    x = "Percentage of Time Series (%)",
    y = "Method",
    fill = "Comparison to Original"
  ) +
  scale_fill_manual(values = c("RT Improved" = "grey40", 
                               "RT Tied" = "grey100",    
                               "RT Deteriorated" = "grey80")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 9),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_line(color = "grey80", linetype = "solid")
  )

print(stacked_bar_plot)

