# =========================================================
# 03-analyze-synthetic.R
# Exploratory analysis and figures over the synthetic
# detection results (official batches).
#
# Input : outputs/synthetic/detections/*.csv
# Output: console tables and figures (Q1-Q39)
# =========================================================

library(tidyverse)
library(ggplot2)
library(here)

# =========================================================
# FLAGS
# =========================================================

# Bootstrap CI is heavy; enable to produce CI tables.
run_bootstrap <- TRUE
n_boot <- 1000

# =========================================================
# PLOT SETTINGS
# =========================================================

wong_palette <- c(
  "Joinpoint" = "#D55E00",
  "MFEF"      = "#009E73",
  "MFEF+JP"   = "#0072B2",
  "None"      = "#999999"
)

theme_pub <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1, color = "black"),
      legend.title     = element_text(size = base_size),
      legend.text      = element_text(size = base_size - 1),
      legend.position  = "right",
      legend.key.size  = unit(0.4, "cm"),
      strip.background = element_blank(),
      strip.text       = element_text(size = base_size, face = "bold"),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(5, 10, 5, 5)
    )
}

# =========================================================
# LOAD
# =========================================================

detections_dir <- here::here("outputs", "synthetic", "detections")

results <- bind_rows(
  read_csv(
    file.path(detections_dir,
              "batch_mfef_joinpoint_hybrid_soft_hard_id_0_850_sw_6_thrgrid_0p90_0p95.csv"),
    show_col_types = FALSE
  ),
  read_csv(
    file.path(detections_dir,
              "batch_mfef_joinpoint_hybrid_soft_hard_id_851_1679_sw_6_thrgrid_0p90_0p95.csv"),
    show_col_types = FALSE
  )
)

# =========================================================
# Q1 — EXCLUDED SERIES
# =========================================================

failed_series <- c(243, 363, 837, 1113, 1191, 1231, 1509, 1641, 1653)

cat("\n=====================================\n")
cat("Q1 — EXCLUDED SERIES\n")
cat("=====================================\n")

results %>%
  filter(serie_id %in% failed_series) %>%
  distinct(serie_id, serie_id_nome) %>%
  arrange(serie_id) %>%
  print(n = Inf)

# =========================================================
# Q2 — FINAL DATASET
# =========================================================

results_clean <- results %>%
  filter(status == "ok", !serie_id %in% failed_series)

cat("\n=====================================\n")
cat("Q2 — FINAL DATASET\n")
cat("=====================================\n")
cat("Valid series:", n_distinct(results_clean$serie_id), "\n")

# =========================================================
# FEATURE ENGINEERING
# =========================================================

make_label <- function(method, log_transform, time_tolerance, threshold) {
  case_when(
    method == "Joinpoint" ~
      paste0("Joinpoint(", ifelse(log_transform, "T", "F"), ")"),
    method == "MFEF" ~
      paste0("MFEF(", time_tolerance, ",", threshold, ")"),
    method == "MFEF_JP_TRUE" ~
      paste0("MFEF+JP(T,", time_tolerance, ",", threshold, ")"),
    method == "MFEF_JP_FALSE" ~
      paste0("MFEF+JP(F,", time_tolerance, ",", threshold, ")"),
    TRUE ~ paste0(method, "_", time_tolerance, "_", threshold)
  )
}

results_clean <- results_clean %>%
  mutate(
    regime = case_when(
      str_detect(serie_id_nome, "^mean")     ~ "mean",
      str_detect(serie_id_nome, "^trend")    ~ "trend",
      str_detect(serie_id_nome, "^variance") ~ "variance",
      str_detect(serie_id_nome, "^null")     ~ "null",
      TRUE ~ "other"
    ),
    data_type = case_when(
      str_detect(serie_id_nome, "hospitalization") ~ "hospitalization",
      str_detect(serie_id_nome, "mortality")       ~ "mortality",
      TRUE ~ "other"
    ),
    cp_position = str_extract(serie_id_nome, "\\d+q"),
    magnitude = case_when(
      str_detect(serie_id_nome, "_high_high") ~ "high_high",
      str_detect(serie_id_nome, "_high_low")  ~ "high_low",
      str_detect(serie_id_nome, "_low_high")  ~ "low_high",
      str_detect(serie_id_nome, "_low_low")   ~ "low_low",
      TRUE ~ NA_character_
    ),
    ar1 = case_when(
      str_detect(serie_id_nome, "with_ar1") ~ "with_ar1",
      str_detect(serie_id_nome, "no_ar1")   ~ "no_ar1",
      TRUE ~ NA_character_
    ),
    method_config = case_when(
      method == "Joinpoint" ~ paste0(method, "_log_", log_transform),
      TRUE ~ paste0(method, "_tt_", time_tolerance, "_thr_", threshold)
    ),
    method_label = make_label(method, log_transform, time_tolerance, threshold),
    method_family = case_when(
      method == "Joinpoint"          ~ "Joinpoint",
      method == "MFEF"               ~ "MFEF",
      str_detect(method, "MFEF_JP")  ~ "MFEF+JP",
      TRUE ~ method
    ),
    method_family = factor(method_family, levels = c("Joinpoint", "MFEF", "MFEF+JP"))
  )

# =========================================================
# Q3 — SERIES WITH AND WITHOUT CP
# =========================================================

series_with_cp    <- results_clean %>% filter(n_true > 0)
series_without_cp <- results_clean %>% filter(n_true == 0)

cat("\n=====================================\n")
cat("Q3 — SPLIT BY n_true\n")
cat("=====================================\n")
cat("Series with CP:", n_distinct(series_with_cp$serie_id), "\n")
cat("Series without CP:", n_distinct(series_without_cp$serie_id), "\n")

# =========================================================
# HELPERS
# =========================================================

count_no_winner <- function(df, metric_col = "hard_F1") {
  df %>%
    group_by(serie_id) %>%
    summarise(
      all_zero_or_na = all(is.na(.data[[metric_col]]) | .data[[metric_col]] == 0),
      .groups = "drop"
    ) %>%
    filter(all_zero_or_na) %>%
    nrow()
}

find_winners <- function(df, metric_col = "hard_F1") {
  df %>%
    group_by(serie_id) %>%
    filter(any(!is.na(.data[[metric_col]]) & .data[[metric_col]] > 0)) %>%
    mutate(best_metric = max(.data[[metric_col]], na.rm = TRUE)) %>%
    filter(.data[[metric_col]] == best_metric) %>%
    ungroup()
}

find_winners_generic <- function(df, metric_col) {
  df %>%
    group_by(serie_id) %>%
    filter(any(!is.na(.data[[metric_col]]) & .data[[metric_col]] > 0)) %>%
    mutate(best_metric = max(.data[[metric_col]], na.rm = TRUE)) %>%
    filter(.data[[metric_col]] == best_metric) %>%
    ungroup()
}

# =========================================================
# Q4 — OVERALL WINNERS (hard_F1)
# =========================================================

n_series_overall    <- n_distinct(series_with_cp$serie_id)
n_no_winner_overall <- count_no_winner(series_with_cp, "hard_F1")

overall_winners <- find_winners(series_with_cp, "hard_F1")

overall_ranking <- overall_winners %>%
  count(method_label, method_family, name = "n_wins") %>%
  arrange(desc(n_wins))

cat("\n=====================================\n")
cat("Q4 — OVERALL WINNERS (hard F1)\n")
cat("=====================================\n")
cat("Series evaluated:", n_series_overall, "\n")
cat("Series without winner:", n_no_winner_overall,
    sprintf(" (%.1f%%)\n", 100 * n_no_winner_overall / n_series_overall))
print(overall_ranking, n = Inf)

# =========================================================
# Q4b — OVERALL WINNERS (soft_F1)
# =========================================================

n_no_winner_soft <- count_no_winner(series_with_cp, "soft_F1")
overall_winners_soft <- find_winners(series_with_cp, "soft_F1")

overall_ranking_soft <- overall_winners_soft %>%
  count(method_label, method_family, name = "n_wins") %>%
  arrange(desc(n_wins))

cat("\n=====================================\n")
cat("Q4b — OVERALL WINNERS (soft F1)\n")
cat("=====================================\n")
cat("Series without winner in soft:", n_no_winner_soft,
    sprintf(" (%.1f%%)\n", 100 * n_no_winner_soft / n_series_overall))
print(overall_ranking_soft, n = Inf)

# =========================================================
# Q4c — CONSOLIDATED HARD vs SOFT COMPARISON
# =========================================================

coverage <- series_with_cp %>%
  group_by(method_label, method_family) %>%
  summarise(
    n_scored_hard = sum(hard_F1 > 0, na.rm = TRUE),
    n_scored_soft = sum(soft_F1 > 0, na.rm = TRUE),
    .groups = "drop"
  )

winners_mark <- series_with_cp %>%
  group_by(serie_id) %>%
  mutate(
    is_winner_hard = hard_F1 == max(hard_F1, na.rm = TRUE) & hard_F1 > 0,
    is_winner_soft = soft_F1 == max(soft_F1, na.rm = TRUE) & soft_F1 > 0
  ) %>%
  ungroup()

quality_stats <- winners_mark %>%
  group_by(method_label) %>%
  summarise(
    mean_hard_when_won   = mean(hard_F1[is_winner_hard], na.rm = TRUE),
    median_hard_when_won = median(hard_F1[is_winner_hard], na.rm = TRUE),
    mean_soft_when_won   = mean(soft_F1[is_winner_soft], na.rm = TRUE),
    median_soft_when_won = median(soft_F1[is_winner_soft], na.rm = TRUE),
    mean_hard_scored   = mean(hard_F1[hard_F1 > 0], na.rm = TRUE),
    median_hard_scored = median(hard_F1[hard_F1 > 0], na.rm = TRUE),
    mean_soft_scored   = mean(soft_F1[soft_F1 > 0], na.rm = TRUE),
    median_soft_scored = median(soft_F1[soft_F1 > 0], na.rm = TRUE),
    .groups = "drop"
  )

overall_compare <- overall_ranking %>%
  select(method_label, method_family, n_wins_hard = n_wins) %>%
  full_join(
    overall_ranking_soft %>% select(method_label, n_wins_soft = n_wins),
    by = "method_label"
  ) %>%
  left_join(coverage, by = c("method_label", "method_family")) %>%
  left_join(quality_stats, by = "method_label") %>%
  mutate(
    n_wins_hard   = replace_na(n_wins_hard, 0),
    n_wins_soft   = replace_na(n_wins_soft, 0),
    win_rate_hard = round(100 * n_wins_hard / n_scored_hard, 1),
    win_rate_soft = round(100 * n_wins_soft / n_scored_soft, 1),
    rank_hard     = rank(-n_wins_hard, ties.method = "min"),
    rank_soft     = rank(-n_wins_soft, ties.method = "min"),
    rank_diff     = rank_soft - rank_hard
  ) %>%
  arrange(rank_hard)

cat("\n=====================================\n")
cat("Q4c — CONSOLIDATED HARD vs SOFT\n")
cat("=====================================\n")
print(overall_compare, n = Inf, width = Inf)

# =========================================================
# Q4c bis — 95% CI via bootstrap for conditional F1
# For each configuration: resample the series where it scored
# (F1 > 0) with replacement, take the median per replicate,
# and return point median plus 2.5%/97.5% bounds.
# =========================================================

set.seed(2025)
n_boot_config <- 1000

bootstrap_median_ci <- function(x, n_boot = 1000, alpha = 0.05) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(c(NA_real_, NA_real_, NA_real_))
  boots <- replicate(n_boot, median(sample(x, replace = TRUE)))
  c(
    point = median(x),
    lower = quantile(boots, alpha / 2, names = FALSE),
    upper = quantile(boots, 1 - alpha / 2, names = FALSE)
  )
}

ci_hard <- series_with_cp %>%
  filter(hard_F1 > 0, !is.na(hard_F1)) %>%
  group_by(method_label) %>%
  summarise(
    ci = list(bootstrap_median_ci(hard_F1, n_boot_config)),
    .groups = "drop"
  ) %>%
  mutate(
    cond_hard_median = map_dbl(ci, ~ .x[1]),
    cond_hard_low    = map_dbl(ci, ~ .x[2]),
    cond_hard_high   = map_dbl(ci, ~ .x[3])
  ) %>%
  select(-ci)

ci_soft <- series_with_cp %>%
  filter(soft_F1 > 0, !is.na(soft_F1)) %>%
  group_by(method_label) %>%
  summarise(
    ci = list(bootstrap_median_ci(soft_F1, n_boot_config)),
    .groups = "drop"
  ) %>%
  mutate(
    cond_soft_median = map_dbl(ci, ~ .x[1]),
    cond_soft_low    = map_dbl(ci, ~ .x[2]),
    cond_soft_high   = map_dbl(ci, ~ .x[3])
  ) %>%
  select(-ci)

overall_compare_ci <- overall_compare %>%
  left_join(ci_hard, by = "method_label") %>%
  left_join(ci_soft, by = "method_label") %>%
  mutate(
    cond_hard_label = sprintf("%.2f [%.2f, %.2f]",
                              cond_hard_median, cond_hard_low, cond_hard_high),
    cond_soft_label = sprintf("%.2f [%.2f, %.2f]",
                              cond_soft_median, cond_soft_low, cond_soft_high)
  )

cat("\n=====================================\n")
cat("Q4c bis — CI 95% for conditional F1 (bootstrap by series)\n")
cat("=====================================\n")

overall_compare_ci %>%
  select(method_label, method_family, rank_hard, rank_soft,
         n_scored_hard, n_wins_hard, cond_hard_label,
         n_scored_soft, n_wins_soft, cond_soft_label) %>%
  arrange(rank_hard) %>%
  print(n = Inf, width = Inf)

# =========================================================
# Q4d — COVERAGE (equal-scale axes)
# =========================================================

lim_coverage <- max(c(coverage$n_scored_hard, coverage$n_scored_soft), na.rm = TRUE) * 1.05

p_coverage <- coverage %>%
  ggplot(aes(n_scored_hard, n_scored_soft, color = method_family)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_point(size = 2.5) +
  geom_text(aes(label = method_label), size = 2.4, vjust = -0.9, show.legend = FALSE) +
  scale_color_manual(values = wong_palette) +
  coord_fixed(xlim = c(0, lim_coverage), ylim = c(0, lim_coverage)) +
  labs(x = "Number of series with hard F1 > 0",
       y = "Number of series with soft F1 > 0",
       color = "Family") +
  theme_pub()
print(p_coverage)

# =========================================================
# Q4d bis — WINS HARD vs SOFT
# =========================================================

wins_plot_data <- overall_compare %>%
  select(method_label, method_family, n_wins_hard, n_wins_soft)

lim_wins <- max(c(wins_plot_data$n_wins_hard, wins_plot_data$n_wins_soft), na.rm = TRUE) * 1.05

p_wins <- wins_plot_data %>%
  ggplot(aes(n_wins_hard, n_wins_soft, color = method_family)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text(aes(label = method_label), size = 2.4, vjust = -0.9, show.legend = FALSE) +
  scale_color_manual(values = wong_palette) +
  coord_fixed(xlim = c(0, lim_wins), ylim = c(0, lim_wins)) +
  labs(x = "Number of wins under hard F1",
       y = "Number of wins under soft F1",
       color = "Family") +
  theme_pub()
print(p_wins)

# =========================================================
# Q4d ter — PRECISION AND RECALL WINS
# =========================================================

precision_winners_hard <- find_winners_generic(series_with_cp, "hard_precision") %>%
  count(method_label, method_family, name = "n_wins_precision")

recall_winners_hard <- find_winners_generic(series_with_cp, "hard_recall") %>%
  count(method_label, method_family, name = "n_wins_recall")

pr_wins_hard <- overall_ranking %>%
  select(method_label, method_family, n_wins_f1 = n_wins) %>%
  full_join(precision_winners_hard %>% select(-method_family), by = "method_label") %>%
  full_join(recall_winners_hard %>% select(-method_family), by = "method_label") %>%
  mutate(
    n_wins_f1        = replace_na(n_wins_f1, 0),
    n_wins_precision = replace_na(n_wins_precision, 0),
    n_wins_recall    = replace_na(n_wins_recall, 0)
  ) %>%
  arrange(desc(n_wins_f1))

cat("\nQ4d ter — PRECISION AND RECALL WINS (hard)\n")
print(pr_wins_hard, n = Inf)

# =========================================================
# Q4e — PAIRWISE COMPARISON
# =========================================================

pairwise_comparison <- function(df, config_a, config_b, metric = "hard_F1", higher_is_better = TRUE) {
  if (higher_is_better) {
    df_a <- df %>% filter(method_label == config_a, .data[[metric]] > 0)
    df_b <- df %>% filter(method_label == config_b, .data[[metric]] > 0)
  } else {
    df_a <- df %>% filter(method_label == config_a, !is.na(.data[[metric]]))
    df_b <- df %>% filter(method_label == config_b, !is.na(.data[[metric]]))
  }
  common_series <- intersect(df_a$serie_id, df_b$serie_id)
  mean_a <- mean(df_a[[metric]][df_a$serie_id %in% common_series])
  mean_b <- mean(df_b[[metric]][df_b$serie_id %in% common_series])
  diff <- if (higher_is_better) mean_a - mean_b else mean_b - mean_a
  tibble(config_a = config_a, config_b = config_b, metric = metric,
         n_common = length(common_series), mean_a = mean_a, mean_b = mean_b, diff = diff)
}

configs <- unique(series_with_cp$method_label)

pairwise_hard <- expand_grid(config_a = configs, config_b = configs) %>%
  filter(config_a != config_b) %>%
  pmap_dfr(function(config_a, config_b) {
    pairwise_comparison(series_with_cp, config_a, config_b, "hard_F1", TRUE)
  })

pairwise_soft <- expand_grid(config_a = configs, config_b = configs) %>%
  filter(config_a != config_b) %>%
  pmap_dfr(function(config_a, config_b) {
    pairwise_comparison(series_with_cp, config_a, config_b, "soft_F1", TRUE)
  })

p_pair_hard <- pairwise_hard %>%
  ggplot(aes(config_b, config_a, fill = diff)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.2f", diff)), size = 3.2) +
  scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2",
                       midpoint = 0, limits = c(-0.6, 0.6), oob = scales::squish,
                       name = "Mean F1 difference") +
  labs(x = NULL, y = NULL) +
  theme_pub() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
    axis.text.y       = element_text(size = 11),
    legend.position   = "top",
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    legend.key.width  = unit(2.5, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.margin     = margin(t = 0, b = -5, l = 0, r = 0),
    panel.grid        = element_blank()
  ) +
  guides(fill = guide_colorbar(title.position = "left", title.vjust = 0.9, title.hjust = 0.5))
print(p_pair_hard)

p_pair_soft <- pairwise_soft %>%
  ggplot(aes(config_b, config_a, fill = diff)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.2f", diff)), size = 3.2) +
  scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2",
                       midpoint = 0, limits = c(-0.6, 0.6), oob = scales::squish,
                       name = "Mean F1 difference") +
  labs(x = NULL, y = NULL) +
  theme_pub() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
    axis.text.y       = element_text(size = 11),
    legend.position   = "top",
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    legend.key.width  = unit(2.5, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.margin     = margin(t = 0, b = -5, l = 0, r = 0),
    panel.grid        = element_blank()
  ) +
  guides(fill = guide_colorbar(title.position = "left", title.vjust = 0.9, title.hjust = 0.5))
print(p_pair_soft)

# =========================================================
# Q4f — PAIRWISE RANKING
# =========================================================

build_pairwise_ranking <- function(pairwise_df) {
  pairwise_df %>%
    mutate(a_wins = diff > 0) %>%
    group_by(config_a) %>%
    summarise(
      n_matchups     = n(),
      n_wins         = sum(a_wins, na.rm = TRUE),
      win_rate       = round(100 * n_wins / n_matchups, 1),
      mean_advantage = round(mean(diff, na.rm = TRUE), 3),
      .groups = "drop"
    ) %>%
    arrange(desc(win_rate))
}

pairwise_ranking_hard <- build_pairwise_ranking(pairwise_hard)
pairwise_ranking_soft <- build_pairwise_ranking(pairwise_soft)

cat("\nQ4f — PAIRWISE RANKING (hard F1)\n")
print(pairwise_ranking_hard, n = Inf)
cat("\nQ4f — PAIRWISE RANKING (soft F1)\n")
print(pairwise_ranking_soft, n = Inf)

# =========================================================
# Q4g — WIN RATE vs MEAN ADVANTAGE
# =========================================================

pair_intensity_hard <- pairwise_ranking_hard %>%
  rename(method_label = config_a) %>%
  left_join(series_with_cp %>% distinct(method_label, method_family), by = "method_label")

p_pair_intensity_hard <- pair_intensity_hard %>%
  ggplot(aes(win_rate, mean_advantage, color = method_family)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text(aes(label = method_label), size = 2.4, vjust = -0.9, show.legend = FALSE) +
  scale_color_manual(values = wong_palette) +
  labs(x = "Pairwise win rate (%)", y = "Mean advantage in F1", color = "Family") +
  theme_pub()
print(p_pair_intensity_hard)

# =========================================================
# Q4h — FAMILY BY REGIME (hard vs soft, stacked bars)
# Self-contained; does not depend on later regime blocks.
# =========================================================

# Hard
regime_no_winner_hard_q4h <- series_with_cp %>%
  group_by(regime, serie_id) %>%
  summarise(all_zero_or_na = all(is.na(hard_F1) | hard_F1 == 0), .groups = "drop") %>%
  filter(all_zero_or_na) %>%
  count(regime, name = "n_no_winner")

family_by_regime_hard_q4h <- series_with_cp %>%
  group_by(regime, serie_id) %>%
  filter(any(!is.na(hard_F1) & hard_F1 > 0)) %>%
  mutate(best_f1 = max(hard_F1, na.rm = TRUE)) %>%
  filter(hard_F1 == best_f1) %>%
  ungroup() %>%
  count(regime, method_family, name = "n_wins") %>%
  bind_rows(regime_no_winner_hard_q4h %>%
              rename(n_wins = n_no_winner) %>%
              mutate(method_family = "None")) %>%
  group_by(regime) %>%
  mutate(pct = round(100 * n_wins / sum(n_wins), 2)) %>%
  ungroup() %>%
  mutate(metric = "Hard F1")

# Soft
regime_no_winner_soft_q4h <- series_with_cp %>%
  group_by(regime, serie_id) %>%
  summarise(all_zero_or_na = all(is.na(soft_F1) | soft_F1 == 0), .groups = "drop") %>%
  filter(all_zero_or_na) %>%
  count(regime, name = "n_no_winner")

family_by_regime_soft_q4h <- series_with_cp %>%
  group_by(regime, serie_id) %>%
  filter(any(!is.na(soft_F1) & soft_F1 > 0)) %>%
  mutate(best_f1 = max(soft_F1, na.rm = TRUE)) %>%
  filter(soft_F1 == best_f1) %>%
  ungroup() %>%
  count(regime, method_family, name = "n_wins") %>%
  bind_rows(regime_no_winner_soft_q4h %>%
              rename(n_wins = n_no_winner) %>%
              mutate(method_family = "None")) %>%
  group_by(regime) %>%
  mutate(pct = round(100 * n_wins / sum(n_wins), 2)) %>%
  ungroup() %>%
  mutate(metric = "Soft F1")

family_by_regime_both <- bind_rows(family_by_regime_hard_q4h, family_by_regime_soft_q4h) %>%
  mutate(method_family = factor(method_family,
                                levels = c("Joinpoint", "MFEF", "MFEF+JP", "None")))

p_family_regime_both <- family_by_regime_both %>%
  ggplot(aes(regime, pct, fill = method_family)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "black") +
  facet_wrap(~ metric) +
  scale_fill_manual(values = wong_palette) +
  labs(x = "Regime", y = "Percentage of series (%)", fill = "Family") +
  theme_pub(base_size = 11) +
  theme(
    strip.text   = element_text(size = 12, face = "bold"),
    axis.text    = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9)
  )
print(p_family_regime_both)

# =========================================================
# Q6 — TIES
# =========================================================

ties <- overall_winners %>% count(serie_id, name = "n_winners") %>% filter(n_winners > 1)

cat("\nQ6 — TIES\n")
cat("Series with ties:", nrow(ties), "out of", n_distinct(overall_winners$serie_id), "\n")

# =========================================================
# Q7 — INTRACTABLE SERIES
# =========================================================

no_winner_series <- series_with_cp %>%
  group_by(serie_id) %>%
  summarise(all_zero_or_na = all(is.na(hard_F1) | hard_F1 == 0), .groups = "drop") %>%
  filter(all_zero_or_na) %>%
  pull(serie_id)

no_winner_features <- series_with_cp %>%
  filter(serie_id %in% no_winner_series) %>%
  distinct(serie_id, regime, data_type, cp_position, magnitude, ar1)

cat("\nQ7 — INTRACTABLE SERIES\n")
no_winner_features %>% count(regime, magnitude, ar1) %>% arrange(desc(n)) %>% print(n = Inf)

# =========================================================
# Q9-Q12 — BY REGIME
# =========================================================

regime_no_winner <- series_with_cp %>%
  group_by(regime, serie_id) %>%
  summarise(all_zero_or_na = all(is.na(hard_F1) | hard_F1 == 0), .groups = "drop") %>%
  filter(all_zero_or_na) %>%
  count(regime, name = "n_no_winner")

winners_by_regime <- series_with_cp %>%
  group_by(regime, serie_id) %>%
  filter(any(!is.na(hard_F1) & hard_F1 > 0)) %>%
  mutate(best_f1 = max(hard_F1, na.rm = TRUE)) %>%
  filter(hard_F1 == best_f1) %>%
  ungroup() %>%
  count(regime, method_label, method_family, name = "n_wins") %>%
  arrange(regime, desc(n_wins))

cat("\nQ9-Q12 — BY REGIME\n")
print(winners_by_regime, n = Inf)

# =========================================================
# Q32 — PAIRWISE DOMINANCE BY REGIME
# =========================================================

pairwise_by_regime <- function(regime_name, metric_col, higher_is_better = TRUE, df_source = series_with_cp) {
  df_regime <- df_source %>% filter(regime == regime_name)
  configs_r <- unique(df_regime$method_label)
  expand_grid(config_a = configs_r, config_b = configs_r) %>%
    filter(config_a != config_b) %>%
    pmap_dfr(function(config_a, config_b) {
      pairwise_comparison(df_regime, config_a, config_b, metric_col, higher_is_better)
    }) %>%
    mutate(a_wins = diff > 0) %>%
    group_by(config_a) %>%
    summarise(
      regime = regime_name,
      n_matchups     = n(),
      n_wins         = sum(a_wins, na.rm = TRUE),
      win_rate       = round(100 * n_wins / n_matchups, 1),
      mean_advantage = round(mean(diff, na.rm = TRUE), 3),
      .groups = "drop"
    )
}

regimes_cp <- c("mean", "trend", "variance")
pairwise_regime_hard <- map_dfr(regimes_cp, ~ pairwise_by_regime(.x, "hard_F1", TRUE, series_with_cp))
pairwise_regime_soft <- map_dfr(regimes_cp, ~ pairwise_by_regime(.x, "soft_F1", TRUE, series_with_cp))

cat("\nQ32 — PAIRWISE DOMINANCE BY REGIME (hard)\n")
print(pairwise_regime_hard, n = Inf)

# =========================================================
# Q36 — FAMILY-LEVEL SUMMARY WITH BOOTSTRAP CI (95%)
# =========================================================

if (run_bootstrap) {
  
  cat("\n=====================================\n")
  cat("Q36 — FAMILY-LEVEL SUMMARY WITH 95% CI\n")
  cat(sprintf("Bootstrap replicates: %d\n", n_boot))
  cat("=====================================\n")
  
  set.seed(2025)
  
  bootstrap_ci <- function(x, n_boot = 1000, stat = median, alpha = 0.05) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(c(NA, NA, NA))
    boots <- replicate(n_boot, stat(sample(x, replace = TRUE)))
    c(point = stat(x),
      lower = quantile(boots, alpha / 2, names = FALSE),
      upper = quantile(boots, 1 - alpha / 2, names = FALSE))
  }
  
  coverage_family <- series_with_cp %>%
    group_by(method_family, method_label) %>%
    summarise(
      n_scored_hard = sum(hard_F1 > 0, na.rm = TRUE),
      n_scored_soft = sum(soft_F1 > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Conditional F1 (only when scored > 0)
  cond_f1_family <- series_with_cp %>%
    group_by(method_family) %>%
    summarise(
      hard_ci = list(bootstrap_ci(hard_F1[hard_F1 > 0], n_boot, median)),
      soft_ci = list(bootstrap_ci(soft_F1[soft_F1 > 0], n_boot, median)),
      cov_hard_ci = list(bootstrap_ci(coverage_family$n_scored_hard[coverage_family$method_family == cur_group()$method_family], n_boot, median)),
      cov_soft_ci = list(bootstrap_ci(coverage_family$n_scored_soft[coverage_family$method_family == cur_group()$method_family], n_boot, median)),
      .groups = "drop"
    ) %>%
    mutate(
      cond_hard_median = map_dbl(hard_ci, ~ .x[1]),
      cond_hard_low    = map_dbl(hard_ci, ~ .x[2]),
      cond_hard_high   = map_dbl(hard_ci, ~ .x[3]),
      cond_soft_median = map_dbl(soft_ci, ~ .x[1]),
      cond_soft_low    = map_dbl(soft_ci, ~ .x[2]),
      cond_soft_high   = map_dbl(soft_ci, ~ .x[3]),
      cov_hard_median  = map_dbl(cov_hard_ci, ~ .x[1]),
      cov_hard_low     = map_dbl(cov_hard_ci, ~ .x[2]),
      cov_hard_high    = map_dbl(cov_hard_ci, ~ .x[3]),
      cov_soft_median  = map_dbl(cov_soft_ci, ~ .x[1]),
      cov_soft_low     = map_dbl(cov_soft_ci, ~ .x[2]),
      cov_soft_high    = map_dbl(cov_soft_ci, ~ .x[3])
    ) %>%
    select(-hard_ci, -soft_ci, -cov_hard_ci, -cov_soft_ci)
  
  cat("\n--- Family-level summary (median [95% CI]) ---\n")
  print(cond_f1_family)
  
  pairwise_by_family <- pairwise_ranking_hard %>%
    rename(method_label = config_a) %>%
    left_join(series_with_cp %>% distinct(method_label, method_family), by = "method_label") %>%
    group_by(method_family) %>%
    summarise(
      wr_ci = list(bootstrap_ci(win_rate, n_boot, median)),
      .groups = "drop"
    ) %>%
    mutate(
      pw_median = map_dbl(wr_ci, ~ .x[1]),
      pw_low    = map_dbl(wr_ci, ~ .x[2]),
      pw_high   = map_dbl(wr_ci, ~ .x[3])
    ) %>%
    select(-wr_ci)
  
  cat("\n--- Pairwise win rate by family (median [95% CI]) ---\n")
  print(pairwise_by_family)
  
} else {
  cat("\nQ36 — SKIPPED (run_bootstrap = FALSE)\n")
}

# =========================================================
# Q37 — KRUSKAL-WALLIS AND PAIRWISE WILCOXON BY REGIME
# Global Kruskal-Wallis per regime followed by pairwise
# Wilcoxon (Bonferroni). Also produces boxplots.
# =========================================================

cat("\n=====================================\n")
cat("Q37 — KRUSKAL-WALLIS AND PAIRWISE WILCOXON\n")
cat("=====================================\n")

run_tests_by_regime <- function(df, metric_col, regimes_to_test = c("mean", "trend", "variance")) {
  results_list <- list()
  for (r in regimes_to_test) {
    df_r <- df %>%
      filter(regime == r, !is.na(.data[[metric_col]]))
    if (nrow(df_r) == 0) next
    kw <- kruskal.test(as.formula(paste0(metric_col, " ~ method_family")), data = df_r)
    pw <- tryCatch({
      pairwise.wilcox.test(df_r[[metric_col]], df_r$method_family,
                           p.adjust.method = "bonferroni",
                           exact = FALSE)
    }, error = function(e) NULL)
    results_list[[r]] <- list(regime = r,
                              metric = metric_col,
                              kruskal_p = kw$p.value,
                              kruskal_chi = kw$statistic,
                              pairwise = pw)
  }
  results_list
}

tests_hard <- run_tests_by_regime(series_with_cp, "hard_F1")
tests_soft <- run_tests_by_regime(series_with_cp, "soft_F1")

cat("\n--- Kruskal-Wallis results (hard F1) ---\n")
for (r in names(tests_hard)) {
  cat(sprintf("Regime: %s | chi2 = %.3f | p = %.3g\n",
              r, tests_hard[[r]]$kruskal_chi, tests_hard[[r]]$kruskal_p))
  cat("Pairwise Wilcoxon (Bonferroni-adjusted p-values):\n")
  print(tests_hard[[r]]$pairwise)
  cat("\n")
}

cat("\n--- Kruskal-Wallis results (soft F1) ---\n")
for (r in names(tests_soft)) {
  cat(sprintf("Regime: %s | chi2 = %.3f | p = %.3g\n",
              r, tests_soft[[r]]$kruskal_chi, tests_soft[[r]]$kruskal_p))
  cat("Pairwise Wilcoxon (Bonferroni-adjusted p-values):\n")
  print(tests_soft[[r]]$pairwise)
  cat("\n")
}

boxplot_data <- series_with_cp %>%
  filter(regime %in% c("mean", "trend", "variance")) %>%
  select(regime, method_family, hard_F1, soft_F1) %>%
  pivot_longer(cols = c(hard_F1, soft_F1), names_to = "metric", values_to = "F1") %>%
  mutate(metric = recode(metric, hard_F1 = "Hard F1", soft_F1 = "Soft F1"))

# Q37 (final) — Boxplot with n and coverage (%) below

n_series_regime <- series_with_cp %>%
  filter(regime %in% c("mean", "trend", "variance")) %>%
  distinct(regime, serie_id) %>%
  count(regime, name = "n_series_total")

# Number of configurations per family, derived from the data
configs_per_family <- series_with_cp %>%
  distinct(method_family, method_label) %>%
  count(method_family, name = "n_configs")

boxplot_counts <- boxplot_data %>%
  filter(!is.na(F1), F1 > 0) %>%
  group_by(regime, method_family, metric) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  left_join(n_series_regime, by = "regime") %>%
  left_join(configs_per_family, by = "method_family") %>%
  mutate(
    n_max_theoretical = n_series_total * n_configs,
    pct_coverage = round(100 * n_obs / n_max_theoretical, 1),
    label = paste0("n=", n_obs, "\n(", pct_coverage, "%)")
  )

cat("\n--- Boxplot counts (n, max theoretical, coverage %) ---\n")
print(boxplot_counts, n = Inf)

p_boxplot_family_regime <- boxplot_data %>%
  filter(!is.na(F1), F1 > 0) %>%
  ggplot(aes(method_family, F1, fill = method_family)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  geom_text(data = boxplot_counts,
            aes(x = method_family, y = -0.10, label = label),
            size = 3.1, inherit.aes = FALSE,
            lineheight = 0.85) +
  facet_grid(metric ~ regime) +
  scale_fill_manual(values = wong_palette) +
  scale_y_continuous(limits = c(-0.18, 1.05)) +
  labs(x = "Family",
       y = "F1 (conditional on non-null detection)",
       fill = "Family") +
  theme_pub() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_boxplot_family_regime)

# =========================================================
# Q38 — FALSE POSITIVE RATE ON NULL SERIES
# FPR = proportion of null series where the configuration
# detected at least one change-point.
# =========================================================

cat("\n=====================================\n")
cat("Q38 — FALSE POSITIVE RATE (null series only)\n")
cat("=====================================\n")

fpr_null <- series_without_cp %>%
  group_by(method_label, method_family) %>%
  summarise(
    n_series = n_distinct(serie_id),
    n_with_detection = sum(n_detected >= 1, na.rm = TRUE),
    fpr = round(100 * n_with_detection / n_series, 1),
    mean_n_detected = round(mean(n_detected, na.rm = TRUE), 2),
    median_n_detected = median(n_detected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fpr)

print(fpr_null, n = Inf)

if (run_bootstrap) {
  
  set.seed(2025)
  
  bootstrap_ci_prop <- function(x, n_boot = 1000, alpha = 0.05) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(c(NA, NA, NA))
    boots <- replicate(n_boot, mean(sample(x, replace = TRUE)))
    c(point = mean(x),
      lower = quantile(boots, alpha / 2, names = FALSE),
      upper = quantile(boots, 1 - alpha / 2, names = FALSE))
  }
  
  fpr_family <- series_without_cp %>%
    mutate(has_detection = as.integer(n_detected >= 1)) %>%
    group_by(method_family) %>%
    summarise(
      fpr_ci = list(bootstrap_ci_prop(has_detection, n_boot)),
      .groups = "drop"
    ) %>%
    mutate(
      fpr_median = round(100 * map_dbl(fpr_ci, ~ .x[1]), 1),
      fpr_low    = round(100 * map_dbl(fpr_ci, ~ .x[2]), 1),
      fpr_high   = round(100 * map_dbl(fpr_ci, ~ .x[3]), 1)
    ) %>%
    select(-fpr_ci)
  
  cat("\n--- FPR by family with 95% CI ---\n")
  print(fpr_family)
  
} else {
  fpr_family <- series_without_cp %>%
    mutate(has_detection = as.integer(n_detected >= 1)) %>%
    group_by(method_family) %>%
    summarise(fpr = round(100 * mean(has_detection, na.rm = TRUE), 1), .groups = "drop")
  cat("\n--- FPR by family (no CI, bootstrap disabled) ---\n")
  print(fpr_family)
}

# =========================================================
# Q39 — FRIEDMAN-NEMENYI ANALYSIS
# Main omnibus analysis (Demsar 2006). Friedman + Nemenyi
# per subset with critical difference diagrams.
# =========================================================

if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
  stop("Package 'PMCMRplus' is required. Install it (or run renv::restore()).",
       call. = FALSE)
}
library(PMCMRplus)

cat("\n=====================================\n")
cat("Q39 — FRIEDMAN-NEMENYI ANALYSIS\n")
cat("=====================================\n")

run_friedman_nemenyi <- function(df, metric_col, subset_name,
                                 fill_missing_with = 0) {
  mat_df <- df %>%
    select(serie_id, method_label, all_of(metric_col)) %>%
    pivot_wider(names_from = method_label, values_from = all_of(metric_col))
  
  mat <- mat_df %>% select(-serie_id) %>% as.matrix()
  mat[is.na(mat)] <- fill_missing_with
  
  # Rename columns to safe identifiers for the test
  original_names <- colnames(mat)
  safe_names <- paste0("M", seq_along(original_names))
  colnames(mat) <- safe_names
  name_map <- setNames(original_names, safe_names)
  
  n_series <- nrow(mat)
  n_methods <- ncol(mat)
  
  cat(sprintf("\n[%s] n series = %d | n methods = %d\n",
              subset_name, n_series, n_methods))
  
  row_var <- apply(mat, 1, function(row) length(unique(row)) > 1)
  if (sum(row_var) < 3) {
    cat("  Insufficient row variation for Friedman test.\n")
    return(NULL)
  }
  
  mat_filtered <- mat[row_var, ]
  
  fr <- friedman.test(mat_filtered)
  cat(sprintf("  Friedman test: chi-square = %.3f | df = %d | p = %.3g\n",
              fr$statistic, fr$parameter, fr$p.value))
  
  ranks <- t(apply(mat_filtered, 1, function(row) rank(-row, ties.method = "average")))
  mean_ranks <- colMeans(ranks)
  names(mean_ranks) <- name_map[names(mean_ranks)]
  mean_ranks_sorted <- sort(mean_ranks)
  
  cat("  Mean ranks (lower is better):\n")
  print(round(mean_ranks_sorted, 2))
  
  nem <- NULL
  if (fr$p.value < 0.05) {
    nem <- tryCatch(
      frdAllPairsNemenyiTest(mat_filtered),
      error = function(e) {
        cat(sprintf("  Nemenyi via PMCMRplus failed: %s\n", e$message))
        NULL
      }
    )
    
    if (!is.null(nem)) {
      pmat <- nem$p.value
      rownames(pmat) <- name_map[rownames(pmat)]
      colnames(pmat) <- name_map[colnames(pmat)]
      nem$p.value <- pmat
      cat("  Nemenyi post-hoc computed successfully.\n")
    }
  } else {
    cat("  Friedman not significant; skipping Nemenyi.\n")
  }
  
  list(
    subset = subset_name,
    friedman = fr,
    mean_ranks = mean_ranks_sorted,
    nemenyi = nem,
    n_series_used = nrow(mat_filtered),
    n_methods = n_methods
  )
}

# Analyses
result_overall_hard <- run_friedman_nemenyi(series_with_cp, "hard_F1",
                                            "Overall - Hard F1")
result_overall_soft <- run_friedman_nemenyi(series_with_cp, "soft_F1",
                                            "Overall - Soft F1")

regimes <- c("mean", "trend", "variance")
results_by_regime <- list()

for (r in regimes) {
  df_r <- series_with_cp %>% filter(regime == r)
  results_by_regime[[paste0(r, "_hard")]] <- run_friedman_nemenyi(
    df_r, "hard_F1", sprintf("Regime %s - Hard F1", r))
  results_by_regime[[paste0(r, "_soft")]] <- run_friedman_nemenyi(
    df_r, "soft_F1", sprintf("Regime %s - Soft F1", r))
}

# Statistical equivalence groups within CD
compute_cd_groups <- function(mean_ranks, cd_value) {
  n <- length(mean_ranks)
  sorted_ranks <- sort(mean_ranks)
  groups <- integer(n)
  names(groups) <- names(sorted_ranks)
  
  current_group <- 1
  groups[1] <- current_group
  group_start_rank <- sorted_ranks[1]
  
  for (i in 2:n) {
    if (sorted_ranks[i] - group_start_rank <= cd_value) {
      groups[i] <- current_group
    } else {
      current_group <- current_group + 1
      groups[i] <- current_group
      group_start_rank <- sorted_ranks[i]
    }
  }
  
  groups
}

# Critical difference diagram
plot_cd_diagram <- function(result_obj, title_str, df_source = series_with_cp) {
  if (is.null(result_obj)) return(NULL)
  
  n_methods_cd <- length(result_obj$mean_ranks)
  n_series_cd  <- result_obj$n_series_used
  q_alpha      <- qtukey(0.95, n_methods_cd, Inf) / sqrt(2)
  cd_value     <- q_alpha * sqrt(n_methods_cd * (n_methods_cd + 1) / (6 * n_series_cd))
  
  cat(sprintf("  Critical difference (%s): %.3f\n", title_str, cd_value))
  
  ranks_df <- tibble(
    method_label = names(result_obj$mean_ranks),
    mean_rank    = as.numeric(result_obj$mean_ranks)
  ) %>%
    arrange(mean_rank) %>%
    left_join(df_source %>% distinct(method_label, method_family),
              by = "method_label")
  
  groups <- compute_cd_groups(result_obj$mean_ranks, cd_value)
  ranks_df <- ranks_df %>%
    mutate(equiv_group = groups[method_label])
  
  cat("  Statistical equivalence groups (within CD):\n")
  for (g in unique(ranks_df$equiv_group)) {
    members <- ranks_df$method_label[ranks_df$equiv_group == g]
    cat(sprintf("    Group %d: %s\n", g, paste(members, collapse = ", ")))
  }
  
  p <- ranks_df %>%
    ggplot(aes(x = mean_rank, y = reorder(method_label, -mean_rank),
               color = method_family)) +
    geom_segment(aes(x = mean_rank - cd_value / 2,
                     xend = mean_rank + cd_value / 2,
                     yend = reorder(method_label, -mean_rank)),
                 linewidth = 1.5, alpha = 0.4) +
    geom_point(size = 3) +
    scale_color_manual(values = wong_palette) +
    labs(
      title = sprintf("Critical difference diagram — %s", title_str),
      subtitle = sprintf("CD = %.2f (alpha = 0.05, N = %d series, k = %d methods)",
                         cd_value, n_series_cd, n_methods_cd),
      x = "Mean rank (lower is better)",
      y = NULL,
      color = "Family"
    ) +
    theme_pub(base_size = 10)
  
  return(list(plot = p, cd_value = cd_value, groups = ranks_df))
}

cat("\n=====================================\n")
cat("CRITICAL DIFFERENCE DIAGRAMS\n")
cat("=====================================\n")

cd_results <- list()

cat("\n--- Overall Hard F1 ---\n")
cd_results$overall_hard <- plot_cd_diagram(result_overall_hard, "Overall Hard F1")
if (!is.null(cd_results$overall_hard)) print(cd_results$overall_hard$plot)

cat("\n--- Overall Soft F1 ---\n")
cd_results$overall_soft <- plot_cd_diagram(result_overall_soft, "Overall Soft F1")
if (!is.null(cd_results$overall_soft)) print(cd_results$overall_soft$plot)

regime_labels <- c("mean" = "Mean-shift", "trend" = "Trend-shift", "variance" = "Variance-shift")

for (r in regimes) {
  for (m in c("hard", "soft")) {
    key <- paste0(r, "_", m)
    title_str <- sprintf("%s - %s F1", regime_labels[r], str_to_title(m))
    
    cat(sprintf("\n--- %s ---\n", title_str))
    cd_results[[key]] <- plot_cd_diagram(results_by_regime[[key]], title_str)
    if (!is.null(cd_results[[key]])) print(cd_results[[key]]$plot)
  }
}

cat("\n=====================================\n")
cat("END OF Q39\n")
cat("=====================================\n")

cat("\n=====================================\n")
cat("END OF ANALYSIS\n")
cat("=====================================\n")