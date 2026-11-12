# ----------------------------------------------------------- #
# -------------------- CARREGA PACOTES ---------------------- #
# ----------------------------------------------------------- #
library(dplyr)
library(tidyr)
library(arrow)
library(ggplot2)


# ----------------------------------------------------------- #
# ------------------ FUNCOES AUXILIARES --------------------- #
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Carrega dados
# ----------------------------------------------------------- #
load_data <- function(path) {
  message("Loading data...")
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("rdata", "rda", "rds")) {
    env <- new.env()
    load(path, envir = env)
    df <- env[[ls(env)[1]]]
  } else if (ext == "parquet") {
    df <- arrow::read_parquet(path) %>% as.data.frame()
  } else {
    stop(sprintf("Unsupported file format: %s", ext))
  }
  df %>% mutate(model = case_when(grepl("^fs", model) ~ paste(model, embedding, sep = "-"),
                                  model == "rew" & wisard == "crw" ~ "crew",
                                  TRUE ~ model ), ts = paste(dataset, ts, sep = "_"))
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Teste de Wilcoxon dos modelos baseados em WiSARD
# ----------------------------------------------------------- #
wilcoxon_fs_vs_crew <- function(data, metric) {
  if (!("crew" %in% data$model)) return(NULL)
  fs_models <- sort(unique(data$model[grepl("fs", data$model, fixed = TRUE)]))
  if (length(fs_models) == 0) return(NULL)
  results <- lapply(fs_models, function(fs_model) {
    paired <- data %>%
      filter(model %in% c(fs_model, "crew")) %>%
      group_by(dataset, seed, model) %>%
      summarise(value = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = model, values_from = value) %>%
      tidyr::drop_na(dplyr::all_of(c(fs_model, "crew")))
    n <- nrow(paired)
    if (n == 0) return(NULL)
    wt <- stats::wilcox.test(paired[[fs_model]], paired[["crew"]], paired = TRUE,  alternative = "two.sided", exact = FALSE)
    total_rank <- n * (n + 1) / 2
    effect_size <- if (total_rank > 0) 1 - (2 * unname(wt$statistic)) / total_rank else NA_real_
    tibble::tibble(
      dataset = c("n", "W", "p", "effect_size"),
      model = fs_model,
      value = c(as.character(n), sprintf("%.1f", unname(wt$statistic)), sprintf("%.4g", wt$p.value), sprintf("%.3f", effect_size))
    )
  })
  dplyr::bind_rows(results)
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Desempenho dos metodos (SMAPE, Tempo etc.) por dataset
# ----------------------------------------------------------- #
summary_dataset <- function(data, metric, steps = NULL) {
  message('Writing summary files')
  if (!is.null(steps)) data <- data %>% filter(strategy == steps)
  dataset_out <- data %>%
    group_by(dataset, seed, model) %>%
    summarise(seed_mean = mean(.data[[metric]], na.rm = TRUE), .groups = 'drop') %>%
    group_by(dataset, model) %>%
    summarise(avg = mean(seed_mean, na.rm = TRUE),
              std = coalesce(sd(seed_mean, na.rm = TRUE), 0),
              .groups = 'drop') %>%
    group_by(dataset) %>%
    mutate(winner = (avg - std) <= min(avg + std),
           value = sprintf('%.4f +- %.4f%s', avg, std, ifelse(winner, '*', ''))) %>%
    ungroup() %>%
    select(dataset, model, value) %>%
    pivot_wider(names_from = model, values_from = value)
  out <- dataset_out
  mean_out <- data %>%
    group_by(seed, model) %>%
    summarise(seed_mean = mean(.data[[metric]], na.rm = TRUE), .groups = 'drop') %>%
    group_by(model) %>%
    summarise(avg = mean(seed_mean, na.rm = TRUE),
              std = coalesce(sd(seed_mean, na.rm = TRUE), 0),
              .groups = 'drop') %>%
    mutate(dataset = "Mean",
           value = sprintf('%.4f +- %.4f', avg, std)) %>%
    select(dataset, model, value) %>%
    pivot_wider(names_from = model, values_from = value)
  out <- bind_rows(out, mean_out)
  if (identical(metric, "smape") && "crew" %in% names(out)) {
    test_rows <- wilcoxon_fs_vs_crew(data, metric)
    if (!is.null(test_rows) && nrow(test_rows) > 0) {
      out <- out %>% mutate(across(everything(), as.character))
      test_rows_wide <- test_rows %>%
        mutate(model = as.character(model)) %>%
        pivot_wider(names_from = model, values_from = value)
      for (col in setdiff(names(test_rows_wide), names(out))) out[[col]] <- "-"
      for (col in setdiff(names(out), names(test_rows_wide))) test_rows_wide[[col]] <- "-"
      out <- bind_rows(out, test_rows_wide) %>%
        mutate(across(everything(), ~replace_na(as.character(.), "-")))
      desired_order <- c("dataset", setdiff(names(out), "dataset"))
      out <- out[, desired_order, drop = FALSE]
    }
  }
  out
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Desempenho dos metodos com embedding por dataset
# ----------------------------------------------------------- #
summary_methods_embedding <- function(data, metric, steps = NULL) {
  message("Writing summary files")
  if (!is.null(steps)) data <- data %>% filter(strategy == steps)
  data <- data %>% filter(grepl("^(lstm|conv1d)(.+)?$", model))
  summary_tbl <- data %>%
    group_by(dataset, seed, model) %>%
    summarise(seed_mean = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    group_by(dataset, model) %>%
    summarise( avg = mean(seed_mean, na.rm = TRUE),
               std = coalesce(sd(seed_mean, na.rm = TRUE), 0), .groups = "drop" ) %>%
    group_by(dataset) %>%
    mutate(group = ifelse(grepl("^lstm", model), "lstm", "conv1d")) %>%
    group_by(dataset, group) %>%
    mutate(winner = (avg - std) <= min(avg + std)) %>%
    ungroup() %>%
    mutate(value = sprintf("%.4f +- %.4f%s", avg, std, ifelse(winner, "*", ""))) %>%
    select(dataset, model, value) %>%
    pivot_wider(names_from = model, values_from = value)
  if (identical(metric, "smape")) {
    emb_models <- sort(unique(data$model[grepl("^(lstm|conv1d)(.+)$", data$model)]))
    if (length(emb_models) > 0) {
      test_rows <- lapply(emb_models, function(emb_model) {
        ref_model <- if (grepl("^lstm", emb_model)) "lstm" else "conv1d"
        if (identical(emb_model, ref_model) || !(ref_model %in% data$model)) {
          return(tibble::tibble(dataset = c("n", "W", "p", "effect_size"), model = emb_model, value = rep("-", 4)))
        }
        paired <- data %>%
          filter(model %in% c(emb_model, ref_model)) %>%
          group_by(dataset, seed, model) %>%
          summarise(value = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = model, values_from = value) %>%
          tidyr::drop_na(dplyr::all_of(c(emb_model, ref_model)))
        n <- nrow(paired)
        if (n == 0) {
          return(tibble::tibble(dataset = c("n", "W", "p", "effect_size"), model = emb_model, value = rep("-", 4)))
        }
        wt <- stats::wilcox.test(paired[[emb_model]], paired[[ref_model]], paired = TRUE, alternative = "two.sided", exact = FALSE)
        total_rank <- n * (n + 1) / 2
        effect_size <- if (total_rank > 0) 1 - (2 * unname(wt$statistic)) / total_rank else NA_real_
        tibble::tibble(
          dataset = c("n", "W", "p", "effect_size"),
          model = emb_model,
          value = c(as.character(n), sprintf("%.1f", unname(wt$statistic)), sprintf("%.4g", wt$p.value), sprintf("%.3f", effect_size)))
      }) %>% dplyr::bind_rows()
      if (nrow(test_rows) > 0) {
        summary_tbl <- summary_tbl %>% mutate(across(everything(), as.character))
        test_rows_wide <- test_rows %>%
          mutate(model = as.character(model)) %>%
          pivot_wider(names_from = model, values_from = value)
        for (col in setdiff(names(test_rows_wide), names(summary_tbl))) summary_tbl[[col]] <- "-"
        for (col in setdiff(names(summary_tbl), names(test_rows_wide))) test_rows_wide[[col]] <- "-"
        summary_tbl <- bind_rows(summary_tbl, test_rows_wide) %>%
          mutate(across(everything(), ~replace_na(as.character(.), "-")))
        desired_order <- c("dataset", setdiff(names(summary_tbl), "dataset"))
        summary_tbl <- summary_tbl[, desired_order, drop = FALSE]
      }
    }
  }
  summary_tbl
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Ranking dos metodos por dataset
# ----------------------------------------------------------- #
ranking_dataset <- function(df, steps = "ro") {
  message('Writing summary files')
  if (!is.null(steps)) df <- dplyr::filter(df, strategy == steps)
  res <- lapply(sort(unique(df$dataset)), function(ds) {
    wide <- df |>
      dplyr::filter(dataset == ds) |>
      dplyr::group_by(ts, model) |>
      dplyr::summarise(smape = mean(smape, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = model, values_from = smape) |>
      tidyr::drop_na()
    if (nrow(wide) < 3) return(NULL)
    tibble::tibble(dataset = ds, !!!as.list(colMeans(t(apply(as.matrix(wide[-1]), 1, rank)), na.rm = TRUE)))
  }) |> dplyr::bind_rows() |> dplyr::arrange(dataset)
  dplyr::bind_rows(
    res,
    res |> dplyr::summarise(dataset = "Mean", dplyr::across(-dataset, ~ round(mean(.x, na.rm = TRUE), 3)))
  )
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Plota rankings
# ----------------------------------------------------------- #
ranking_plot <- function(df, steps = "ro", file = NULL, wisard_names = FALSE) {
  if (!is.null(steps)) df <- dplyr::filter(df, strategy == steps)
  format_model_name <- function(x) {
    x <- x[!grepl("^(lstm|conv1d).+$", x)]
    dplyr::recode(
      x,
      regwisard = "CReW",
      foresight = "ForeSight",
      conv1d = "Conv1D",
      chronos = "Chronos",
      knn = "kNN",
      .default = toupper(x)
    )
  }
  ranking_by_dataset <- lapply(sort(unique(df$dataset)), function(ds) {
    wide <- df |>
      dplyr::filter(dataset == ds) |>
      dplyr::group_by(ts, model) |>
      dplyr::summarise(smape = mean(smape, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = model, values_from = smape) |>
      tidyr::drop_na()
    if (nrow(wide) < 3) return(NULL)
    ranks <- apply(as.matrix(wide[-1]), 1, rank, ties.method = "average")
    model_names <- colnames(wide)[-1]
    if (!wisard_names) {
      keep <- !grepl("^(lstm|conv1d).+$", model_names)
      model_names <- model_names[keep]
      ranks <- ranks[keep, , drop = FALSE]
    }
    tibble::tibble(
      dataset = ds,
      model = format_model_name(model_names),
      rank = colMeans(t(ranks), na.rm = TRUE)
    )
  }) |> dplyr::bind_rows()
  summary_rank <- ranking_by_dataset |>
    dplyr::group_by(model) |>
    dplyr::summarise(
      avg_rank = mean(rank, na.rm = TRUE),
      sd_rank = sd(rank, na.rm = TRUE),
      n = dplyr::n(),
      se = sd_rank / sqrt(n),
      ci = stats::qnorm(0.975)*se,
      lower = avg_rank-se,
      upper = avg_rank+se,
      .groups = "drop"
    ) |>
    dplyr::arrange(avg_rank) |>
    dplyr::mutate(model = factor(model, levels = model))
  best_lower <- summary_rank$lower[[1]]
  best_upper <- summary_rank$upper[[1]]
  best_band <- tibble::tibble(xmin = best_lower, xmax = best_upper)
  summary_rank <- summary_rank |>
    dplyr::mutate(overlap_best = (lower <= best_upper) & (upper >= best_lower))
  p <- ggplot(summary_rank, aes(x = avg_rank, y = model)) +
    geom_rect(
      data = best_band,
      inherit.aes = FALSE,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = "#1f77b4",
      alpha = 0.10
    ) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.16, linewidth = 0.7, color = "grey25") +
    geom_point(aes(color = overlap_best), size = 2.7) +
    scale_color_manual(values = c(`TRUE` = "#d62728", `FALSE` = "grey20"), guide = "none") +
    scale_x_continuous("Ranking Médio", breaks = scales::pretty_breaks(), expand = expansion(mult = c(0.02, 0.05))) +
    ylab(NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.4),
      axis.text.y = element_text(face = "bold"),
      axis.line = element_line(linewidth = 0.4, color = "black")
    )
  if (!is.null(file))
    ggsave(file, plot = p, width = 8, height = max(4, 0.35 * nrow(summary_rank) + 1), dpi = 300)
  return(invisible(p))
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Plota tempos medios por metodo
# ----------------------------------------------------------- #
time_methods <- function(tbl, file = NULL) {
  plot_data <- tbl %>% filter(dataset == "Mean") %>% pivot_longer(-dataset, names_to = "model", values_to = "value") %>%
    filter(!grepl("^(lstm|conv1d).+$", model)) %>%
    mutate(mean = as.numeric(sub(" \\+\\-.*$", "", value)), sd = as.numeric(sub("^.*\\+\\- ", "", value)), energy_mean = mean * 60, energy_sd = sd * 60,
           model = case_when(model == "regwisard" ~ "CReW", model == "foresight" ~ "ForeSight", model == "conv1d" ~ "Conv1D", model == "chronos" ~ "Chronos",
                             model == "knn" ~ "kNN", TRUE ~ toupper(model))) %>%
    arrange(mean) %>% mutate(model = factor(model, levels = model[order(mean, decreasing = TRUE)]))
  base_energy <- plot_data$energy_mean[plot_data$model == "ForeSight"][1]
  if (is.na(base_energy) || length(base_energy) == 0) base_energy <- min(plot_data$energy_mean)
  plot_data <- plot_data %>% mutate(y_pos = as.numeric(model), rel_energy = energy_mean / base_energy,
                                    energy_label = if_else(as.character(model) == "ForeSight", sprintf("%s (1,0x)", formatC(energy_mean, format = "f", digits = 2, decimal.mark = ",")),
                                                           sprintf("%s (%sx)", formatC(energy_mean, format = "f", digits = 2, decimal.mark = ","),
                                                                   formatC(rel_energy, format = "f", digits = 1, decimal.mark = ","))))
  row_levels <- levels(plot_data$model)
  x_max <- max(plot_data$mean + plot_data$sd)
  bar_half_height <- 0.35
  p_bars <- ggplot(plot_data, aes(x = y_pos, y = mean)) +
    geom_rect(aes(xmin = 0, xmax = mean, ymin = y_pos - bar_half_height, ymax = y_pos + bar_half_height),
              fill = "#1f77b4") +
    geom_segment(aes(x = mean, xend = mean + sd, y = y_pos, yend = y_pos), linewidth = .6) +
    geom_errorbarh(aes(y = y_pos, xmin = mean + sd, xmax = mean + sd), height = .25, linewidth = .6) +
    scale_x_continuous(breaks = seq(0, ceiling(x_max), by = 1), expand = expansion(mult = c(0, .08))) +
    scale_y_continuous(breaks = seq_along(row_levels), labels = row_levels, limits = c(0.5, length(row_levels) + 0.5), expand = expansion(add = c(0, 0))) +
    labs(x = "Tempo Médio de Execução (s)", y = NULL) + theme_minimal(base_size = 14) +
    theme(axis.title.x = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(linewidth = .4))
  p_energy <- ggplot(plot_data, aes(x = 0.5, y = y_pos, label = energy_label)) +
    geom_text(size = 4.6, hjust = .5, color = "gray25") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq_along(row_levels), labels = row_levels, limits = c(0.5, length(row_levels) + 0.5), expand = expansion(add = c(0, 0))) +
    labs(title = "Consumo\nEnergético (J)") + coord_cartesian(clip = "off") + theme_minimal(base_size = 14) +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
          plot.title = element_text(hjust = .5, face = "bold", size = 15), plot.margin = margin(5.5, 5.5, 5.5, 5.5))
  p <- patchwork::wrap_plots(p_bars, p_energy, widths = c(3.4, 0.8))
  if (!is.null(file)) ggsave(file, plot = p, width = 10, height = 5.5, dpi = 300)
  invisible(p)
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Desempenho dos metodos por serie
# ----------------------------------------------------------- #
summary_series <- function(df, max_test_size) {
  df %>%
    inner_join(max_test_size, by = "dataset") %>%
    filter((strategy == "sa" & test_size == 5) | (strategy == "ro" & test_size == max_test_size)) %>%
    group_by(dataset, ts, model, seed, strategy, test_size) %>%
    summarise(smape = mean(smape, na.rm = TRUE), time_sec = mean(time_sec, na.rm = TRUE), .groups = "drop") %>%
    mutate(energy_j = 60*time_sec)
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Desempenho (SMAPE, Tempo etc.) dos metodos baseados em WiSARD
# ----------------------------------------------------------- #
summary_wisard <- function(data, metric, group_var, steps = NULL) {
  if (!is.null(steps))
    data <- data %>% filter(strategy == steps)
  data %>%
    group_by(seed, model, .data[[group_var]]) %>%
    summarise(seed_mean = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    group_by(model, .data[[group_var]]) %>%
    summarise(avg = mean(seed_mean, na.rm = TRUE),
              std = coalesce(sd(seed_mean, na.rm = TRUE), 0),
              .groups = "drop") %>%
    mutate(level = as.character(.data[[group_var]]),
           value = sprintf("%.4f +- %.4f", avg, std)) %>%
    select(level, model, value) %>%
    pivot_wider(names_from = model, values_from = value) %>%
    mutate(across(-level, ~replace_na(as.character(.), "-")))
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Parametros dos metodos baseados em WiSARD
# ----------------------------------------------------------- #
wisard_report <- function(data, metric, strategy) {
  views <- list(
    wisard   = summary_wisard(data, metric, "wisard", strategy),
    bits     = summary_wisard(data, metric, "bits", strategy),
    ram_size = summary_wisard(data, metric, "ram_size", strategy),
    dim      = summary_wisard(data, metric, "dim", strategy)
  )
  out <- list()
  for (name in names(views)) {
    out[[length(out) + 1]] <- data.frame(level = paste0("### ", name), stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- views[[name]]
    out[[length(out) + 1]] <- data.frame(level = "", stringsAsFactors = FALSE)
  }
  bind_rows(out) %>%
    mutate(across(everything(), ~replace_na(as.character(.), "")))
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Desempenho dos metodos baseados em WiSARD por parametro
# ----------------------------------------------------------- #
wisard_grid <- function(data, metric, strategy_value) {
  tbl <- data %>%
    filter(strategy == strategy_value) %>%
    mutate(model_group = case_when( model == "crew" ~ "crew",
                                    model == "rew" ~ "rew",
                                    grepl("^fs", model) & wisard == "crw" ~ "fs-crew",
                                    grepl("^fs", model) & wisard == "rw" ~ "fs-rew",
                                    TRUE ~ NA_character_) ) %>%
    filter(!is.na(model_group)) %>% group_by(seed, model_group, bits, ram_size) %>%
    summarise(seed_mean = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    group_by(model_group, bits, ram_size) %>%
    summarise(avg = mean(seed_mean, na.rm = TRUE), std = coalesce(sd(seed_mean, na.rm = TRUE), 0), .groups = "drop") %>%
    mutate(value = sprintf("%.4f +- %.4f", avg, std))
  out <- list()
  for (grp in c("crew", "rew", "fs-crew", "fs-rew")) {
    section <- tbl %>% filter(model_group == grp) %>% select(ram_size, bits, value) %>%
      pivot_wider(names_from = bits, values_from = value) %>% arrange(ram_size) %>% mutate(ram_size = as.character(ram_size))
    cols <- names(section)
    bits_row <- as.list(c("bits", cols[-1])); names(bits_row) <- cols
    ram_row <- as.list(rep("", length(cols))); names(ram_row) <- cols; ram_row[[1]] <- "ram_size"
    title_row <- as.list(rep("", length(cols))); names(title_row) <- cols; title_row[[1]] <- paste0("### ", grp)
    out[[length(out) + 1]] <- as.data.frame(title_row, stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- as.data.frame(bits_row, stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- as.data.frame(ram_row, stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- section
    out[[length(out) + 1]] <- data.frame(ram_size = "", stringsAsFactors = FALSE)
  }
  bind_rows(out) %>% mutate(across(everything(), ~replace_na(as.character(.), "")))
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Salva arquivo
# ----------------------------------------------------------- #
write_report <- function(data, path, type = c("csv2", "text")) {
  type <- match.arg(type)
  if (type == "csv2")
    write.csv2(data, path, row.names = FALSE)
  else
    write.table(data, file = path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# ---------------------- BLOCO PRINCIPAL -------------------- #
# ----------------------------------------------------------- #
current_path <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error=function(e) getwd())
output_dir <- file.path(current_path, "analysis")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ----------------------------------------------------------- #
# Carrega dados dos metodos
# ----------------------------------------------------------- #
df <- load_data(file.path(current_path, "combined_results.parquet"))
max_test_size <- df %>%
  group_by(dataset) %>%
  summarise(max_test_size = max(test_size, na.rm = TRUE), .groups = "drop")
df_filtered <- df %>%
  inner_join(max_test_size, by = "dataset") %>%
  filter(test_size == max_test_size)
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# SMAPE dos metodos
# ----------------------------------------------------------- #
tbl <- summary_dataset(df_filtered, "smape", "ro")
write_report(tbl, file.path(output_dir, "smape_datasets_ro.csv"), "csv2")
tbl <- summary_dataset(df_filtered, "smape", "sa")
write_report(tbl, file.path(output_dir, "smape_datasets_sa.csv"), "csv2")
tbl <- summary_methods_embedding(df_filtered, "smape", "ro")
write_report(tbl, file.path(output_dir, "smape_methods_embedding_ro.csv"), "csv2")
tbl <- summary_methods_embedding(df_filtered, "smape", "sa")
write_report(tbl, file.path(output_dir, "smape_methods_embedding_sa.csv"), "csv2")
tbl <- ranking_dataset(df_filtered, "ro")
write_report(tbl, file.path(output_dir, "ranking_datasets_ro.csv"), "csv2")
tbl <- ranking_dataset(df_filtered, "sa")
write_report(tbl, file.path(output_dir, "ranking_datasets_sa.csv"), "csv2")
ranking_plot(df_filtered, steps = "ro", file = file.path(output_dir, "ranking_datasets_ro.pdf"))
ranking_plot(df_filtered, steps = "sa", file = file.path(output_dir, "ranking_datasets_sa.pdf"))
tbl <- summary_series(df, max_test_size)
write_report(tbl, file.path(output_dir, "smape_series.csv"), "csv2")
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Tempos de execucao dos metodos
# ----------------------------------------------------------- #
tbl <- summary_dataset(df_filtered, "time_sec", "sa")
write_report(tbl, file.path(output_dir, "time_datasets.csv"), "csv2")
time_methods(tbl, file.path(output_dir, "time_methods.pdf"))
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Frequencia de NAN (apenas metodos baseados em WiSARD)
# ----------------------------------------------------------- #
tbl <- summary_dataset(df_filtered, "nan_freq", "ro")
write_report(tbl, file.path(output_dir, "nan_freq_datasets_ro.csv"), "csv2")
tbl <- summary_dataset(df_filtered, "nan_freq", "sa")
write_report(tbl, file.path(output_dir, "nan_freq_datasets_sa.csv"), "csv2")
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Carrega dados dos metodos baseados em WiSARD
# ----------------------------------------------------------- #
df <- load_data(file.path(current_path, "combined_results_wisard.parquet"))
df_filtered <- df %>%
  inner_join(max_test_size, by = "dataset") %>%
  filter(test_size == max_test_size)
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# SMAPE dos metodos baseados em WiSARD
# ----------------------------------------------------------- #
tbl <- summary_dataset(df_filtered, "smape", "ro")
write_report(tbl, file.path(output_dir, "smape_wisard_ro.csv"), "csv2")
tbl <- summary_dataset(df_filtered, "smape", "sa")
write_report(tbl, file.path(output_dir, "smape_wisard_sa.csv"), "csv2")
tbl <- ranking_dataset(df_filtered, "ro")
write_report(tbl, file.path(output_dir, "ranking_wisard_ro.csv"), "csv2")
tbl <- ranking_dataset(df_filtered, "sa")
write_report(tbl, file.path(output_dir, "ranking_wisard_sa.csv"), "csv2")
ranking_plot(df_filtered, steps = "ro", file = file.path(output_dir, "ranking_wisard_ro.pdf"))
ranking_plot(df_filtered, steps = "sa", file = file.path(output_dir, "ranking_wisard_sa.pdf"))
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Tempo de execucao dos metodos baseados em WiSARD
# ----------------------------------------------------------- #
tbl <- summary_dataset(df_filtered, "time_sec", "sa")
write_report(tbl, file.path(output_dir, "time_wisard.csv"), "csv2")
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Frequencia de NAN dos metodos baseados em WiSARD
# ----------------------------------------------------------- #
tbl <- summary_dataset(df_filtered, "nan_freq", "ro")
write_report(tbl, file.path(output_dir, "nan_freq_wisard_ro.csv"), "csv2")
tbl <- summary_dataset(df_filtered, "nan_freq", "sa")
write_report(tbl, file.path(output_dir, "nan_freq_wisard_sa.csv"), "csv2")
# ----------------------------------------------------------- #

# ----------------------------------------------------------- #
# Desempenho por parametro dos modelos baseados em WiSARD
# ----------------------------------------------------------- #
metrics <- c("smape", "nan_freq", "time_sec")
strategies <- c("ro", "sa")
for (metric in metrics) {
  for (strategy in if (metric == "time_sec") "sa" else strategies) {
    message("Writing summary files")
    write_report(
      wisard_report(df_filtered, metric, strategy),
      file.path(output_dir, if (metric == "time_sec") sprintf("wisard_%s.txt", metric) else sprintf("wisard_%s_%s.txt", metric, strategy)),
      "text"
    )
    if (metric != "time_sec") {
      write_report(
        wisard_grid(df_filtered, metric, strategy),
        file.path(output_dir, sprintf("wisard_grid_%s_%s.txt", metric, strategy)),
        "text"
      )
    }
  }
}
# ----------------------------------------------------------- #
