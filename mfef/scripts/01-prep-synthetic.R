# =========================================================
# 01-prep-synthetic.R
# Reshape raw synthetic series and metadata into the nested
# structure consumed by the MFEF pipeline.
#
# Input : data/synthetic/series.csv, data/synthetic/metadata.csv
# Output: data/processed/synthetic_series.rds
# =========================================================

library(tidyverse)
library(here)

# Paths (relative to the project root)

series_file   <- here::here("data", "synthetic", "raw", "series.csv")
metadata_file <- here::here("data", "synthetic", "raw", "metadata.csv")

output_dir    <- here::here("data", "synthetic", "processed")
output_file <- file.path(output_dir, "synthetic_series.rds")


# Load
series_df <- read_csv(series_file, show_col_types = FALSE) %>%
  rename(tempo = time, valor = value)

metadata_df <- read_csv(metadata_file, show_col_types = FALSE) %>%
  rename(serie_id_nome = series_id_name)

# Build one row per series with nested observations.
# is_true_cp flags the first index of each anomalous segment.
synthetic_series <- series_df %>%
  arrange(series_id, tempo) %>%
  group_split(series_id) %>%
  map_df(function(df_one) {
    
    id <- df_one$series_id[1]
    meta_row <- metadata_df %>% filter(series_id == id)
    
    tibble(
      serie_id      = id,
      serie_id_nome = meta_row$serie_id_nome[1],
      data = list(
        tibble(
          tempo = df_one$tempo,
          valor = df_one$valor,
          is_true_cp =
            as.logical(df_one$is_anomaly) &
            !lag(as.logical(df_one$is_anomaly), default = FALSE)
        )
      )
    )
    
  })

# Save
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(synthetic_series, file = output_file)

cat("Output written:", output_file, "\n")
cat("Number of series:", nrow(synthetic_series), "\n")