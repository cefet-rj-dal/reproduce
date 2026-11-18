library(dplyr)
library(zip)

mod_select <- function(df, preprocess_filter, instance = NULL) {
  df_temp <- df %>%
    filter(preprocess %in% preprocess_filter,
           test_size <= 3,
           strategy == 'ro') %>%
    group_by(instance, dataset) %>%
    summarise(smape_val = mean(smape), .groups = 'drop') %>%
    group_by(dataset) %>%
    slice_min(smape_val, n = 1) %>%
    ungroup()
  df_temp <- df %>%
    semi_join(df_temp, by = c('instance', 'dataset'))
  df_temp$instance <- instance
  return(df_temp)
}


mod_naive <- function(df, naive) {
  df_temp <- df %>% 
    filter(instance == naive)
  df_temp$instance <- 'naive'
  return(df_temp)
}


### --- LOAD DATA ---
obj <- load('./combined_results.rdata')
df_all <- get(obj[1]) %>%
  filter(!instance %in% c('aidan', 'baseline','naive'))

### --- AIDAN ---
df_aidan <- mod_select(
  df_all,
  preprocess_filter = setdiff(unique(df_all$preprocess), 'none'),
  instance = 'aidan'
)

### --- BASELINE ---
df_baseline <- mod_select(
  df_all,
  preprocess_filter = 'none',
  instance = 'baseline'
)

### --- NAIVE ---
naive <- 'mlp_norm-diff_aug-jitter'
df_naive <- mod_naive(df_all, naive)

### --- SAVE DATA ---
df_all <- bind_rows(df_all, df_aidan, df_baseline, df_naive)
save(df_all, file = 'combined_results.rdata')
tmp_csv <- tempfile(fileext = '.csv')
write.csv2(df_all, tmp_csv, row.names = FALSE)
zipr('combined_results.zip', files = tmp_csv)