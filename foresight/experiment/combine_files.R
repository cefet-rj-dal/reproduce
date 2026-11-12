library(purrr)
library(dplyr)
library(arrow)

merge_files <- function(path, model = NULL, name = NULL) {
  current_path <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) getwd())
  datasets <- list.dirs(current_path, recursive = FALSE, full.names = FALSE) %>%
    keep(~ dir.exists(file.path(current_path, .x, path)))
  results_list <- list()
  
  for (ds in datasets) {
    merge_path <- file.path(current_path, ds, path)
    message(sprintf("%s/%s/%s", current_path, ds, path))
    all_files <- list.files(merge_path, full.names = TRUE, recursive = TRUE, pattern = "\\.rdata$") %>%
      (\(x) {
        if (!is.null(model)) {
          if (startsWith(model, "!")) {
            pattern <- substring(model, 2)
            x <- x[!grepl(pattern, x, perl = TRUE)]
          } else {
            x <- x[grepl(model, x, perl = TRUE)]
          }
        }
        x
      })() %>%
      map_df(~ {
        obj_name <- load(.x)
        get(obj_name[1])
      })
    results_list[[ds]] <- all_files
  }
  results <- bind_rows(results_list)
  filename <- file.path(
    current_path,
    sprintf("combined_%s%s.parquet", path, if (!is.null(name)) paste0("_", name) else "")
  )
  message(sprintf("Writing '%s'...", basename(filename)))
  write_parquet(results, filename)
}

merge_files("results", model = "!(fs|rew)")
#merge_files("results", model="fs|rew", name="wisard")
#merge_files("results", name="all")