library(purrr)
library(dplyr)


merge_files <- function(path, model=NULL){
  
  current_path <- getwd()
  datasets <- c('bioenergy', 'climate', 'emissions', 'fertilizers', 'gdp', 'pesticides')
  consolidated_data <- list()
  
  for (ds in datasets) {
    merge_path <- paste(current_path, ds, path, sep="/")
    filename <- sprintf('%s/%s/%s_combined_%s.rdata', current_path, ds, ds, path)
    if (!is.null(model)) filename <- gsub('.rdata', sprintf('_%s.rdata', gsub('[^a-zA-Z0-9]', '', model)), filename)
    print(filename)
    if (file.exists(filename)) file.remove(filename)
    
    pattern <- ifelse(!is.null(model), paste0('.*', model, '.*\\.rdata'), '.rdata')
    all_files <- list.files(merge_path, full.names=TRUE, recursive=TRUE, pattern=pattern) %>%
      map_df(~ get(load(file=.x)))
    consolidated_data[[ds]] <- all_files
  }
  
  consolidated_data
  consolidated_data <- bind_rows(consolidated_data) %>%
    mutate(smape = 2 * abs(pred - true) / (abs(pred) + abs(true)) * 100)
  
  filename <- sprintf('%s/combined_%s.rdata', current_path, path)
  if (!is.null(model)) filename <- gsub('.rdata', sprintf('_%s.rdata', gsub('[^a-zA-Z0-9]', '', model)), filename)
  save(consolidated_data, file = filename)
}

merge_files('results')