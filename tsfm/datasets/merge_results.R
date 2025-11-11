library(purrr)
library(dplyr)

# merge_results
# Purpose: Consolidate individual experiment result files (.rdata) for each dataset
#          into per-dataset merged files, and a global merged file (and CSVs).
# How it works:
# - For each dataset folder, reads all .rdata result files under results/.
# - Loads and row-binds them into a single data frame per dataset.
# - Saves the merged data to <dataset>_combined_results.rdata and .csv.
# - Finally, binds all datasets together and saves combined_results.rdata and .csv.
# Usage:
# - Place this script in the working directory where dataset folders live
#   (e.g., climate/, emissions/, etc.). Then source and call merge_results().

merge_results <- function(){
  current_path <- getwd()
  datasets <- c('climate', 'emissions', 'fertilizers', 'gdp', 'pesticides')
  results_path <- 'results/'
  consolidated_data <- list()

  for (ds in datasets) {
    merge_path <- paste(current_path, ds, results_path, sep="/")
    filename <- sprintf("%s/%s/%s_combined_results.rdata", current_path, ds, ds)
    print(filename)

    # Remove existing merged file to avoid mixing old results
    if (file.exists(filename)) {
      file.remove(filename)
    }

    # Load all .rdata files and row-bind them into a single data frame
    all_files <- list.files(merge_path, full.names=TRUE, recursive=TRUE, pattern='.rdata') %>%
      map_df(~ get(load(file=.x)))

    # Accumulate per-dataset merged results
    consolidated_data[[ds]] <- all_files

    # Save per-dataset merged results (.rdata and .csv)
    save(all_files, file = filename)
    write.csv(all_files, file = gsub("rdata", "csv", filename), row.names=FALSE)
  }

  # Merge all datasets together
  consolidated_data <- bind_rows(consolidated_data)

  # Save global merged results (.rdata and .csv)
  filename <- paste(current_path, 'combined_results.rdata', sep="/")
  save(consolidated_data, file = filename)
  write.csv(consolidated_data, file = gsub("rdata", "csv", filename), row.names=FALSE)
}

# Execute when run directly
merge_results()
