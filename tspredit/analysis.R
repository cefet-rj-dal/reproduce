## Analysis helpers: aggregate metrics and generate simple videos from plots
library(dplyr)
library(reshape)

generate_analysis <- function(data, rank, ro) {
  # Tag scenario label (ro: rolling origin; sa: steps-ahead)
  if (ro) data$scenario <- sprintf("ro%d", data$test_size) else data$scenario <- sprintf("sa%d", data$test_size)

  # Pivot train metrics
  data_summary <- data |> dplyr::select(model = method, name = name, scenario = scenario, train = smape_train)
  result_train <- cast(data_summary, formula = name + scenario ~ model, mean, value = 'train')
  colnames(result_train)[3:ncol(result_train)] <- sprintf("%s_train", colnames(result_train[3:ncol(result_train)]))

  # Pivot test metrics
  data_summary <- data |> dplyr::select(model = method, name = name, scenario = scenario, test = smape_test)
  result_test <- cast(data_summary, formula = name + scenario ~ model, mean, value = 'test')
  colnames(result_test)[3:ncol(result_test)] <- sprintf("%s_test", colnames(result_test[3:ncol(result_test)]))

  # Merge and tidy
  result <- merge(x=result_train, y=result_test, by=c("name", "scenario"))
  result[,3:ncol(result)] <- round(result[,3:ncol(result)], 2)

  # Simple win flag (example comparing the last two columns)
  result$victory <- 0
  result$victory[result[,ncol(result)-2] > result[,ncol(result)-1]] <- 1

  # Order by rank list and scenario
  result <- merge(x=result, y=rank, by=c("name", "name"))
  result <- result[order(result$pos, result$scenario),]
  rownames(result) <- sprintf("%d-%s", result$pos, result$scenario)
  result$pos <- NULL
  return(result)
}

generate_mp4 <- function(dataset, suffix) {
  # Build an mp4 by stitching per-step JPEGs per dataset (requires ImageMagick)
  imagename  <-  ""
  for (i in 1:length(dataset)) {
    name <- names(dataset)[i]
    print(name)
    pattern <- sprintf("%s\\S+%s\\d.jpg", name, suffix)
    x <- sort(list.files(path = "graphics", pattern = pattern, all.files = FALSE,
                         full.names = FALSE, recursive = FALSE,
                         ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
    x <- sprintf("graphics/%s", x)
    imagename <- stri_paste(unlist(x), collapse=' ')
    videoname <- sprintf("graphics/%s-%s.mp4", name, suffix)
    system(sprintf("convert -delay 300 %s mp4:%s", imagename, videoname))
  }
}

# Examples (adjust file names to your result artifacts)
# result_ro <- get(load("results/tsreg_arima-ro.rdata"))
# result_ro <- rbind(result_ro, get(load("results/ts_elm-3-55-1-none-ts_diff-none-ro.rdata")))
# analysis <- generate_analysis(result_ro, rank, ro=TRUE)

# result_sa <- get(load("results/tsreg_arima-sa.rdata"))
# result_sa <- rbind(result_sa, get(load("results/ts_elm-3-55-1-none-ts_diff-none-sa.rdata")))
# analysis <- generate_analysis(result_sa, rank, ro=FALSE)

# generate_mp4(dataset, "ro")
# generate_mp4(dataset, "sa")
