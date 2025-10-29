library(dplyr)
library(reshape)


generate_analysis <- function(data, rank, ro) {
  if (ro)
    data$scenario <- sprintf("ro%d", data$test_size)
  else
    data$scenario <- sprintf("sa%d", data$test_size)
  
  data_summary <- data |> dplyr::select(model = method, name = name, scenario = scenario, train = smape_train) 
  result_train <- cast(data_summary,  formula = name + scenario ~ model, mean, value = 'train')
  colnames(result_train)[3:ncol(result_train)] <- sprintf("%s_train", colnames(result_train[3:ncol(result_train)]))
  
  data_summary <- data |> dplyr::select(model = method, name = name, scenario = scenario, test = smape_test) 
  result_test <- cast(data_summary,  formula = name + scenario ~ model, mean, value = 'test')
  colnames(result_test)[3:ncol(result_test)] <- sprintf("%s_test", colnames(result_test[3:ncol(result_test)]))
  
  result <- merge(x=result_train, y=result_test, by=c("name", "scenario"))
  result[,3:ncol(result)] <- round(result[,3:ncol(result)], 2)
  
  result$victory <- 0
  result$victory[result[,ncol(result)-2] > result[,ncol(result)-1]] <- 1
  result <- merge(x=result, y=rank, by=c("name", "name"))
  result <- result[order(result$pos, result$scenario),]
  rownames(result) <- sprintf("%d-%s", result$pos, result$scenario)
  result$pos <- NULL
  return(result)  
}

generate_mp4 <- function(dataset, suffix) {
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
  
  #ls *.mp4 -1
  #mkvmerge -o project_name.mp4 file1.mp4 \+ file2.mp4 
}

if (TRUE) {
  result_ro <- get(load("results/tsreg_arima-ro.rdata"))
  result_ro <- rbind(result_ro, get(load("results/ts_elm-3-55-1-none-ts_diff-none-ro.rdata")))
  analysis <- generate_analysis(result_ro, rank, ro=TRUE)
}

if (TRUE) {
  result_sa <- get(load("results/tsreg_arima-sa.rdata"))
  result_sa <- rbind(result_sa, get(load("results/ts_elm-3-55-1-none-ts_diff-none-sa.rdata")))
  analysis <- generate_analysis(result_sa, rank, ro=TRUE)
}

if (FALSE) {
  generate_mp4(dataset, "ro")
  generate_mp4(dataset, "sa")
}
