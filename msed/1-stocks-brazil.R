##
## 1 - Stocks (Brazil): Multi-Scale Event Detection
## -------------------------------------------------
## Purpose:
## - Load Brazilian stock indices from the consolidated Excel dataset
## - Run multi-scale event detection per index against EPU-based labels
## - Compute F1/precision/recall per series and write a metrics CSV
##
library(readxl)
library(tibble)

source("multi-scale-detect.R")

experiment <- "Event"

stocks_name_columns <- c(
    "Data",
    experiment,
    "Ibovespa",
    "Ibrx100",
    "Ibrx50",
    "Ibra",
    "IGC"
)

stocks <- read_xlsx("dataset/Base de Dados Consolidada base line.xlsx", sheet = "Stocks")
method_emd <- "CEEMD"

reference <- stocks[c("Data", experiment)]

df_results <- NULL

name_serie <- ""
sensitivity <- c(0)
specificity <- c(0)
precision <- c(0)
recall <- c(0)
f1 <- c(0)
df_results <- data.frame(name_serie, f1, precision, recall)

for (name_column in stocks_name_columns) {
    if (name_column == experiment || name_column == "Data") {
        next
    }
    serie_df <- data.frame(setNames(list(stocks$Data, stocks[[name_column]]), c("time", "value")))
    
    metrics <- multi_scale_event_detect(
        serie = serie_df,
        type_events = list("all"),
        rf_dataframe = reference
    )

    df_results <- add_row(df_results,
        name_serie = name_column,
        f1 = metrics["F1"],
        precision = metrics["precision"],
        recall = metrics["recall"]
    )
}
file_metrics <- sprintf("files/stocks_brasil_%s.csv", "metrics")
write.csv(df_results, file_metrics)
