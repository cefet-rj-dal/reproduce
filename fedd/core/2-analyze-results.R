##
## 2 - Analyze Experiment Results (SoftED Metrics)
## -----------------------------------------------
## Purpose:
## - Load 3W well examples and align event annotations
## - Read per-combination CSV results and compute SoftED metrics
## - Aggregate results to compare detectors and settings across examples
##
## Notes:
## - Paths reference a user environment; adjust `data_path`, `load_all(...)`,
##   and `folder_path` to your local setup.
##
library(dalevents)
library(tidyr)
library(daltoolbox)
library(devtools)
library(harbinger)

# Local data path containing RData files (relative)
data_path <- 'data/'

data(oil_3w_Type_1)
data(oil_3w_Type_2)
load(paste0(data_path, 'data_3w_tp3_real_sample_exp.RData'))
load(paste0(data_path, 'data_3w_tp4_sample.RData'))
data(oil_3w_Type_5)
data(oil_3w_Type_6)
data(oil_3w_Type_7)
data(oil_3w_Type_8)


## Example configuration: each key selects a well/time series and window size
example_list <- list(
  '1WELL1' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`), ws=1000),
  # '1WELL2' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`), ws=150)
  # '1WELL6' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`), ws=150),
  '2WELL2' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00002_20131104014101`), ws=1000),
  # '2WELL3_1' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20141122214325`), ws=100),
  # '2WELL3_2' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20170728150240`), ws=150),
  '3WELL1' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00001_20170320120025`), ws=1000), # One Event
  # '3WELL14_1' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170917190000`), ws=150), # One Event
  # '3WELL14_2' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170917140000`), ws=150), # One Event
  '4WELL1_1' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316110203`), ws=1000), # One Event
  # '4WELL1_2' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316130000`), ws=150), # One Event
  # '4WELL1_3' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316150005`), ws=150), # One Event
  '5WELL15_1' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00015_20170620160349`), ws=1000), # One Event
  # '5WELL15_2' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00015_20171013140047`), ws=150),
  # '5WELL16_1' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00016_20180405020345`), ws=150),
  '6WELL2_1' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140212170333`), ws=1000),
  # '6WELL2_2' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140301151700`), ws=150),
  # '6WELL2_3' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140325170304`), ws=150),
  '7WELL1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00001_20170226220309`), ws=1000)
  # '7WELL6_1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00006_20180618110721`), ws=150), # One Event
  # '7WELL6_2' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00006_20180620181348`), ws=150), # One Event
  # '8WELL19' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00019_20170301182317`), ws=150)
  # '8WELL20' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00020_20120410192326`), ws=150),
  # '8WELL21' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00021_20170509013517`), ws=150)
)


# Directory holding CSV results for all combinations (relative)
folder_path <- 'results/'

one_event_examples <- c(
  '3WELL1', '3WELL14_1', '3WELL14_2',
  '4WELL1_1', '4WELL1_2', '4WELL1_3',
  '5WELL15_1',
  '7WELL6_1', '7WELL6_2'
  )

consolidated_results <- c()
for (example_string in names(example_list)){
  event_type <- substring(example_string, 1, 1)
  example <- substring(example_string, 2)
  print(example_string)
  
  df <- example_list[[example_string]][['data']]
  
  df['index'] <- as.numeric(rownames(df))
  
  df[is.na(df['class']),'class'] <- 0
  
  event1 <- head(df[df['class'] == unique(df['class'])[2,], 'index'], 1)
  df['event'] <- 0
  df[df['index'] == event1, 'event'] <- 1
  if (!(example_string %in% one_event_examples)){
    event2 <- head(df[df['class'] == unique(df['class'])[3,], 'index'], 1)
    df[df['index'] == event2, 'event'] <- 1
  }
  
  results <- c()
  softed <- har_eval_soft(sw_size=example_list[[example_string]][['ws']])
  softed_results <- c()
  for (f in list.files(folder_path)){
    if (!length(grep(example_string, f))) next
    print(f)
    
    comb_run <- NULL
    comb_run <- read.csv(paste0(folder_path, f))
    comb_run['file'] <- f
    if (grepl('multi_criteria', f)){
      next
    }
    comb_run[, c('X', 'accuracy', 'precision', 'recall', 'f1', 'auc', 'drifter_target', 'classifier', 'incremental_memory')] <- NULL
    results <- rbind(results, comb_run)
    comb_run['event'] <- 0
    comb_run[comb_run['index'] == event1, 'event'] <- 1
    if (!(example_string %in% one_event_examples)){
      comb_run[comb_run['index'] == event2, 'event'] <- 1
    }
    # comb_run[(comb_run['index'] >= event1) & (comb_run['index'] <= event2), 'event'] <- 1
    # print(table(comb_run[,c('drift', 'event')]))
    # if(nrow(table(comb_run[,c('drift', 'event')])) == 1){
    #   print('No detection')
    #   next
    # }
    softed_metrics <- evaluate(softed, comb_run[['drift']]==1, comb_run[['event']]==1)
    softed_metrics$confMatrix <- NULL
    softed_metrics['n_detections'] <- sum(comb_run['drift'])
    softed_metrics['classifier'] <- comb_run[1, 'classifier']
    softed_metrics['drifter'] <- comb_run[1, 'drifter']
    softed_metrics['drifter_target'] <- comb_run[1, 'drifter_target']
    softed_metrics['incremental_memory'] <- comb_run[1, 'incremental_memory']
    softed_metrics['event_type'] <- comb_run[1, 'event_type']
    softed_metrics['example'] <- comb_run[1, 'example']
    cats <- strsplit(f, '-')[[1]]
    diff_data <- 'No Diff'
    if(cats[[1]] == 'diff'){
      diff_data <- 'diff'
    }
    softed_metrics['data'] <- diff_data
    softed_metrics['normalization'] <- cats[[length(cats)-3]]
    softed_metrics['file'] <- f
    
    softed_results <- rbind(softed_results, softed_metrics)
  }
  
  softed_results <- as.data.frame(softed_results)
  rownames(softed_results) <- 1:nrow(softed_results)
  
  softed_results <- softed_results[(softed_results['event_type'] == event_type) & (softed_results['example'] == example), ]
  
  names_res <- names(softed_results)
  for (n in 1:length(names_res)){
    if(names_res[n] %in% c('TP', 'FP', 'FN', 'TN')){
      names_res[n] <- paste0(names_res[n], 's')
    }
  }
  
  names(softed_results) <- names_res
  
  consolidated_results <- rbind(consolidated_results, softed_results)
}

consolidated_results <- consolidated_results[order(-as.vector(unlist(consolidated_results[['F1']]))),]

consolidated_results[['example_code']] <- paste0(consolidated_results[['event_type']], consolidated_results[['example']])

exhibition_columns <- c('precision', 'recall', 'F1', 'file', 'example_code', 'drifter')#, 'normalization', 'data')

consolidated_results[!is.na(consolidated_results['F1']), exhibition_columns]

best_combs <- c()
for (example_code in unique(consolidated_results[['example_code']])){
  best_combs <- rbind(best_combs, head(consolidated_results[consolidated_results['example_code'] == example_code, exhibition_columns], 1))
}

best_combs

classification_metrics <- c("TPs", "FPs", "FNs", "TNs", "accuracy", "sensitivity", "specificity", "prevalence", "PPV",
                            "NPV", "detection_rate", "detection_prevalence", "balanced_accuracy", "precision", "recall", "F1")

for (colname in names(consolidated_results)){
  consolidated_results[colname] <- as.vector(unlist(consolidated_results[colname]))
}

drifters_results <- aggregate(consolidated_results[classification_metrics], by=list(as.vector(unlist(consolidated_results[['drifter']])), as.vector(unlist(consolidated_results[['normalization']]))), FUN=mean, na.rm=TRUE)
drifters_results <- drifters_results[order(-drifters_results[['F1']]), c('Group.1', 'Group.2', 'accuracy', 'precision', 'recall', 'F1')]

drifters_results
