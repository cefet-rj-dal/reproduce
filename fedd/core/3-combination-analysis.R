##
## 3 - Combination Analysis and Ranking
## ------------------------------------
## Purpose:
## - Define a broader set of 3W examples
## - Load all CSV results per example and compute SoftED metrics
## - Rank best drifters/combinations and run non-parametric tests
##
## Notes:
## - Adjust local paths for `data_path` and `folder_path` as needed.
##
library(dalevents)
library(tidyr)
library(daltoolbox)
library(devtools)
library(harbinger)

data_path <- 'data/'

data(oil_3w_Type_1)
data(oil_3w_Type_2)
load(paste0(data_path, 'data_3w_tp3_real_sample_exp.RData'))
load(paste0(data_path, 'data_3w_tp4_sample.RData'))
data(oil_3w_Type_5)
data(oil_3w_Type_6)
data(oil_3w_Type_7)
data(oil_3w_Type_8)

ws <- 4000

## Example set: choose per-well series and window size
example_list <- list(
  # '1WELL1' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`), ws=ws),
  '1WELL2' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`), ws=ws),
  '1WELL6' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`), ws=ws),
  '1WELL6_2' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`), ws=ws),
  # '2WELL2' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00002_20131104014101`), ws=ws),
  '2WELL3_1' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20141122214325`), ws=ws),
  '2WELL3_2' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20170728150240`), ws=ws), # Ok
  '2WELL3_3' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20180206182917`), ws=ws),
  # '3WELL1' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00001_20170320120025`), ws=ws), # One Event
  '3WELL14_1' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170917190000`), ws=ws), # One Event
  '3WELL14_2' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170917140000`), ws=ws), # One Event
  '3WELL14_2' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170918010114`), ws=ws), # One Event
  # '4WELL1_1' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316110203`), ws=ws), # One Event
  '4WELL1_2' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316130000`), ws=ws), # One Event
  # '4WELL1_3' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316150005`), ws=ws), # One Event
  # '5WELL15_1' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00015_20170620160349`), ws=ws), # One Event
  '5WELL15_2' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00015_20171013140047`), ws=ws), # Ok
  '5WELL16_1' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00016_20180405020345`), ws=ws),
  '5WELL16_2' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00016_20180426142005`), ws=ws),
  # '6WELL2_1' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140212170333`), ws=ws),
  '6WELL2_2' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140301151700`), ws=ws),
  '6WELL2_3' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140325170304`), ws=ws), # Ok
  '6WELL4_1' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00004_20171031181509`), ws=ws),
  # '7WELL1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00001_20170226220309`), ws=ws)
  '7WELL6_1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00006_20180618110721`), ws=ws), # One Event
  '7WELL6_2' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00006_20180620181348`), ws=ws), # Ok One Event
  # '7WELL18_1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00018_20180611040207`), ws=ws), # One Event
  '8WELL19' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00019_20170301182317`), ws=ws),
  '8WELL20' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00020_20120410192326`), ws=ws),
  '8WELL21' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00021_20170509013517`), ws=ws)
)


folder_path <- 'results/'

one_event_examples <- c(
  '3WELL1', '3WELL14_1', '3WELL14_2',
  '4WELL1_1', '4WELL1_2', '4WELL1_3',
  '5WELL15_1', '5WELL16_2',
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
    # softed_metrics['normalization'] <- cats[[length(cats)-3]]
    softed_metrics['drifter_name'] <- cats[[1]]
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

exhibition_columns <- c('precision', 'recall', 'F1', 'n_detections', 'file', 'example_code', 'drifter', 'drifter_name')#, 'normalization', 'data')

# consolidated_results <- consolidated_results[!is.na(consolidated_results['F1']), exhibition_columns]

# consolidated_results <- consolidated_results[consolidated_results['drifter'] == 'dfr_multi_criteria', ]

for (colname in names(consolidated_results)){
  consolidated_results[colname] <- as.vector(unlist(consolidated_results[colname]))
}

consolidated_results[consolidated_results['drifter'] != 'dfr_multi_criteria', 'drifter_name'] <- consolidated_results[consolidated_results['drifter'] != 'dfr_multi_criteria', 'drifter']

example_performance <- aggregate(consolidated_results[, exhibition_columns], by=list(consolidated_results[, 'example_code']), FUN=head, n=1, na.rm=TRUE)
example_performance[order(-example_performance[['F1']]), ]

# Anova Best detectors

kruskal.test(consolidated_results[complete.cases(consolidated_results), 'F1'], consolidated_results[complete.cases(consolidated_results), 'drifter_name'], na.rm=TRUE)
pairwise.wilcox.test(consolidated_results[complete.cases(consolidated_results), 'F1'], consolidated_results[complete.cases(consolidated_results), 'drifter_name'], p.adjust.method = 'BH')
kruskal.test(consolidated_results[complete.cases(consolidated_results), 'precision'], consolidated_results[complete.cases(consolidated_results), 'drifter_name'], na.rm=TRUE)
pairwise.wilcox.test(consolidated_results[complete.cases(consolidated_results), 'precision'], consolidated_results[complete.cases(consolidated_results), 'drifter_name'], p.adjust.method = 'BH')
kruskal.test(consolidated_results[complete.cases(consolidated_results), 'recall'], consolidated_results[complete.cases(consolidated_results), 'drifter_name'], na.rm=TRUE)
pairwise.wilcox.test(consolidated_results[complete.cases(consolidated_results), 'recall'], consolidated_results[complete.cases(consolidated_results), 'drifter_name'], p.adjust.method = 'BH')

consolidated_results['count'] <- 0
consolidated_results[complete.cases(consolidated_results), 'count'] <- 1
drifters <- aggregate(consolidated_results[, c('precision', 'recall', 'F1', 'count')], by=list(consolidated_results[, 'drifter_name']), FUN=mean, na.rm=TRUE)
drifters <- drifters[order(-drifters$F1),]

drifters
