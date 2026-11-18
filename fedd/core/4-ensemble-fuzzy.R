##
## 4 - Ensemble of Drifter Detectors (Fuzzy MV/TV)
## -----------------------------------------------
## Purpose:
## - Combine top-performing drifter detection outputs using fuzzy logic
##   and majority-vote (MV) or threshold-vote (TV) strategies
## - Write ensemble detection CSVs per example for downstream evaluation
##
## Notes:
## - Adjust `folder_path` to point to your results directory.
##
library(dalevents)
library(tidyr)
library(daltoolbox)
library(devtools)
library(harbinger)

folder_path <- 'results/'

hutils <- harutils()
har_fuz <- hutils$har_fuzzify_detections_triangle

shift <- function(x, n){
  
  c(rep(NA, n), x[(n:length(x))])
}

## Examples to ensemble (by example code)
example_list <- c(
  # '1WELL1', 
  # '2WELL2', 
  # '3WELL1', 1000
  # '4WELL1_1', 
  # '5WELL15_1', 
  # '6WELL2_1', 
  # '7WELL1'
  '1WELL2', '1WELL6_1', '1WELL6_2',
  '2WELL3_1', '2WELL3_3', '2WELL3_2',#, Ok 1000
  '3WELL14_1', '3WELL14_2', '3WELL14_3',
  '4WELL1_2', #'4WELL1_3', # Missing One
  '5WELL16_1', '5WELL16_2', '5WELL15_2',# Ok 2500
  '6WELL2_2', '6WELL4_1', '6WELL2_3', # Ok 1500
  '7WELL6_1', '7WELL6_2', #'7WELL18_1' Ok 2000
  '8WELL19','8WELL20', '8WELL21'

  )

# Temporal tolerance for fuzzy expansion
tolerance <- 4000

calculate_fuzzy <- function(example_name, best_models_files, tolerance){
  fuzzy_df <- c()
  drifters_df <- c()
  for (f in best_models_files){
    print(f)
    
    comb_run <- NULL
    comb_run <- as.data.frame(read.csv(paste0(folder_path, f)))
    
    # print(names(comb_run))
    
    drifters_df <- cbind(drifters_df, comb_run[['drift']])
    
    # tolerance <- as.integer(nrow(comb_run) * 0.1)
    
    ens <- as.data.frame(har_fuz(comb_run[['drift']], tolerance=tolerance))
    
    fuzzy_df <- cbind(fuzzy_df, ens[,1])
  }
  
  return(list(drifters_df, fuzzy_df))
}


for (example_name in example_list){
  drifter <- 'dfr_multi_criteria'
  drifter_target <- 'combination'
  incremental_memory <- 'combination'
  
  best_models_files <- c(
    paste0('cla_majority-dfr_adwin-zscore-distribution-incremental-', example_name ,'.csv'),
    paste0('cla_majority-dfr_kldist-fixed_zscore-distribution-incremental-', example_name, '.csv'),
    paste0('cla_majority-dfr_kswin-fixed_zscore-distribution-incremental-', example_name, '.csv'),
    paste0('cla_majority-dfr_mcdd-fixed_zscore-distribution-incremental-', example_name, '.csv'),
    paste0('cla_majority-dfr_page_hinkley-zscore-distribution-incremental-', example_name, '.csv'),
    paste0('cla_majority-dfr_aedd-fixed_zscore-distribution-incremental-', example_name, '.csv')
  )
  
  output <- calculate_fuzzy(example_name, best_models_files=best_models_files, tolerance=tolerance)
  drifters_df <- output[[1]]
  fuzzy_df <- output[[2]]
  
  # Fuzzy Combination
  # comb_run['drift'] <- (rowSums(fuzzy_df) > (length(best_models_files)/2)) * 1
  # comb_run['drift'] <- comb_run['drift'] - shift(comb_run[, 'drift'], n=1)
  # comb_run[(comb_run['drift'] != 1) | (is.na(comb_run['drift'])), 'drift'] <- 0
  
  # Testing
  # if (example_name == '8WELL20'){
  #   
  # }else{
  #   next
  # }
  
  # Fuzzy Combination MV
  drifters_check <- as.data.frame(drifters_df)
  fuzzy_check <- as.data.frame(fuzzy_df)
  fuzzy_mv_output <- as.data.frame(rep(FALSE, nrow(drifters_df)))
  rownames(fuzzy_mv_output) <- rownames(drifters_check)
  names(fuzzy_mv_output) <- c('drift')
  fuzzy_mv_output['index'] <- as.integer(rownames(drifters_check))
  for (i in 1:nrow(fuzzy_df)){
    row <- fuzzy_check[rownames(fuzzy_check) == i, ]
    if (sum(row) > length(best_models_files)/2){
     fuzzy_mv_output[rownames(fuzzy_mv_output) == i, 'drift'] <- TRUE
     drifters_check <- drifters_check[rownames(drifters_check) %in% as.character(i:nrow(drifters_df)),]
     fuzzy_check <- fuzzy_check[rownames(fuzzy_check) %in% as.character(i:nrow(fuzzy_df)),]
     drift_columns <- names(fuzzy_check[,row != 0])
     
     # Testing
     # print(drift_columns)
     # print(i)
     # print(row)
     # print(sum(row))
     # print(nrow(drifters_check))
     # print(nrow(fuzzy_check))
     
     fuzzy_check <- c()
     for (feat in names(drifters_check)){
       if (feat %in% drift_columns){
         drift_index <- as.integer(rownames(head(drifters_check[drifters_check[, feat], feat, drop=FALSE], 1)))
         drifters_check[rownames(drifters_check) %in% c(drift_index), feat] <- FALSE
       }
       
       # Recalculate Fuzzy
       ens <- as.data.frame(har_fuz(drifters_check[[feat]], tolerance=tolerance))
       fuzzy_values <- ens[,1]
       attr(fuzzy_values, 'type') <- NULL
       fuzzy_check <- cbind(fuzzy_check, fuzzy_values)
       
       # Testing
       # plot(x=rownames(drifters_check), y=fuzzy_values)
     }
     fuzzy_check <- as.data.frame(fuzzy_check)
     rownames(fuzzy_check) <- rownames(drifters_check)
     names(fuzzy_check) <- names(drifters_check)
    }
  }
  fuzzy_mv_output['example'] <- substring(example_name, 2)
  fuzzy_mv_output['drifter'] <- drifter
  fuzzy_mv_output['event_type'] <- substring(example_name, 1, 1)
  
  # 
  # comb_run['drift'] <- (rowSums(fuzzy_df) > (length(best_models_files)/2)) * 1
  # comb_run['drift'] <- comb_run['drift'] - shift(comb_run[, 'drift'], n=1)
  # comb_run[(comb_run['drift'] != 1) | (is.na(comb_run['drift'])), 'drift'] <- 0
  
  # comb_run[comb_run['drift'] == 1, 'index']
  
  # comb_run['X'] <- NULL
  
  write.csv(fuzzy_mv_output, paste0(folder_path, 'dfr_multi_criteria_best_fuzzy-combination-', example_name, '.csv'))
  
  
  # Fuzzy Combination TV
  drifters_check <- as.data.frame(drifters_df)
  fuzzy_check <- as.data.frame(fuzzy_df)
  fuzzy_tv_output <- as.data.frame(rep(FALSE, nrow(drifters_df)))
  rownames(fuzzy_tv_output) <- rownames(drifters_check)
  names(fuzzy_tv_output) <- c('drift')
  fuzzy_tv_output['index'] <- as.integer(rownames(drifters_check))
  for (i in 1:nrow(fuzzy_df)){
    row <- fuzzy_check[rownames(fuzzy_check) == i, ]
    if (sum(row) >= 4){
      fuzzy_tv_output[rownames(fuzzy_tv_output) == i, 'drift'] <- TRUE
      drifters_check <- drifters_check[rownames(drifters_check) %in% as.character(i:nrow(drifters_df)),]
      fuzzy_check <- fuzzy_check[rownames(fuzzy_check) %in% as.character(i:nrow(fuzzy_df)),]
      drift_columns <- names(fuzzy_check[,row != 0])
      
      # Testing
      # print(drift_columns)
      # print(i)
      # print(row)
      # print(sum(row))
      # print(nrow(drifters_check))
      # print(nrow(fuzzy_check))
      
      fuzzy_check <- c()
      for (feat in names(drifters_check)){
        if (feat %in% drift_columns){
          drift_index <- as.integer(rownames(head(drifters_check[drifters_check[, feat], feat, drop=FALSE], 1)))
          drifters_check[rownames(drifters_check) %in% c(drift_index), feat] <- FALSE
        }
        
        # Recalculate Fuzzy
        ens <- as.data.frame(har_fuz(drifters_check[[feat]], tolerance=tolerance))
        fuzzy_values <- ens[,1]
        attr(fuzzy_values, 'type') <- NULL
        fuzzy_check <- cbind(fuzzy_check, fuzzy_values)
        
        # Testing
        # plot(x=rownames(drifters_check), y=fuzzy_values)
      }
      fuzzy_check <- as.data.frame(fuzzy_check)
      rownames(fuzzy_check) <- rownames(drifters_check)
      names(fuzzy_check) <- names(drifters_check)
    }
  }
  fuzzy_tv_output['example'] <- substring(example_name, 2)
  fuzzy_tv_output['drifter'] <- drifter
  fuzzy_tv_output['event_type'] <- substring(example_name, 1, 1)
  
  write.csv(fuzzy_tv_output, paste0(folder_path, 'dfr_multi_criteria_best_fuzzy_tv-combination-', example_name, '.csv'))
  
  # EFER Combination
  # comb_run['drift'] <- rowSums(drifters_df) >= 1
  
  # plot(x=comb_run[,'index'], y=comb_run[, 'drift'])
  
  # comb_run[comb_run['drift'] == 1, 'index']
  # 
  # comb_run['X'] <- NULL
  # 
  # write.csv(comb_run, paste0(folder_path, 'dfr_multi_criteria_best_or-combination-', example_name, '.csv'))
  # comb_run['drift'] <- 0
  
  # MV Combination
  # comb_run['drift'] <- rowSums(drifters_df) > length(best_models_files)/2
  
  # plot(x=comb_run[,'index'], y=comb_run[, 'drift'])
  
  # comb_run[comb_run['drift'] == 1, 'index']
  
  # comb_run['X'] <- NULL
  
  # write.csv(comb_run, paste0(folder_path, 'dfr_multi_criteria_best_and-combination-', example_name, '.csv'))
  # comb_run['drift'] <- 0
  
  # TV Combination
  # comb_run['drift'] <- rowSums(drifters_df) >= 2
  
  # plot(x=comb_run[,'index'], y=comb_run[, 'drift'])
  
  # comb_run[comb_run['drift'] == 1, 'index']
  # 
  # comb_run['X'] <- NULL
  
  # write.csv(comb_run, paste0(folder_path, 'dfr_multi_criteria_best_tv-combination-', example_name, '.csv'))
  # comb_run['drift'] <- 0
}
