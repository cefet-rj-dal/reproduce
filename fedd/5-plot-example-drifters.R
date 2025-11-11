##
## 5 - Plot Example Drifters vs. Events
## ------------------------------------
## Purpose:
## - Given a selected example (well) and a list of model output files,
##   render a stacked plot of the time series and drifter detections.
## - Helps visually compare detectors and ensembles against event markers.
##
## Notes:
## - Adjust `data_path`, `folder_path`, and `example_string` to your setup.
##
library(dalevents)
library('ggpubr')
library(devtools)
library("dplyr")
library('ggplot2')

data_path <- 'data/'
data(oil_3w_Type_1)
data(oil_3w_Type_2)
load(paste0(data_path, 'data_3w_tp3_real_sample_exp.RData'))
load(paste0(data_path, 'data_3w_tp4_sample.RData'))
data(oil_3w_Type_5)
data(oil_3w_Type_6)
data(oil_3w_Type_7)
data(oil_3w_Type_8)

## Example dictionary: maps example code to its source series
example_list <- list(
  # '1WELL1' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`), ws=1000),
  '1WELL2' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`)),
  '1WELL6_1' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`)),
  '1WELL6_2' = list(data=as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`)),
  # '2WELL2' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00002_20131104014101`), ws=1000),
  '2WELL3_1' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20141122214325`)),
  '2WELL3_2' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20170728150240`)), # Ok
  '2WELL3_3' = list(data=as.data.frame(oil_3w_Type_2$Type_2$`WELL-00003_20180206182917`)),
  # '3WELL1' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00001_20170320120025`), ws=1000), # One Event
  '3WELL14_1' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170917190000`)), # One Event
  '3WELL14_2' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170917140000`)), # One Event
  '3WELL14_2' = list(data=as.data.frame(data_3w_tp3_real_sample_exp$`WELL-00014_20170918010114`)), # One Event
  # '4WELL1_1' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316110203`), ws=1000), # One Event
  '4WELL1_2' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316130000`)), # One Event
  '4WELL1_3' = list(data=as.data.frame(data_3w_tp4_sample$`WELL-00001_20170316150005`)), # One Event
  # '5WELL15_1' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00015_20170620160349`), ws=1000), # One Event
  '5WELL15_2' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00015_20171013140047`)),
  '5WELL16_1' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00016_20180405020345`)),
  '5WELL16_2' = list(data=as.data.frame(oil_3w_Type_5$Type_5$`WELL-00016_20180426142005`)),
  # '6WELL2_1' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140212170333`), ws=1000),
  '6WELL2_2' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140301151700`)),
  '6WELL2_3' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00002_20140325170304`)), # Ok
  '6WELL4_1' = list(data=as.data.frame(oil_3w_Type_6$Type_6$`WELL-00004_20171031181509`)),
  # '7WELL1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00001_20170226220309`), ws=1000)
  '7WELL6_1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00006_20180618110721`)), # One Event
  '7WELL6_2' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00006_20180620181348`)), # One Event
  # '7WELL18_1' = list(data=as.data.frame(oil_3w_Type_7$Type_7$`WELL-00018_20180611040207`), ws=150), # One Event
  '8WELL19' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00019_20170301182317`)),
  '8WELL20' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00020_20120410192326`)),
  '8WELL21' = list(data=as.data.frame(oil_3w_Type_8$Type_8$`WELL-00021_20170509013517`))
)

## Select which example to plot
example_string <- '5WELL15_2'

best_models_files <- c(
  paste0('cla_majority-dfr_adwin-zscore-distribution-incremental-', example_string ,'.csv'),
  paste0('cla_majority-dfr_kldist-fixed_zscore-distribution-incremental-', example_string, '.csv'),
  paste0('cla_majority-dfr_kswin-fixed_zscore-distribution-incremental-', example_string, '.csv'),
  paste0('cla_majority-dfr_mcdd-fixed_zscore-distribution-incremental-', example_string, '.csv'),
  paste0('cla_majority-dfr_page_hinkley-zscore-distribution-incremental-', example_string, '.csv'),
  paste0('cla_majority-dfr_aedd-fixed_zscore-distribution-incremental-', example_string, '.csv'),
  paste0('dfr_multi_criteria_best_and-combination-', example_string, '.csv'),
  paste0('dfr_multi_criteria_best_tv-combination-', example_string, '.csv'),
  paste0('dfr_multi_criteria_best_fuzzy-combination-', example_string, '.csv'),
  paste0('dfr_multi_criteria_best_fuzzy_tv-combination-', example_string, '.csv'),
  paste0('dfr_multi_criteria_best_or-combination-', example_string, '.csv')
)

folder_path <- 'results/'

df <- example_list[[example_string]][['data']]
df['index'] <- 1:nrow(df)
df[is.na(df['class']), 'class'] <- 0
df[df['class'] == 0, 'class'] <- 1
df[df[['class']] %in% c(5, 8), 'class'] <- 1

# unique(df['class'])

df['class'] <- c(diff(df[['class']]), 0)
event_indexes <- as.integer(rownames(df[df['class'] != 0,]))

# df[df['class'] != 0,]

yname_list <- list(
  dfr_adwin = 'ADWIN',
  dfr_aedd = 'AEDD',
  dfr_kldist = 'KLDIST',
  dfr_kswin = 'KSWIN',
  dfr_mcdd = 'MCDD',
  dfr_page_hinkley = 'Page Hinkley',
  best_and = 'CEDD-MV',
  best_tv = 'CEDD-TV',
  best_fuzzy = 'FEDD-MV',
  best_fuzzy_tv = 'FEDD-TV',
  best_or = 'CEDD EFER'
)

drifter_plot_list <- list()

ts_plot <- ggplot(data=df, aes(x=index, y=T_JUS_CKP, group=1)) + 
  geom_line() +
  xlab('') +
  ylab('T_JUS_CKP') +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y=element_text(size=8, face="bold")
  )

for (e_index in event_indexes){
  ts_plot <- ts_plot + geom_vline(xintercept=e_index, linetype='dotted', col='blue')
}

drifter_plot_list[[example_string]] <- list(ts = ts_plot)

for (f in best_models_files){
  if ((!length(grep(example_string, f))) | (!(f %in% best_models_files))) next
  print(f)
  
  comb_run <- NULL
  comb_run <- read.csv(paste0(folder_path, f))
  comb_run['file'] <- f
  
  if(comb_run[1, 'drifter'] == 'dfr_multi_criteria'){
    ylabel <- substring(strsplit(f, '-')[[1]][1], nchar('dfr_multi_criteria_') + 1)
  }else{
    ylabel <- comb_run[1, 'drifter']
  }
  
  
  drifter_plot <- ggplot() + 
    geom_line(data=comb_run, aes(x=index, y=0)) +
    geom_point(data=comb_run[comb_run['drift'] == 1,], aes(x=index, y=0), color='red') +
    xlab('') +
    ylab(yname_list[[ylabel]]) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y=element_text(size=8, face="bold")
    )
  
  for (e_index in event_indexes){
    drifter_plot <- drifter_plot + geom_vline(xintercept=e_index, linetype='dotted', col='blue')
  }
  
  drifter_plot_list[[example_string]][[f]] <- drifter_plot
}

ggarrange(
  plotlist=drifter_plot_list[[example_string]],
  heights=c(5, vector(mode='logical', length=11) + 2),
  # widths=c(14, vector(mode='logical', length=length(uni_data_drifters) + 1) + 18),
  align='v',
  ncol=1, nrow=12)
