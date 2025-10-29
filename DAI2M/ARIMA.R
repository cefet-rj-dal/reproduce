###############################################################################################################################
# --- LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------------------------------ #
###############################################################################################################################
source("Functions_and_Libraries.R")


###############################################################################################################################
# --- LOAD DATA ------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
dataset <- read.csv2("data/Etanol_df.csv")


###############################################################################################################################
# --- PARAMETERS ------------------------------------------------------------------------------------------------------------ #
###############################################################################################################################
meses_teste = 12
remove_anos_finais = 0
AnoTesteInicial = (max(year(as.Date(dataset$Data))))
states <- c("SP", "GO", "MG", "MT", "MS", "PR")
products <- c("hydrous", "anhydrous")


###############################################################################################################################
# --- TRAINING AND TESTING SCENARIOS  --------------------------------------------------------------------------------------- #
###############################################################################################################################
# a) Creating the results dataset
results_ARIMA <- data.frame()

# b) Training and Testing all scenarios
for(state in states){
  for(product in products){
    scenario = paste0(state, "_", product)
    create_directories(scenario)
    for(AnoTeste in (AnoTesteInicial-4):AnoTesteInicial){
      titulo  <- "ARIMA"
      remove_anos_finais = max(year(dataset$Data)) - AnoTeste
      result <- F_ARIMA(df = dataset, estado=state, etanol=product, meses_teste = meses_teste, titulo = titulo, 
                        seed=1, remove_anos_finais=remove_anos_finais)
      print(result)
      results_ARIMA <- rbind(results_ARIMA, result)
    }}}

# c) Saving results
saveRDS(results_ARIMA, "results/results_ARIMA.RDS")
