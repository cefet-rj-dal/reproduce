###############################################################################################################################
# 1) ARIMA Baseline: Training and Evaluation
# ---------------------------------------------------------------------------------------------------------------
# Steps
# - Load shared utilities
# - Load dataset (monthly ethanol production by state and product)
# - Define scenarios (states x products) and rolling-origin test years
# - Train ARIMA per scenario and test year; save plots and aggregate metrics
###############################################################################################################################
source("0-functions-and-libraries.R")


###############################################################################################################################
# --- LOAD DATA ------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
dataset <- read.csv2("data/Etanol_df.csv")


###############################################################################################################################
# --- PARAMETERS ------------------------------------------------------------------------------------------------------------ #
###############################################################################################################################
# Rolling-origin evaluation window
test_months <- 12
remove_final_years <- 0
initial_test_year <- (max(year(as.Date(dataset$Data))))
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
    for(AnoTeste in (initial_test_year-4):initial_test_year){
      titulo  <- "ARIMA"
      remove_final_years <- max(year(dataset$Data)) - AnoTeste
      result <- F_ARIMA(df = dataset, estado=state, etanol=product, meses_teste = test_months, titulo = titulo, 
                        seed=1, remove_anos_finais=remove_final_years)
      print(result)
      results_ARIMA <- rbind(results_ARIMA, result)
    }}}

# c) Saving results
saveRDS(results_ARIMA, "results/results_ARIMA.RDS")
