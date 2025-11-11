###############################################################################################################################
# 2) PRE + MLM Experiments: Rolling-Origin Evaluation
# ---------------------------------------------------------------------------------------------------------------
# Steps
# - Load shared utilities
# - Load dataset and define global tuning grids
# - Define PRE+MLM model families (preprocess + learner)
# - Run scenarios (states x products) across rolling-origin test years
# - Aggregate all RDS results into a single CSV
###############################################################################################################################
source("0-functions-and-libraries.R")


###############################################################################################################################
# --- LOAD DATA ------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
dataset <- read.csv2("data/Etanol_df.csv")


###############################################################################################################################
# --- PARAMETERS ------------------------------------------------------------------------------------------------------------ #
###############################################################################################################################
# a) General parameters
remove_final_years <- 0
initial_test_year <- (max(year(as.Date(dataset$Data))))
sw_par <- seq(9, 18, 3)
input_size <- c(1:10)

# b) Specific parameters
PRE_MLM = list(
  # b1) an_mlm
  an_lstm   = list(base_model = ts_lstm(ts_norm_an()), ranges = list(epochs=700)),
  an_elm    = list(base_model = ts_elm(ts_norm_an()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                          'relu', 'purelin'))),
  an_svm    = list(base_model = ts_svm(ts_norm_an()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                    epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  an_mlp    = list(base_model = ts_mlp(ts_norm_an()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
  an_conv1d = list(base_model = ts_conv1d(ts_norm_an()), ranges = list(epochs=700)),
  
  # b2) diff_mlm
  diff_lstm   = list(base_model = ts_lstm(ts_norm_diff()), ranges = list(epochs=700)),
  diff_elm    = list(base_model = ts_elm(ts_norm_diff()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                              'relu', 'purelin'))),
  diff_svm    = list(base_model = ts_svm(ts_norm_diff()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                        epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  diff_mlp    = list(base_model = ts_mlp(ts_norm_diff()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
  diff_conv1d = list(base_model = ts_conv1d(ts_norm_diff()), ranges = list(epochs=700)),
  # b3) gmm_mlm
  gmm_lstm   = list(base_model = ts_lstm(ts_norm_gminmax()), ranges = list(epochs=700)),
  gmm_elm    = list(base_model = ts_elm(ts_norm_gminmax()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                                'relu', 'purelin'))),
  gmm_svm    = list(base_model = ts_svm(ts_norm_gminmax()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                          epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  gmm_mlp    = list(base_model = ts_mlp(ts_norm_gminmax()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
  gmm_conv1d = list(base_model = ts_conv1d(ts_norm_gminmax()), ranges = list(epochs=700))
)


###############################################################################################################################
# --- TRAINING AND TESTING SCENARIOS ---------------------------------------------------------------------------------------- #
###############################################################################################################################

# --------------------------------------------------------------------------------------------------------------------------- #
# 1) STATE:SP | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "SP", product = "hydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 2) STATE:SP | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "SP", product = "anhydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 3) STATE:GO | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "GO", product = "hydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 4) STATE:GO | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "GO", product = "anhydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)


# --------------------------------------------------------------------------------------------------------------------------- #
# 5) STATE:MG | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "MG", product = "hydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 6) STATE:MG | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "MG", product = "anhydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 7) STATE:MT | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "MT", product = "hydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 8) STATE:MT | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "MT", product = "anhydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 9) STATE:MS | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "MS", product = "hydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 10) STATE:MS | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "MS", product = "anhydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 11) STATE:PR | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "PR", product = "hydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 12) STATE:MS | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "PR", product = "anhydrous", AnoTesteInicial=initial_test_year, PRE_MLM=PRE_MLM)


###############################################################################################################################
# --- INTEGRATE SCENARIO RESULTS INTO A .CSV FILE---------------------------------------------------------------------------- #
###############################################################################################################################
integrateAndSaveResults("results")

