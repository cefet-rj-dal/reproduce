DAI2M: Data-Centric AI for Ethanol Production Forecasting

This directory hosts the code and artifacts used to evaluate the DAI2M methodology for ethanol production forecasting in Brazil.

Paper
- Mello, A., Giusti, L., Tavares, T., Alexandrino, F., Guedes, G., Soares, J., Barbastefano, R., Porto, F., Carvalho, D., Ogasawara, E. DAI2M: Ethanol Production Forecasting in Brazil Using Data-Centric Artificial Intelligence Methodology. IEEE Latin America Transactions, 22(11): 899–910, Oct 22, 2024.

Objectives
- Benchmark a classical baseline (ARIMA) and DAI2M pipelines (pre-processing + machine learning models) for monthly ethanol production forecasting.
- Evaluate across multiple Brazilian states and product types (hydrous, anhydrous) using a Rolling Forecast Origin strategy.
- Report accuracy (R2) and save visual comparisons between adjustments and forecasts.

Folder Structure
- 0-functions-and-libraries.R: Common libraries and helper functions used by all experiments (ARIMA and PRE+MLM).
  - File: [0-functions-and-libraries.R](0-functions-and-libraries.R)
- 1-arima-baseline.R: ARIMA baseline training and evaluation across scenarios.
  - File: [1-arima-baseline.R](1-arima-baseline.R)
- 2-pre-mlm-experiments.R: PRE+MLM (data pre-processing + learner) experiments with tuning and rolling-origin testing.
  - File: [2-pre-mlm-experiments.R](2-pre-mlm-experiments.R)
- Data folder: input datasets.
  - Path: [data/](data/)
- Results folder: model outputs per scenario (RDS) and the integrated CSV.
  - Path: [results/](results/)
- ACF/PACF analysis PDF: residual diagnostics (ARIMA and DIFF+ELM).
  - File: [ACF - PACF Analysis.pdf](ACF%20-%20PACF%20Analysis.pdf)
- R project file: open this folder as an R project.
  - File: [Ethanol_Production.Rproj](Ethanol_Production.Rproj)

Prerequisites
- R (>= 4.x recommended).
- Packages used by the experiments (installed automatically in your R environment if available):
  - Core: daltoolbox, forecast, tspredit, lubridate, ggplot2, zoo, corrplot.
  - Learners (optional depending on selected models): nnet, elmNNRcpp, e1071.

How to Run
1) Set your working directory to this folder (DAI2M).
2) Run the ARIMA baseline across all scenarios:
   - `source("1-arima-baseline.R")`
3) Run the PRE+MLM experiments across all scenarios (this may take longer):
   - `source("2-pre-mlm-experiments.R")`

Key Implementation Notes
- Both scripts expect the dataset at `data/Etanol_df.csv` with date, state (sigla), and production series.
- Rolling Forecast Origin is applied to evaluate multiple test years per scenario. Plots are saved under `graphics/<STATE>_<PRODUCT>/`.
- Results are saved to `results/*.RDS`. At the end of PRE+MLM runs, an integrated CSV is produced at `results/IntegratedResults.csv`.

Outputs
- RDS files per scenario with model metrics and parameters.
- Integrated CSV with all scenarios combined: `results/IntegratedResults.csv`.
- JPG plots comparing fitted and forecasted values per scenario/year under `graphics/`.

Citation
If you use this code, please cite:

Mello, A., Giusti, L., Tavares, T., Alexandrino, F., Guedes, G., Soares, J., Barbastefano, R., Porto, F., Carvalho, D., Ogasawara, E. DAI2M: Ethanol Production Forecasting in Brazil Using Data-Centric Artificial Intelligence Methodology. IEEE Latin America Transactions, 22(11): 899–910, Oct 22, 2024.

