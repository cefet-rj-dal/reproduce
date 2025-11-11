SoftED: Experiments and Organization

This directory contains the code, data pointers, and results for the experiments reported in the paper:

Salles, R., Lima, J., Reis, M., Coutinho, R., Pacitti, E., Masseglia, F., Akbarinia, R., Chen, C., Garibaldi, J., Porto, F., Ogasawara, E. SoftED: Metrics for soft evaluation of time series event detection. Computers and Industrial Engineering, 2024. DOI: 10.1016/j.cie.2024.110728.

Goals
- Introduce SoftED metrics for soft evaluation of time-series event detection.
- Associate true events with their representative detections with temporal tolerance.
- Compare SoftED against hard (classification) metrics and the NAB score across datasets/methods.

Folder Structure
- Metrics (R implementations): [softed_metrics/](softed_metrics/)
  - [softed_metrics.r](softed_metrics/softed_metrics.r), [hard_metrics.r](softed_metrics/hard_metrics.r), [nab_metrics.r](softed_metrics/nab_metrics.r)
- Experiment code: [experiment_code/](experiment_code/)
  - Shared utilities: [exp_func_SoftED.r](experiment_code/exp_func_SoftED.r) — loading, aggregation, Hard/SoftED/NAB evaluation helpers.
  - Per dataset: [evaluate_dataset/](experiment_code/evaluate_dataset/)
    - [1-evaluate-3W.R](experiment_code/evaluate_dataset/1-evaluate-3W.R) (3W, per well)
    - [2-evaluate-NAB.R](experiment_code/evaluate_dataset/2-evaluate-NAB.R) (NAB, POSIXct timestamps)
    - [3-evaluate-TMN.R](experiment_code/evaluate_dataset/3-evaluate-TMN.R) (TMN)
    - [4-evaluate-Yahoo.R](experiment_code/evaluate_dataset/4-evaluate-Yahoo.R) (Yahoo)
  - Aggregation/Comparison:
    - [5-aggregate-metrics.R](experiment_code/5-aggregate-metrics.R) — aggregates F1/precision/recall for SoftED/Hard/NAB
    - [6-compare-all-datasets.R](experiment_code/6-compare-all-datasets.R) — cross-dataset comparisons and figures
  - Event detection (examples/helpers): [experiment_code/event_detection/](experiment_code/event_detection/) — e.g., 1-run-detection-methods.R, harbinger.r, anomalies.R, myTimeseries.R
- Datasets used in the experiments: [detection_data/](detection_data/)
- Aggregated results and figures: [metrics_results/](metrics_results/)
- Presentations: [presentations/](presentations/)
- Qualitative analysis and survey: [quali_survey/](quali_survey/)

Prerequisites
- R (>= 4.x recommended).
- Core packages: tidyverse, dplyr, tibble, stringr, anomalize, magrittr, otsad, ggplot2, RColorBrewer, Cairo.
- Optional (for some detectors): tensorflow, keras, tfdatasets, nnfor, e1071, randomForest, elmNNRcpp, RSNNS, fpc, TSPred.

How to Run
1) Open R/RStudio with the working directory set to `softed/experiment_code`.
2) Run the dataset scripts:
   - `source("evaluate_dataset/1-evaluate-3W.R")`
   - `source("evaluate_dataset/2-evaluate-NAB.R")`
   - `source("evaluate_dataset/3-evaluate-TMN.R")`
   - `source("evaluate_dataset/4-evaluate-Yahoo.R")`
3) Aggregate and compare:
   - `source("5-aggregate-metrics.R")`
   - `source("6-compare-all-datasets.R")`

Outputs
- Per-dataset evaluations: `evaluate_metrics_*.rds` files and intermediates under [experiment_code/evaluate_dataset/results/](experiment_code/evaluate_dataset/results/) (when applicable).
- Aggregated results: `datasets_*` structures created by [5-aggregate-metrics.R](experiment_code/5-aggregate-metrics.R).
- Consolidated comparisons and figures: produced by [6-compare-all-datasets.R](experiment_code/6-compare-all-datasets.R) (see also [metrics_results/](metrics_results/)).

References
- Metrics code (R): [softed_metrics/](softed_metrics/)
- SoftED paper (CIE 2024): DOI 10.1016/j.cie.2024.110728.

