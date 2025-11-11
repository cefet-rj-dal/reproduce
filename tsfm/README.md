# TSFM — LLMs for Time Series on Low-Frequency Data

- Authors: Rodrigo Parracho, Fernando Alexandrino, Maykon de Souza Figueiredo, Lucas P. da Silva, Bruno D. de Macedo, Arthur L. Vaz, Daniel Louback, Victor C. Desouzart, Rodrigo Salles, Fábio Porto, Diego Carvalho, Eduardo Ogasawara
- Category: Experiments / Reproducibility

---

## Overview
This folder contains experimental materials related to the paper:

Parracho, R., Alexandrino, F., Figueiredo, M. de S., Silva, L. P. Da, Macedo, B. D. De, Vaz, A. L., Louback, D., Desouzart, V. C., Salles, R., Porto, F., Carvalho, D., Ogasawara, E. (2025). Leveraging Large Language Models for Time Series Prediction on Low-Frequency Data. In: Brazilian Symposium on Databases (SBBD). https://doi.org/10.5753/sbbd.2025.247062

Objective: evaluate Large Language Models (LLMs) for forecasting low-frequency time series, comparing zero-shot and fine-tuning strategies against classical baselines and neural models.

---

## What’s Here
- Datasets and workflows: [tsfm/datasets/](datasets/)
  - Baseline and LLM workflows: [wf_arima.R](datasets/wf_arima.R), [wf_lstm.R](datasets/wf_lstm.R), [wf_chronos.R](datasets/wf_chronos.R), [wf_llama.R](datasets/wf_llama.R)
  - Chronos bridge (R/Python): [chronos R](chronos/zero%20shot/ts_chronos.R), [chronos Python](chronos/zero%20shot/ts_chronos.py)
  - Lag-LLaMA bridge (R/Python): [ts_lag_llama.R](llama/lag-llama/scripts/ts_lag_llama.R), [ts_lag_llama.py](llama/lag-llama/scripts/ts_lag_llama.py)
  - Fine-tune CLI (Python/R): [finetune_CLI.py](llama/lag-llama/scripts/finetune_CLI.py), [finetune_CLI.R](llama/lag-llama/scripts/finetune_CLI.R)
- Results: per-dataset `results/` subfolders and consolidated `combined_results.*`
- Legacy overview: [tsfm/README.md](README.md) (short intro)
- Combined results README: [combined_results_README.txt](combined_results_README.txt)

---

## Experiment Structure (semantic numbering)
To make execution order and goals explicit, use the numbered wrappers below. Original workflow scripts are kept for compatibility.

1-load_data: dataset ingestion and layout (not provided here)
- Tasks: gather low-frequency series; define train/test splits; validate schema; save `input/*.csv`.

2-preprocessing: cleaning and alignment (not provided here)
- Tasks: handle missing/zeros; normalization; alignment across series.

3-configuration: experiment parameters (not provided here)
- Tasks: set test size, sampling, model lists.

4-modeling: forecasting models
- ARIMA: [4-modeling_arima.R](datasets/4-modeling_arima.R) (wraps [wf_arima.R](datasets/wf_arima.R))
- LSTM: [4-modeling_lstm.R](datasets/4-modeling_lstm.R) (wraps [wf_lstm.R](datasets/wf_lstm.R))
- Chronos (zero-shot): [4-modeling_chronos.R](datasets/4-modeling_chronos.R) (wraps [wf_chronos.R](datasets/wf_chronos.R))
- Lag-LLaMA (zero-shot/fine-tune): [4-modeling_llama.R](datasets/4-modeling_llama.R) (wraps [wf_llama.R](datasets/wf_llama.R))

5-evaluation: metrics and consolidation
- Merge all results: [5-evaluation_merge_results.R](datasets/5-evaluation_merge_results.R) (wraps [merge_results.R](datasets/merge_results.R))

6-reporting: figures and tables (not provided here)
- Tasks: generate plots and tables for the paper.

Notes
- All code, filenames, and comments are in English with short, didactic notes.
- No Jupyter notebooks are present; if you add any, also export to Markdown with the same numbering (e.g., `4-modeling_chronos.md`).

---

## How to Run (examples)
- Chronos zero-shot across datasets: run `Rscript tsfm/datasets/4-modeling_chronos.R`
- Lag-LLaMA zero-shot across datasets: run `Rscript tsfm/datasets/4-modeling_llama.R`
- ARIMA baseline: run `Rscript tsfm/datasets/4-modeling_arima.R`
- LSTM experiments: run `Rscript tsfm/datasets/4-modeling_lstm.R`
- Merge results: run `Rscript tsfm/datasets/5-evaluation_merge_results.R`

Prerequisites
- R environment with required packages (e.g., daltoolbox and dependencies).
- Python environment for Chronos and Lag-LLaMA bridges (GPU optional).

---

## Code Notes (bridges)
- Chronos (Python): `tsfm/chronos/zero shot/ts_chronos.py` includes didactic docstrings and zero-shot pipeline calls.
- Lag-LLaMA (Python): `tsfm/llama/lag-llama/scripts/ts_lag_llama.py` predicts with a pre-trained checkpoint, zero-shot or fine-tune.
- Fine-tune CLI: `tsfm/llama/lag-llama/scripts/finetune_CLI.py` accepts `--train_dataset`, `--steps_ahead`, `--num_epochs`, `--samples`, `--zero_shot`.

---

## Citation
If you use or extend these experiments, please cite the paper:

Parracho, R., Alexandrino, F., Figueiredo, M. de S., Silva, L. P. Da, Macedo, B. D. De, Vaz, A. L., Louback, D., Desouzart, V. C., Salles, R., Porto, F., Carvalho, D., Ogasawara, E. (2025). Leveraging Large Language Models for Time Series Prediction on Low-Frequency Data. In: Brazilian Symposium on Databases (SBBD). https://doi.org/10.5753/sbbd.2025.247062
