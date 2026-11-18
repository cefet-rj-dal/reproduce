# SST — Sea Surface Temperature Prediction

- Author: Eduardo Ogasawara
- Category: Experiments

---

## Overview
This folder documents experiments inspired by the article:

Salles, R., Mattos, P., Iorgulescu, A.-M. D., Bezerra, E., Lima, L., Ogasawara, E. (2016). Evaluating temporal aggregation for predicting the sea surface temperature of the Atlantic Ocean. Ecological Informatics. https://doi.org/10.1016/j.ecoinf.2016.10.004

Objective: evaluate how different temporal aggregation windows (e.g., daily, weekly, monthly) impact the predictability of Atlantic Ocean sea surface temperature (SST) across multiple forecast horizons.

---

## Experiment Structure (proposed)
If/when code is added, use the following semantic numbering and English naming. This makes execution order and purpose explicit.

1-load_data: data acquisition and raw loading
- Scripts: `1-load_data.py` (or `1-load_data.md` if documented-only)
- Example tasks: download or locate SST grids/series; subset spatial/temporal region; basic integrity checks.

2-preprocessing: cleaning and alignment
- Scripts: `2-preprocessing.py`
- Example tasks: handle missing values, unit normalization, detrending/seasonality options, train/validation/test split.

3-aggregation: temporal aggregation windows
- Scripts: `3-aggregation.py`
- Example tasks: build aggregated series (e.g., mean/max/min) for window sizes (e.g., 7, 14, 30 days); persist versions for modeling.

4-modeling: forecasting models
- Scripts: `4-modeling_arima.py`, `4-modeling_mlp.py`, `4-modeling_lstm.py`, `4-modeling_elm.py`, etc.
- Example tasks: fit per-aggregation models; tune hyperparameters; produce forecasts for target horizons.

5-evaluation: metrics and comparisons
- Scripts: `5-evaluation.py`
- Example tasks: compute RMSE/MAE/MAPE; compare aggregations and horizons; rank models; export tables/plots.

6-reporting: figures and tables
- Scripts: `6-reporting.py` or `6-reporting.md`
- Example tasks: generate publication-quality figures; summarize findings; save artefacts to `reports/`.

Notes:
- If Jupyter notebooks are present, convert them to Markdown (`.md`) with the same numbering (e.g., `4-modeling_arima.md`).
- Keep variable names, comments, and outputs in English.
- Prefer small, single-responsibility scripts rather than monoliths to match the numbering.

---

## Methodology (summary)
- Data: SST time series from the Atlantic Ocean (synthetic and/or observational products).
- Factor under study: temporal aggregation window size (e.g., weekly vs. monthly).
- Procedure: for each aggregation and forecast horizon, fit models and evaluate predictive accuracy.
- Rationale: aggregation can reduce noise but may remove fine-scale dynamics; the study seeks a practical balance.

---

## Expected Findings (from the paper)
- Higher aggregation can reduce noise but may lose fine-scale variability.
- Benefits of high temporal resolution are more evident for short horizons and fade as the horizon grows.
- There exists a “sweet spot” aggregation balancing noise reduction and signal retention.

---

## Current Folder Contents
At the time of this README update, no experiment code or notebooks are present in `sst/`.
- Markdown files in this folder: this `sst/readme.md` only.
- If additional `.md` files are later added, reference them here for quick navigation.

---

## How to Contribute or Extend
- Place new scripts in this folder following the numbering scheme above.
- Use English for code, filenames, function/variable names, and comments. Add short, didactic comments explaining each step.
- If you add notebooks (`.ipynb`), also provide a Markdown export with the same filename and number.

---

## Citation
If you use or extend these experiments, please cite the paper:

Salles, R., Mattos, P., Iorgulescu, A.-M. D., Bezerra, E., Lima, L., Ogasawara, E. (2016). Evaluating temporal aggregation for predicting the sea surface temperature of the Atlantic Ocean. Ecological Informatics. https://doi.org/10.1016/j.ecoinf.2016.10.004

