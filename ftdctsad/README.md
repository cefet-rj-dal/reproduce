Fine-Tuning Detection Criteria for Enhancing Anomaly Detection in Time Series (FTDCTSAD)

Paper
- Sobrinho, E. P., Souza, J., Lima, J., Giusti, L., Bezerra, E., Coutinho, R., Baroni, L., Pacitti, E., Porto, F., Belloze, K., Ogasawara, E. Fine-Tuning Detection Criteria for Enhancing Anomaly Detection in Time Series. SBBD 2025. DOI: 10.5753/sbbd.2025.247063.

Goals
- Evaluate how tuning decision criteria in anomaly detection pipelines impacts performance (e.g., via distances, outlier functions, and group-check strategies).
- Compare multiple detectors (SVM, LSTM autoencoder, Conv1D, ELM) on labeled time-series datasets using standard classification metrics (F1, precision, recall, accuracy).
- Provide a clear, reproducible script with semantic step naming.

Folder Structure
- Scripts: [scripts/](scripts/)
  - [1-run-all-models.R](scripts/1-run-all-models.R): loads a benchmark dataset (e.g., `nab_sampleML`), runs multiple detectors under different criteria (distance/outlier/check), evaluates, and writes per-run results to `results/rds/`.
- Datasets: [datasets/](datasets/) — place input RData files here (e.g., `nab_sampleML.RData`).
- Results: [results/](results/) — generated outputs (per-run RDS files).
- License: [LICENSE](LICENSE)

Prerequisites
- R (>= 4.0 is recommended).
- Packages: harbinger, daltoolbox, plus common dependencies (e.g., stats). Install via:
  - `install.packages(c("harbinger", "daltoolbox"))`

How to Run
1) Place your dataset RData containing `nab_sampleML` under `datasets/`.
2) From the `ftdctsad` folder in R/RStudio, run:
   - `source("scripts/1-run-all-models.R")`
3) Check outputs under `results/rds/` (one RDS per configuration run).

Outputs
- Per-run RDS files containing confusion counts and derived metrics (accuracy, precision, recall, F1, balanced_accuracy), along with the selected criteria and model identifiers.

Notes
- The script searches `datasets/nab_sampleML.RData` first, then the current directory.
- If you have additional datasets (Yahoo, Numenta, GECCO Challenge), adapt the loader and loop similarly, keeping the semantic step organization.

License
- MIT License (see [LICENSE](LICENSE)).

