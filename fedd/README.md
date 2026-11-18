FEDD: Fuzzy Ensemble for Drifter Detection (3W Oil Wells)

This folder contains scripts and data pointers to reproduce experiments on change/drift detection (drifters) over 3W oil well time series, with evaluation using SoftED-style metrics and ensemble strategies.

Objectives
- Run base drifter detection experiments for selected wells and configurations.
- Aggregate results to compare detectors and combinations across examples.
- Build fuzzy/majority-vote ensembles and visualize detections versus events.

Structure
- Core scripts: [core/](core/)
  - [1-run-examples.R](core/1-run-examples.R): Runs example experiments over a selected well using combinations provided by external helpers; writes CSV results to a results directory.
  - [2-analyze-results.R](core/2-analyze-results.R): Loads per-combination CSVs and computes SoftED metrics per example; aggregates and ranks configurations.
  - [3-combination-analysis.R](core/3-combination-analysis.R): Broader combination analysis and non-parametric tests (Kruskal–Wallis, Wilcoxon pairwise).
  - [4-ensemble-fuzzy.R](core/4-ensemble-fuzzy.R): Fuzzy ensemble of top drifters with MV/TV variants; emits combined detection CSVs.
- Plot example drifters: [5-plot-example-drifters.R](5-plot-example-drifters.R)
- Experiments: [experiments/](experiments/) — per-example drivers grouped by event type and well IDs (e.g., `1/1WELL1.R`, `2/2WELL3_1.R`).
- Data: [data/](data/) — RData artifacts for the 3W dataset used by the scripts.
- Sample results: [results.zip](results.zip) — unzip to inspect.

How to Run (high-level)
1) Ensure required packages are available: `dalevents`, `daltoolbox`, `harbinger`, `devtools`, `ggplot2`, `dplyr`, `tidyr`, `pROC`, `caret`, `data.table`, `ggpubr`.
2) Adjust local paths in the scripts (marked with comments) for:
   - Development libraries: `load_all('/home/...')`
   - Source helpers: `source('/home/.../core.R')`
   - Data and results directories: `data_path`, `folder_path`, `results_path`.
3) Run end-to-end:
   - Execute per-example scripts under `experiments/<type>/` to generate CSVs, or use `core/1-run-examples.R` for quick runs.
   - Analyze and rank configurations with `core/2-analyze-results.R` or `core/3-combination-analysis.R`.
   - Create ensemble detections with `core/4-ensemble-fuzzy.R`.
   - Visualize detections vs. events with `5-plot-example-drifters.R`.

Notes
- All scripts use paths relative to this folder (e.g., `data/`, `results/`). If you keep helper sources, place them under `core/` and adjust the commented `source(...)` lines.
- The `experiments/` subfolders hold multiple example runners; they already follow a numeric grouping by event type. The numbered filenames in `core/` provide a semantic step order for the overall workflow.

Citation
- Please refer to the related drifter/SoftED publications for methodological details when reporting results.

