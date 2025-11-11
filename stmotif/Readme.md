# STMotif — Spatio-Temporal Motifs Discovery

- Authors: Heraldo Borges, Murillo Dutra, Amin Bazaz, Rafaelli Coutinho, Fábio Perosi, Fábio Porto, Florent Masseglia, Esther Pacitti, Eduardo Ogasawara
- Category: Experiments

---

## Overview
This folder documents experiments and materials related to the paper:

Borges, H., Dutra, M., Bazaz, A., Coutinho, R., Perosi, F., Porto, F., Masseglia, F., Pacitti, E., Ogasawara, E. (2020). Spatial-time motifs discovery. Intelligent Data Analysis. https://doi.org/10.3233/IDA-194759

Objective: discover and rank motifs in spatio-temporal series by combining local time series into “combined series” blocks and applying a hash-based motif discovery algorithm that honors both temporal and spatial constraints.

---

## Experiment Structure (proposed)
If/when code is added to this folder, follow the semantic numbering below so the execution order and goals are explicit. Keep code, filenames, and comments in English, and add short, didactic comments explaining each step.

1-load_data: data acquisition and raw loading
- Scripts: `1-load_data.py` (or `1-load_data.md` if documented-only)
- Tasks: load spatial-temporal datasets (synthetic, seismic, or others); define spatial grid/adjacency; check integrity.

2-preprocessing: cleaning and alignment
- Scripts: `2-preprocessing.py`
- Tasks: handle missing data, normalization, resampling/alignment across sensors; define block partitioning parameters.

3-combined_series: block construction and combination
- Scripts: `3-combined_series.py`
- Tasks: partition into blocks; build combined series from subsequences that capture spatial and temporal locality; persist versions.

4-motif_discovery: hash-based motif discovery
- Scripts: `4-motif_discovery.py`
- Tasks: apply the discovery algorithm over combined series; enforce temporal and spatial constraints; deduplicate candidates.

5-ranking: motif scoring and selection
- Scripts: `5-ranking.py`
- Tasks: compute entropy-based scores, occurrences count, and proximity measures; rank motifs; produce tables.

6-evaluation: metrics and comparisons
- Scripts: `6-evaluation.py`
- Tasks: compare CSA against time-series-only baselines; compute precision/recall over synthetic ground-truth; summarize results.

7-reporting: figures and tables
- Scripts: `7-reporting.py` or `7-reporting.md`
- Tasks: generate publication-quality figures; export to `figures/` and `reports/`.

Notes:
- If Jupyter notebooks are present, convert them to Markdown (`.md`) with the same numbering (e.g., `4-motif_discovery.md`).
- Prefer small, single-responsibility scripts aligned to each numbered step.

---

## Methodology (summary)
- Partition spatio-temporal series into blocks to preserve locality.
- Build combined series from subsequences inside each block to expose joint spatial-temporal structure.
- Run a hash-based motif discovery algorithm over combined series.
- Validate motifs using temporal and spatial constraints; rank them by entropy, occurrence count, and proximity.

---

## Figures
- Synthetic dataset example: `stmotif/figure1.png`
- CSA approach illustration: `stmotif/figure2.png`
- Top motifs (CSA ranking): `stmotif/figure3.png`
- Top motifs (occurrences): `stmotif/figure4.png`
- Additional figure: `stmotif/figure5.png`

---

## Code Availability
- CRAN package: https://CRAN.R-project.org/package=STMotif
- GitHub (CSA reference): https://github.com/eogasawara/CSA

At the time of this README, no local experiment scripts or notebooks are present under `stmotif/`. If you add them here, please follow the numbering scheme above and keep everything in English.

---

## Related Markdown in This Folder
- Legacy post: `stmotif/Readme.md` (older write-up; kept for reference). This file (lowercase `readme.md`) is the canonical, up-to-date overview.

---

## How to Contribute or Extend
- Place new scripts in this folder using the numbering scheme.
- Use English for code, filenames, function/variable names, and comments. Add short, didactic comments to clarify intent.
- If you add notebooks (`.ipynb`), also export to Markdown with the same filename and number.

---

## Citation
If you use or extend these experiments, please cite the paper:

Borges, H., Dutra, M., Bazaz, A., Coutinho, R., Perosi, F., Porto, F., Masseglia, F., Pacitti, E., Ogasawara, E. (2020). Spatial-time motifs discovery. Intelligent Data Analysis. https://doi.org/10.3233/IDA-194759

