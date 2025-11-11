# STMotif Explorer — Spatiotemporal Motif Analysis

- Authors: Heraldo Borges, Antonio Castro, Rafaelli Coutinho, Fábio Porto, Esther Pacitti, Eduardo Ogasawara
- Category: Experiments / Tooling

---

## Overview
This folder documents the STMotif Explorer tool and experiments inspired by the paper:

Borges, H., Castro, A., Coutinho, R., Porto, F., Pacitti, E., Ogasawara, E. (2023). STMotif Explorer: A Tool for Spatiotemporal Motif Analysis. In: Extended Abstracts of the Brazilian Symposium on Databases (SBBD). https://doi.org/10.5753/sbbd_estendido.2023.233371

Objective: provide an interactive environment to discover, analyze, and visualize spatiotemporal motifs across domains, enabling algorithm registration, dataset registration, and comparative analysis with a canonical data structure (STMotifDS).

---

## What’s Here
- Images: demo animation and screenshot under `stmotifexplorer/img/`.
- License: `stmotifexplorer/LICENSE` (MIT).
- Legacy write-up: `stmotifexplorer/README.md` (original description; see below). This file (`readme.md`) is the canonical, up-to-date overview in English.

At the time of this update, no local application code (e.g., `main.R`) or notebooks are present in this folder. If you add them here, follow the experiment structure below and keep everything in English.

---

## Experiment Structure (proposed)
When adding experiments or reproductions for STMotif Explorer, use semantic numbering to make order and purpose explicit. Keep code, filenames, and comments in English with short, didactic explanations.

1-load_data: dataset registration and loading
- Scripts: `1-load_data.R` or `1-load_data.py`
- Tasks: load/ingest spatiotemporal datasets; define spatial layout/adjacency; validate schema; save canonical inputs.

2-preprocessing: cleaning and alignment
- Scripts: `2-preprocessing.R`
- Tasks: handle missing data; normalize; resample/align across sensors; select windows; persist cleaned datasets.

3-register_algorithms: plug-in functions
- Scripts: `3-register_algorithms.R`
- Tasks: register motif discovery and ranking functions compatible with STMotifDS; document parameters and signatures.

4-motif_discovery: run discovery
- Scripts: `4-motif_discovery.R`
- Tasks: execute discovery over datasets; enforce spatial and temporal constraints; output candidates in STMotifDS.

5-ranking: score and select
- Scripts: `5-ranking.R`
- Tasks: compute entropy, occurrence counts, proximity measures; select top-k motifs; export tables.

6-evaluation: comparison and metrics
- Scripts: `6-evaluation.R`
- Tasks: compare algorithms and settings; compute precision/recall on synthetic ground truth; summarize metrics.

7-reporting: figures and dashboards
- Scripts: `7-reporting.R` or `7-reporting.md`
- Tasks: generate figures; prepare app-ready assets; write short summaries.

Notes:
- If Jupyter notebooks are used, also export to Markdown (`.md`) with the same numbering (e.g., `4-motif_discovery.md`).
- Prefer small, single-responsibility scripts aligned to each numbered step.

---

## Methodology (summary)
- Canonical structure: STMotifDS captures motifs, occurrence positions, algorithm-specific information, and generic metadata.
- API concepts: register algorithms and rankers; register datasets and ground truth; run discovery; rank; compare results.
- Outputs: ranked motifs, comparison tables, and visualizations for qualitative analysis.

---

## Figures
- Demo (GIF): `stmotifexplorer/img/demo.gif`
- Demo (PNG): `stmotifexplorer/img/demo.png`

---

## Getting the App Code
This folder does not contain the full application source. See the linked repository or bring your local copy here if you plan to run or extend it:
- Repository: https://github.com/cefet-rj-dal/stmotifexplorer
- R environment: install R and RStudio (or VS Code + R extensions). Open `main.R` in the app repo and run. Default URL: `http://localhost:8000/`.

If you add the app’s source here, keep function and variable names in English and provide didactic comments describing each step.

---

## Related Markdown in This Folder
- Legacy overview: `stmotifexplorer/README.md` (older write-up kept for reference). This `readme.md` is the primary pointer and experiment guide.

---

## Citation
If you use or extend these experiments or the tool, please cite:

Borges, H., Castro, A., Coutinho, R., Porto, F., Pacitti, E., Ogasawara, E. (2023). STMotif Explorer: A Tool for Spatiotemporal Motif Analysis. In: Extended Abstracts of the Brazilian Symposium on Databases (SBBD). https://doi.org/10.5753/sbbd_estendido.2023.233371

