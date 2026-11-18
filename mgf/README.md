## Mixed Graph Framework (MGF)

### Objective
- Explore the complementarity of communication tools (e.g., email vs social) in learning platforms by modeling each tool as a network and analyzing combined effects on graph metrics. Based on:
- Carvalho, L., Assis, L., Lima, L., Bezerra, E., Guedes, G., Ziviani, A., Porto, F., Barbastefano, R., Ogasawara, E. [Evaluating the complementarity of communication tools for learning platforms](https://doi.org/10.5220/0006798701420153). CSEDU 2018 — Proceedings of the 10th International Conference on Computer Supported Education, 2018. DOI: 10.5220/0006798701420153.

### Code Layout
- Core helpers
  - `MGF.R`: Graph generation, weighting, transformations, and metrics.
  - `MGF-Datasets.R`: Scenario generation (email + social) and utilities (binning).
  - `MGF-Graphics.R`: Plot helpers for series, boxplots, and distributions.
- Experiments (semantic numbering)
  - `1-toy.R`: Minimal example to visualize networks and distributions.
  - `2-med-course.R`: Parameter sweep for a larger course setting.
  - `3-sma-participation.R`: Small course, participation sweep via social edges.
  - `4-sma-team-size.R`: Small course, varying number of groups (team size).
- RMarkdowns
  - `*.Rmd`: Narrative variants of some experiments; not required to run numbered scripts.

### Usage
- Run any experiment with Rscript from the repository root or inside `mgf/`:
  - `Rscript mgf/1-toy.R`
  - `Rscript mgf/2-med-course.R`
  - `Rscript mgf/3-sma-participation.R`
  - `Rscript mgf/4-sma-team-size.R`

### Notes
- No Jupyter notebooks are present; no conversions needed.
- Scripts were refactored with English, didactic headers and consistent naming.
