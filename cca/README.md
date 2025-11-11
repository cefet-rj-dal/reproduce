CCA: Contextual–Compositional Associations for NMR Monitoring

This folder contains code and notebooks related to the paper:

Baroni, L. R., Scoralick, L., Reis, A., Belloze, K. T., Pedroso, M. D. M., Alves, R. F. S., Boccolini, C. S., De Moraes Mello Boccolini, P. M., Ogasawara, E. S. A contextual-compositional approach to discover associations between health determinants and health indicators for neonatal mortality rate monitoring in situations of anomalies. PLOS ONE, 2024. DOI: 10.1371/journal.pone.0310413.

Goals
- Monitor neonatal mortality rate (NMR) in the presence of anomalies by discovering contextual–compositional associations between health determinants and indicators.
- Group health facilities by characteristic NMR patterns and use these groups in downstream analyses.
- Support exploratory analysis and reproducible steps via scripts and notebooks.

Folder and Files
- [acc.Rproj](acc.Rproj): R project file for this analysis workspace.
- [1-classify-cnes-by-nmr-zeros.R](1-classify-cnes-by-nmr-zeros.R): Classifies CNES maternity facilities by proportion of months with zero NMR using k-means (k=3), relabels clusters so 1 has the highest zero-rate and 3 the lowest, and plots example series with anomaly markers.
- [Notebook-acc.md](Notebook-acc.md): Markdown companion export of the main notebook (code-focused, outputs omitted).
- [Notebook.md](Notebook.md): Markdown companion export of the exploratory notebook (code-focused, outputs omitted).
- [df.RData](df.RData), [df_3cat.RData](df_3cat.RData): Prepared data objects used by the notebooks/analysis.
- [LICENSE](LICENSE): License file for this subproject.

How to Run the Classification Script
1) Open R (or RStudio) in this folder.
2) Ensure an `rmrj` object is loaded in your session with at least the columns: `local` (facility id), `time` (Date), `tmn` (NMR). Optional columns for anomaly markers: `anom.p`, `anom.t`, `anom.c`.
   - Example: `load("path/to/rmRJ.RData"); stopifnot(exists("rmrj"))`
3) Run the script:
   - `source("1-classify-cnes-by-nmr-zeros.R")`

Script Output
- Prints the distribution of cluster labels (1..3).
- Data frame `cnes_cat` (constructed in-script) with cluster assignment per facility, mergeable back to the original time series by `local`.
- Example plots: per-category example series and per-facility series for one category with anomaly markers (if present).

Notebooks
- Prefer a text view? See the Markdown companions [Notebook-acc.md](Notebook-acc.md) and [Notebook.md](Notebook.md). Original `.ipynb` notebooks may be opened in Jupyter/VS Code if present in your workspace.

Step Order (Experimental Procedure)
- 1) Classify facilities by NMR zero-rate: [1-classify-cnes-by-nmr-zeros.R](1-classify-cnes-by-nmr-zeros.R)
- 2) Explore associations and context: Notebook (or its Markdown companion)

Prerequisites
- R (>= 4.x recommended).
- R packages: ggplot2, dplyr, reshape2, stats.
- Optional: Jupyter with an R kernel (or Python) to open the `.ipynb` notebooks.

Citation
If you use this material, please cite the PLOS ONE paper above (DOI: 10.1371/journal.pone.0310413).

