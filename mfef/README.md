# MFEF: confidence-weighted detection of structural changes in epidemiological time series

Code, data, and result artifacts for the synthetic benchmark and real-data application of the **Multilevel Fuzzy Ensemble Framework (MFEF)**.

MFEF integrates heterogeneous change-point detectors into confidence-weighted evidence of structural change. The framework combines classical statistical, Bayesian online, machine-learning, and deep-learning methods and is evaluated against optimized Joinpoint regression configurations. The MFEF consensus mechanism is implemented in the **Harbinger** R package.

## Repository structure

```text
mfef/
├── scripts/
│   ├── 01-prep-synthetic.R
│   ├── 02-run-mfef-synthetic.R
│   ├── 03-analyze-synthetic.R
│   └── 04-analyze-real.R
│
├── data/
│   ├── synthetic/
│   │   ├── raw/
│   │   │   └── README.md
│   │   └── processed/
│   └── real/
│       └── raw/
│           ├── txs_mortalidade_alcool.csv
│           ├── txs_internacao_alcool.csv
│           ├── txs_mortalidade_humor.csv
│           └── txs_internacao_humor.csv
│
├── outputs/
│   └── synthetic/
│       └── detections/
│           ├── batch_mfef_joinpoint_hybrid_soft_hard_id_0_850_sw_6_thrgrid_0p90_0p95.csv
│           └── batch_mfef_joinpoint_hybrid_soft_hard_id_851_1679_sw_6_thrgrid_0p90_0p95.csv
│
├── results/
│   ├── synthetic/
│   │   └── *.rds
│   └── real/
│       ├── *.rds
│
├── figures/
│   ├── synthetic/
│   │   └── *.pdf
│   └── real/
│       └── *.pdf
│
├── renv.lock
├── .Rprofile
├── .gitignore
└── README.md
```

The scripts have the following responsibilities:

- `01-prep-synthetic.R` prepares the raw synthetic series for change-point detection.
- `02-run-mfef-synthetic.R` executes the MFEF and Joinpoint configurations and saves the synthetic detection results.
- `03-analyze-synthetic.R` reads the complete detection results, performs the synthetic statistical analysis, and saves the corresponding tables and figures.
- `04-analyze-real.R` executes the four real-data applications and saves the complete results, derived tables, and figures.

## Requirements

### R environment

The R environment is managed with `renv`. Restore the package versions recorded in `renv.lock` with:

```r
install.packages("renv")
renv::restore()
```

### Python environment

The detection scripts use Python through `reticulate`. The real-data analysis requests Python 3.12 and the required versions of:

```text
numpy
pandas
scikit-learn
torch
```

The Python requirements for the real-data analysis are declared in `scripts/04-analyze-real.R` through `reticulate::py_require()`.

Python is a system-level dependency and is not managed by `renv.lock`.

On macOS, the real-data script enables an OpenMP compatibility workaround and restricts numerical backends to one thread.

### Java environment

Some detection components may require Java. Java 21 can be configured from R with:

```r
rJavaEnv::java_quick_install(21)
```

Java is a system-level dependency and is not managed by `renv.lock`.

## Reproducing the validated analyses

The synthetic and real-data analyses can be executed independently.

### Synthetic benchmark

Run:

```r
source("scripts/03-analyze-synthetic.R")
```

The script reads the two complete detection batches:

```text
outputs/synthetic/detections/batch_mfef_joinpoint_hybrid_soft_hard_id_0_850_sw_6_thrgrid_0p90_0p95.csv

outputs/synthetic/detections/batch_mfef_joinpoint_hybrid_soft_hard_id_851_1679_sw_6_thrgrid_0p90_0p95.csv
```

Together, the files cover synthetic series IDs 0–1679. After validating the predefined exclusions, the analysis retains 1,671 valid series evaluated under 20 detector configurations.

The analysis includes hard- and soft-F1 summaries, parameter-matched contrasts, Friedman and Nemenyi analyses, paired Wilcoxon tests with Bonferroni correction, bootstrap intervals with 1,000 replicates, and false-detection analyses restricted to the 240 null series.

The outputs are saved in:

```text
results/synthetic/
figures/synthetic/
```

### Real-data application

Run:

```r
source("scripts/04-analyze-real.R")
```

The script executes the following four applications without manual reconfiguration:

```text
Alcohol-attributable mortality
Alcohol-attributable hospitalization
Mood-disorder mortality
Mood-disorder hospitalization
```

All four series are analyzed with the fixed shortlisted configuration:

```text
MFEF+JP(F,12,0.95)
```

The analysis estimates MFEF+JP and internal Joinpoint detections, segment means and variances, annual percentage change, average annual percentage change, and block-bootstrap intervals with 1,000 replicates.

The outputs are saved in:

```text
results/real/
figures/real/
```

For each series, three PDF files are generated:

```text
<series>_detectors.pdf
<series>_score.pdf
<series>_heatmap.pdf
```

The script verifies the existence and size of the six expected RDS files and 12 expected PDF files.

## Regenerating the synthetic detections

The committed detection batches are sufficient to reproduce the validated statistical analysis with `03-analyze-synthetic.R`.

To regenerate the detector-level synthetic results, run:

```r
source("scripts/01-prep-synthetic.R")
source("scripts/02-run-mfef-synthetic.R")
```

Synthetic detection is computationally intensive. The batch parameters in `02-run-mfef-synthetic.R` determine the subset of series processed in each execution.

After generating the two required batches, run:

```r
source("scripts/03-analyze-synthetic.R")
```

The complete script order is:

```r
source("scripts/01-prep-synthetic.R")
source("scripts/02-run-mfef-synthetic.R")
source("scripts/03-analyze-synthetic.R")
source("scripts/04-analyze-real.R")
```

Scripts `01` and `02` prepare and process the synthetic series. Script `03` performs the complete synthetic statistical analysis. Script `04` independently executes the four real-data applications.

## Data

### Synthetic data

The synthetic series are stored in:

```text
data/synthetic/raw/
data/synthetic/processed/
```

The local README in `data/synthetic/raw/` describes how the raw synthetic series were generated.

The detector-level results used by the validated analysis are stored in:

```text
outputs/synthetic/detections/
```

### Real data

The real-data application uses refined monthly Brazilian mortality and hospitalization indicators supplied by the **Digital Public Mental Health** project at ICICT/Fiocruz.

The input files are:

```text
data/real/raw/txs_mortalidade_alcool.csv
data/real/raw/txs_internacao_alcool.csv
data/real/raw/txs_mortalidade_humor.csv
data/real/raw/txs_internacao_humor.csv
```

Mortality data originate from the Mortality Information System (SIM), hospitalization data originate from the Hospital Information System of the Brazilian Unified Health System (SIH/SUS), and population denominators are provided by the Brazilian Institute of Geography and Statistics (IBGE).

The fully alcohol-attributable indicators follow the definitions documented by the Digital Public Mental Health project and Brazilian Ministry of Health Technical Note No. 44/2022.

The mood-disorder indicators comprise ICD-10 codes F30, F31, F32, F33, F34, and F39.

The analysis selects national age-standardized rates. The rates were standardized by the direct method using the 2010 Brazilian population as the reference population.

The hospitalization series cover January 2009 through December 2023. The mortality series cover January 2000 through December 2023.

## Output directories

```text
outputs/synthetic/detections/
```

Detector-level synthetic results used as input by the statistical analysis.

```text
results/synthetic/
```

Synthetic statistical summaries and table objects in RDS format.

```text
results/real/
```

Complete and derived real-data result objects in RDS format.

```text
figures/synthetic/
```

Synthetic-analysis figures in PDF format.

```text
figures/real/
```

Detector, ensemble-score, and fuzzy-score figures for the four real-data series.

## License

Code is distributed under the MIT License.

Data and generated figures are distributed under the CC BY 4.0 License, subject to the provenance and reuse conditions of the original data providers.