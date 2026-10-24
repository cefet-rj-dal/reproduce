# biostat — MFEF for confidence-weighted detection of structural changes in epidemiological time series

Code and data to reproduce the simulation study and the real-data application of the **Multilevel Fuzzy Ensemble Framework (MFEF)**, which integrates heterogeneous change-point detectors (classical statistical, Bayesian online, and machine/deep-learning) into confidence-weighted evidence of structural change, benchmarked against an optimized Joinpoint regression. The MFEF consensus mechanism is distributed as part of the **Harbinger** R package (CRAN).

## Repository structure

```
biostat/
├── scripts/
│   ├── 01-prep-synthetic.R      # raw synthetic CSV -> nested RDS
│   ├── 02-run-mfef-synthetic.R  # MFEF + Joinpoint over synthetic series
│   ├── 03-analyze-synthetic.R   # simulation analysis, tests and figures
│   └── 04-analyze-real.R        # real-data application
├── data/
│   ├── synthetic/{raw,processed}/
│   └── real/raw/
├── outputs/
│   ├── synthetic/{detections,figures}/
│   └── real/figures/
├── renv.lock
├── .Rprofile
└── .gitignore
```

## Reproduce

**1. System dependencies (outside R):** Java 21 (`rJavaEnv::java_quick_install(21)`) and Python 3.11 (64-bit) with `torch` (CPU), `scikit-learn`, `numpy`, `pandas`. Not captured by `renv.lock`. The generation steps (`02`, `04`) need this toolchain (run on Linux); the analysis step (`03`) is portable.

**2. R environment:**

``` r
install.packages("renv")
renv::restore()
```

**3. Run order:**

``` r
source("scripts/01-prep-synthetic.R")
source("scripts/02-run-mfef-synthetic.R")
source("scripts/03-analyze-synthetic.R")
source("scripts/04-analyze-real.R")
```

Set the batch range in `02` to cover the full set of synthetic series before the final run.

## Data availability

- **Synthetic series** — `data/synthetic/raw/` (see local README); generator available separately.
- **Real data** — Brazilian mortality and hospitalization indicators from SUS administrative data, provided by the Saude Mental Publica Digital project (ICICT/Fiocruz); see `data/real/raw/README.md`.

## License

MIT (code); CC-BY 4.0 (data and figures).