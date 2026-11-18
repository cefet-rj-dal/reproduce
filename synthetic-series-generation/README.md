# Synthetic Series — Ensemble Fuzzy / Mental Health

Generates synthetic monthly time series for benchmarking changepoint detection algorithms (specifically the "Ensemble Fuzzy" method) applied to Brazilian mental health surveillance data (SUS). The design replicates Noé et al. (2025).

---

## Dependencies

```bash
pip install timeeval-gutenTAG pandas numpy scipy scikit-learn pyreadr matplotlib
```

## Running

```bash
python generate_synthetic_series.py
```

Output is written to `synthetic_series/`:

- `metadata.csv` / `metadata.RData` — one row per series, all scenario parameters as columns
- `series.csv` / `series.RData` — long format: `(series_id, time, value, is_anomaly)`

Load in R:

```r
meta   <- read.csv("synthetic_series/metadata.csv")
series <- read.csv("synthetic_series/series.csv")
```

Both files are linked by `series_id`.

---

## Series Generator: GutenTAG

Synthetic series are generated with the GutenTAG library (Wenig et al., 2022), which composes time series with controllable seasonality, trend, and anomalies. The generator produces a `value` column and an `is_anomaly` column marking all time points in the segment following each changepoint.

### Off-by-one bug fix (monkey-patch)

GutenTAG has an off-by-one bug in its `trend` anomaly: `int(N * 0.2) + int(N * 0.8)` is not always equal to N (e.g. N=143 yields 28+114=142). The fix computes `transition_length = length - plateau_length` instead. This is applied as a monkey-patch to `AnomalyTrend.generate` at import time via `patch_gutentag.py`.

---

## Experimental Design

### Null scenarios (no changepoint)

Series with no structural breaks, used to estimate false positive rates. Generated for all combinations of series type, noise, and autocorrelation:

**2 series types × 2 noise levels × 2 AR(1) × 30 replicates = 240 series**

### Change scenarios

**2 noise levels × 2 AR(1) × 2 magnitudes × 3 change types × 2 series types × 30 replicates = 1,440 series**

**Total: 1,680 series**

---

## Parameters

### Series lengths

| Series type     | Length    | Period covered            |
|-----------------|-----------|---------------------------|
| mortality       | 288 months | Jan/2000–Dec/2024        |
| hospitalization | 180 months | Jan/2009–Dec/2024        |

### Replicates

**30 replicates** per scenario. Break positions are resampled per replicate using a deterministic seed derived from a SHA-256 hash of the scenario name, ensuring full reproducibility across runs.

### Change types

- **mean**: abrupt shift in the series mean at the changepoint. Implemented as a GutenTAG `mean` anomaly with `offset`.
- **trend**: change in the slope of the linear trend at the changepoint. Implemented as a GutenTAG `trend` anomaly with a degree-1 polynomial.
- **variance**: change in noise variance at the changepoint. Implemented as a GutenTAG `variance` anomaly.

### Number and position of changepoints

Each change series has 1–5 breakpoints (sampled uniformly per replicate), with a minimum spacing of **12 months** between consecutive breaks and from the series edges. Signs alternate (+, −, +, …) at successive breaks to prevent indefinite drift.

For variance anomalies, each break replaces the noise variance of the remaining segment (they do not stack).

### Magnitudes

Magnitudes are defined as multiples of each series type's estimated noise, calibrated from real SUS data:

| Series type     | Estimated noise |
|-----------------|-----------------|
| hospitalization | 0.278           |
| mortality       | 0.021           |

| Label | Multiple | SNR  |
|-------|----------|------|
| low   | 0.5×     | 0.5  |
| high  | 2.0×     | 2.0  |

SNR = magnitude / noise. For variance anomalies, the multiple is applied directly as a factor (not scaled by noise).

### Noise levels

Gaussian white noise N(0, variance) added by GutenTAG:

| Label | Variance |
|-------|----------|
| low   | 0.002    |
| high  | 0.006    |

All values are in normalized [0, 1] space.

### AR(1) autocorrelation

Real SUS mental health series exhibit strong lag-1 autocorrelation. After GutenTAG generation, white noise can be replaced by AR(1) noise:

```
noise[t] = rho * noise[t-1] + innovation[t]
```

where `innovation[t] ~ N(0, sigma * sqrt(1 − rho²))`, preserving the marginal variance.

| Label    | rho | Description                        |
|----------|-----|------------------------------------|
| no_ar1   | 0.0 | White noise (GutenTAG default)     |
| with_ar1 | 0.8 | Calibrated from real SUS series    |

### Base signal

Each series includes:

- Sinusoidal oscillation with period 12 months (frequency = 1/12) and amplitude 0.5.
- Mild linear trend (coefficient 0.002/month).

---

## Output Schema

### metadata.csv

One row per series.

| Column                | Description                                              |
|-----------------------|----------------------------------------------------------|
| series_id             | Unique numeric index                                     |
| series_id_name        | Descriptive name encoding all scenario parameters        |
| replicate             | Replicate number (0–29)                                  |
| series_type           | `"mortality"` or `"hospitalization"`                     |
| length                | Number of time points                                    |
| anomaly_type          | `"mean"`, `"trend"`, `"variance"`, or `"none"`           |
| n_breaks              | Number of changepoints (0 for null scenarios)            |
| breakpoint_positions  | Semicolon-separated month indices of breaks (empty if 0) |
| magnitude_label       | `"low"`, `"high"`, or `"none"`                           |
| magnitude_value       | Numeric magnitude value (null for null scenarios)        |
| noise_label           | `"low"` or `"high"`                                      |
| variance              | Gaussian noise variance value                            |
| autocorrelation_label | `"no_ar1"` or `"with_ar1"`                               |
| autocorrelation_rho   | rho value (0.0 or 0.8)                                   |
| sparse_series         | Always `False` (not used in this version)                |

### series.csv

One row per time point.

| Column     | Description                                         |
|------------|-----------------------------------------------------|
| series_id  | Links to metadata.csv                               |
| time       | Month index (0, 1, 2, …)                            |
| value      | Simulated series value (clipped to minimum 1e-6)    |
| is_anomaly | 1 if the point is after a changepoint, 0 otherwise  |

---

## References

- Noé, L. et al. (2025). Evaluation of Methods for Joinpoint Analysis of Time Series Using Simulated and Real-World Data. *Journal of Clinical Epidemiology*, 187, 111966.

- Wenig, P. et al. (2022). TimeEval: A Benchmarking Toolkit for Time Series Anomaly Detection Algorithms. *VLDB Endowment*, 15(12).
