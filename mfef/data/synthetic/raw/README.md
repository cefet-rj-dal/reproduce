# Synthetic series (raw)

Frozen copy of the synthetic series used in the simulation study. These files
are the exact inputs consumed by the pipeline and must not be modified.

## Files

- `series.csv` — long-format series: one row per (series, time) with the
  observed value and the anomaly flag.
- `metadata.csv` — one row per series describing the experimental design
  (ground truth), used to evaluate detections.
- `series_list.RDS` — the same series as an R list object.

## `metadata.csv` (ground truth)

Documented here because the metadata links each series to its known
change-point, which is required to compute the false positive rate (FPR) and
detection accuracy.

- `series_id` — numeric series identifier.
- `series_id_name` — descriptive name encoding the design factors:
  `<regime>_<data_type>_<cp_position>_<magnitude>_<ar1>_rep_<id>`, where
    - `regime` ∈ {mean, trend, variance, null} — type of structural change
      (mean-shift, trend-shift, variance-shift, or no change).
    - `data_type` ∈ {mortality, hospitalization} — calibration target.
    - `cp_position` — change-point position (e.g. `4q` = fourth quantile of the
      series length).
    - `magnitude` ∈ {high_high, high_low, low_high, low_low} — change magnitude
      levels (signal-to-noise combinations).
    - `ar1` ∈ {with_ar1, no_ar1} — presence or absence of first-order
      autoregressive dependence.
    - `rep_<id>` — replicate index within the scenario.
- additional generator parameters as produced by the generator.

## Provenance

Produced by the synthetic-series generator (Python), available at:

- Generator repository: https://github.com/isadpr/synthetic-series-generation

The generator and the full experimental design are documented in that
repository. The files in this folder are a frozen copy corresponding to a fixed
release/commit of the generator, versioned here so the simulation study can be
reproduced without re-running the generator.