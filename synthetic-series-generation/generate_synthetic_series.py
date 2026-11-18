"""
Synthetic series generation for changepoint detection benchmarking.

Usage:
    python generate_synthetic_series.py
    from generate_synthetic_series import main, PARAMETERS
    main({**PARAMETERS, "n_replicates": 2, "output_dir": "test"})
"""

import os
import hashlib
os.environ["TQDM_DISABLE"] = "1"

import pandas as pd
import numpy as np
import pyreadr
from gutenTAG import GutenTAG

import patch_gutentag
from config import PARAMETERS

MINIMUM_VALUE = 1e-6


def magnitudes_for_type(series_type, noise_by_type, magnitude_multiples):
    """Absolute change magnitudes for a series type, scaled by its estimated noise."""
    noise = noise_by_type[series_type]
    return {label: round(mult * noise, 6) for label, mult in magnitude_multiples.items()}


def sample_positions(length, n_breaks, min_spacing, rng):
    """Sample breakpoint positions with guaranteed minimum spacing from edges and each other.

    Uses the bijection q_i = p_i - (i+1)*min_spacing to uniformly sample over
    valid configurations without rejection sampling.
    """
    N = length - (n_breaks + 1) * min_spacing
    if N < n_breaks:
        raise ValueError(
            f"Series of {length} months is too short for {n_breaks} breaks "
            f"with spacing {min_spacing}"
        )
    qs = sorted(rng.choice(N, size=n_breaks, replace=False))
    return [int(q + (i + 1) * min_spacing) for i, q in enumerate(qs)]


def apply_ar1_noise(values, rho, seed=None):
    """Replace white noise with AR(1) noise while preserving the original std."""
    rng    = np.random.default_rng(seed)
    values = np.array(values, dtype=float)
    n      = len(values)

    std_original = np.std(np.diff(values)) / np.sqrt(2)
    if rho == 0.0 or std_original == 0.0:
        return values

    std_innovation = std_original * np.sqrt(1 - rho ** 2)
    ar1_noise      = np.zeros(n)
    ar1_noise[0]   = rng.normal(0, std_original)
    for i in range(1, n):
        ar1_noise[i] = rho * ar1_noise[i - 1] + rng.normal(0, std_innovation)

    clean_signal = values - rng.normal(0, std_original, n)
    return clean_signal + ar1_noise


def _base_oscillation(variance):
    # 12-month sine (annual seasonality) + linear trend of +0.002/month
    return {
        "kind"     : "sine",
        "frequency": 1.0 / 12.0,
        "amplitude": 0.5,
        "variance" : variance,
        "trend"    : {"kind": "polynomial", "polynomial": [0, 0.002]},
    }


def null_scenario(name, length, variance):
    return {
        "name"             : name,
        "length"           : length,
        "base-oscillations": [_base_oscillation(variance)],
        "anomalies"        : [],
    }


def changepoint_scenario(name, length, positions, magnitude, variance, change_type):
    """Build a GutenTAG config with structural breaks at the given positions.

    Sign alternates (+, -, +, ...) at each break to prevent indefinite drift
    for mean/trend types. Variance anomalies multiply/divide by (1+magnitude)
    symmetrically on the log scale and replace (not stack) the noise.
    """
    anomalies = []
    for i, pos in enumerate(positions):
        sign           = 1 if i % 2 == 0 else -1
        segment_length = length - pos

        if change_type == "mean":
            params = {"kind": "mean", "offset": sign * magnitude}

        elif change_type == "trend":
            params = {
                "kind"       : "trend",
                "oscillation": {"kind": "polynomial", "polynomial": [0, sign * magnitude]},
            }

        elif change_type == "variance":
            factor = (1 + magnitude) if i % 2 == 0 else 1 / (1 + magnitude)
            params = {"kind": "variance", "variance": variance * factor}

        else:
            raise ValueError(f"Unsupported change type: {change_type}")

        anomalies.append({"exact-position": pos, "length": segment_length, "kinds": [params]})

    return {
        "name"             : name,
        "length"           : length,
        "base-oscillations": [_base_oscillation(variance)],
        "anomalies"        : anomalies,
    }


def generate_series(config_ts, seed, rho=0.0):
    """Run GutenTAG and return a DataFrame with series values and anomaly labels."""
    gt = GutenTAG(seed=seed)
    gt.load_config_dict({"timeseries": [config_ts]})
    df = gt.generate(return_timeseries=True)[0].timeseries

    col = next(c for c in df.columns if c.startswith("value"))
    if rho > 0.0:
        df[col] = apply_ar1_noise(df[col].values, rho, seed=seed)
    df[col] = np.maximum(df[col], MINIMUM_VALUE)
    return df


def _record(records, df, replicate, series_type, length,
            anomaly_type, n_breaks, breakpoint_positions,
            magnitude_label, magnitude_value,
            noise_label, variance,
            autocorrelation_label, autocorrelation_rho,
            sparse, name):
    col = next(c for c in df.columns if c.startswith("value"))
    records.append({
        "replicate"            : replicate,
        "series_type"          : series_type,
        "length"               : length,
        "anomaly_type"         : anomaly_type,
        "n_breaks"             : n_breaks,
        "breakpoint_positions" : breakpoint_positions,
        "magnitude_label"      : magnitude_label,
        "magnitude_value"      : magnitude_value,
        "noise_label"          : noise_label,
        "variance"             : variance,
        "autocorrelation_label": autocorrelation_label,
        "autocorrelation_rho"  : autocorrelation_rho,
        "sparse_series"        : sparse,
        "series_id_name"       : name,
        "values"               : df[col].tolist(),
        "labels"               : df["is_anomaly"].tolist(),
    })


def _log_progress(counter, total, interval=200):
    if counter % interval == 0 or counter == total:
        print(f"  {counter:>6,}/{total:,}  ({100 * counter / total:.1f}%)")


def run_simulation(cfg):
    """Generate all synthetic series for the factorial experiment.

    Null:   2 noise × 2 autocorr × 2 series types × 30 reps = 240
    Change: 2 noise × 2 autocorr × 2 magnitudes × 3 types × 2 series × 30 reps = 1440
    Total:  1680

    Break positions are deterministic per scenario via SHA-256 hash of the scenario name.
    """
    os.makedirs(cfg["output_dir"], exist_ok=True)
    records = []

    n_null   = (len(cfg["noise_levels"]) * len(cfg["autocorrelations"])
                * len(cfg["lengths"]) * cfg["n_replicates"])
    n_change = (len(cfg["noise_levels"]) * len(cfg["autocorrelations"])
                * len(cfg["magnitude_multiples"]) * len(cfg["change_types"])
                * len(cfg["lengths"]) * cfg["n_replicates"])
    total    = n_null + n_change

    print(f"Generating {total:,} series  (null: {n_null:,}  with change: {n_change:,})\n")
    counter = 0

    for noise_name, var in cfg["noise_levels"].items():
        for ac_name, rho in cfg["autocorrelations"].items():

            for replicate in range(cfg["n_replicates"]):
                seed = replicate
                for series_type, length in cfg["lengths"].items():
                    name = f"null_{series_type}_{ac_name}_{noise_name}_rep_{replicate:04d}"
                    try:
                        df = generate_series(null_scenario(name, length, var), seed, rho=rho)
                        _record(records, df, replicate, series_type, length,
                                "none", 0, [], "none", None,
                                noise_name, var, ac_name, rho, False, name)
                    except Exception as e:
                        print(f"  ERROR [{name}]: {e}")
                    counter += 1
                    _log_progress(counter, total)

            for mag_name in cfg["magnitude_multiples"]:
                for replicate in range(cfg["n_replicates"]):
                    seed = replicate
                    for series_type, length in cfg["lengths"].items():
                        magnitudes = magnitudes_for_type(
                            series_type, cfg["noise_by_type"], cfg["magnitude_multiples"]
                        )
                        for change_type in cfg["change_types"]:
                            series_seed = int(
                                hashlib.sha256(
                                    f"{noise_name}_{ac_name}_{mag_name}_{replicate}"
                                    f"_{change_type}_{series_type}".encode()
                                ).hexdigest()[:8], 16
                            )
                            series_rng = np.random.default_rng(series_seed)
                            n_breaks   = int(series_rng.integers(1, 6))
                            positions  = sample_positions(
                                length, n_breaks, cfg["min_spacing"], series_rng
                            )
                            mag = (cfg["magnitude_multiples"][mag_name]
                                   if change_type == "variance"
                                   else magnitudes[mag_name])
                            name = (
                                f"{change_type}_{series_type}_{n_breaks}q_{mag_name}"
                                f"_{noise_name}_{ac_name}_rep_{replicate:04d}"
                            )
                            try:
                                config = changepoint_scenario(
                                    name, length, positions, mag, var, change_type
                                )
                                df = generate_series(config, seed, rho=rho)
                                _record(records, df, replicate, series_type,
                                        length, change_type, n_breaks, positions,
                                        mag_name, mag, noise_name, var,
                                        ac_name, rho, False, name)
                            except Exception as e:
                                print(f"  ERROR [{name}]: {e}")
                            counter += 1
                            _log_progress(counter, total)

    return records


def save_results(records, output_dir):
    """Write metadata and series to CSV and RData inside output_dir.

    Outputs:
        metadata.csv / metadata.RData  — one row per series
        series.csv   / series.RData    — long format (series_id, time, value, is_anomaly)
    """
    metadata = []
    for i, r in enumerate(records):
        metadata.append({
            "series_id"            : i,
            "series_id_name"       : r["series_id_name"],
            "replicate"            : r["replicate"],
            "series_type"          : r["series_type"],
            "length"               : r["length"],
            "anomaly_type"         : r["anomaly_type"],
            "n_breaks"             : r["n_breaks"],
            "breakpoint_positions" : ";".join(str(p) for p in r["breakpoint_positions"]),
            "magnitude_label"      : r["magnitude_label"],
            "magnitude_value"      : r["magnitude_value"],
            "noise_label"          : r["noise_label"],
            "variance"             : r["variance"],
            "autocorrelation_label": r["autocorrelation_label"],
            "autocorrelation_rho"  : r["autocorrelation_rho"],
            "sparse_series"        : r["sparse_series"],
        })

    df_meta = pd.DataFrame(metadata)
    df_meta.to_csv(os.path.join(output_dir, "metadata.csv"), index=False)
    pyreadr.write_rdata(os.path.join(output_dir, "metadata.RData"), df_meta, df_name="metadata")
    print(f"\nMetadata: {len(df_meta):,} series")
    print(df_meta.groupby(["series_type", "anomaly_type"]).size().rename("n").to_string())

    rows = [
        {"series_id": i, "time": t, "value": v, "is_anomaly": lbl}
        for i, r in enumerate(records)
        for t, (v, lbl) in enumerate(zip(r["values"], r["labels"]))
    ]
    df_series = pd.DataFrame(rows)
    df_series.to_csv(os.path.join(output_dir, "series.csv"), index=False)
    pyreadr.write_rdata(os.path.join(output_dir, "series.RData"), df_series, df_name="series")
    print(f"Series:   {len(df_series):,} rows")


def main(cfg=None):
    if cfg is None:
        cfg = PARAMETERS

    print("Calibrated magnitudes (multiple × series noise):")
    for series_type in cfg["lengths"]:
        mags  = magnitudes_for_type(series_type, cfg["noise_by_type"], cfg["magnitude_multiples"])
        noise = cfg["noise_by_type"][series_type]
        print(f"  {series_type} (noise = {noise}):")
        for label, val in mags.items():
            print(f"    {label:6s} = {val:.6f}  (SNR = {val/noise:.1f}×)")
    print()

    records = run_simulation(cfg)
    save_results(records, cfg["output_dir"])


if __name__ == "__main__":
    main()
