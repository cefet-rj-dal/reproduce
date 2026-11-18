"""
Estimates NOISE_BY_TYPE, NOISE_LEVELS, and AUTOCORRELATIONS from real monthly
SUS indicators (indicadores_mensais/).

Methods
-------
NOISE_BY_TYPE    : mean std_diff/sqrt(2) per series in original scale.
NOISE_LEVELS     : Gaussian noise variance in normalized [0,1] space, after
                   removing linear trend and monthly seasonality.
AUTOCORRELATIONS : lag-1 AR(1) coefficient of residuals.

Usage
-----
    python calibrate_noise.py
"""

import os
import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler

INDICATORS_DIR = os.path.join("indicadores_mensais", "indicadores_mensais")


def load_national_series(path: str) -> pd.Series:
    df = pd.read_csv(path, encoding="latin-1")
    return (
        df[df["Tipo_Local"] == "Pais"]
        .groupby(["Ano", "Mes"])["Taxa_Padronizada"]
        .mean()
        .reset_index(drop=True)
        .dropna()
    )


def normalize(series: pd.Series) -> np.ndarray:
    return MinMaxScaler().fit_transform(series.values.reshape(-1, 1)).flatten()


def remove_trend_seasonality(values: np.ndarray) -> np.ndarray:
    n         = len(values)
    t         = np.arange(n)
    coef      = np.polyfit(t, values, 1)
    detrended = values - np.polyval(coef, t)
    months    = np.tile(np.arange(12), int(np.ceil(n / 12)))[:n]
    monthly_means = np.array([detrended[months == m].mean() for m in range(12)])
    return detrended - monthly_means[months]


def original_noise_std(series: pd.Series) -> float:
    return series.diff().dropna().std() / np.sqrt(2)


def normalized_noise_variance(series: pd.Series) -> float:
    resid = remove_trend_seasonality(normalize(series))
    std   = pd.Series(resid).diff().dropna().std() / np.sqrt(2)
    return round(std ** 2, 6)


def ar1_coefficient(series: pd.Series) -> float:
    resid = remove_trend_seasonality(normalize(series))
    return round(pd.Series(resid).autocorr(lag=1), 4)


def main():
    results = []
    for fname in sorted(os.listdir(INDICATORS_DIR)):
        if not fname.endswith(".csv"):
            continue
        series_type = "hospitalization" if "internacao" in fname else "mortality"
        series = load_national_series(os.path.join(INDICATORS_DIR, fname))
        results.append({
            "file"      : fname,
            "type"      : series_type,
            "noise_orig": original_noise_std(series),
            "var_norm"  : normalized_noise_variance(series),
            "ar1"       : ar1_coefficient(series),
        })

    df = pd.DataFrame(results)

    print(f"{'File':<42} {'noise_orig':>10} {'var_norm':>10} {'ar1':>8}")
    print("-" * 75)
    for _, r in df.iterrows():
        print(f"  {r['file']:<40} {r['noise_orig']:>10.4f} {r['var_norm']:>10.6f} {r['ar1']:>8.4f}")

    print()
    for stype in ["hospitalization", "mortality"]:
        sub = df[df["type"] == stype]
        print(f"--- {stype} ---")
        print(f"  NOISE_BY_TYPE  mean={sub['noise_orig'].mean():.4f}  "
              f"(min={sub['noise_orig'].min():.4f}, max={sub['noise_orig'].max():.4f})")
        print(f"  var_norm       mean={sub['var_norm'].mean():.6f}  "
              f"(min={sub['var_norm'].min():.6f}, max={sub['var_norm'].max():.6f})")
        print(f"  AR(1)          mean={sub['ar1'].mean():.4f}  "
              f"(min={sub['ar1'].min():.4f}, max={sub['ar1'].max():.4f})")
        print()

    var_low  = round(df[df["type"] == "hospitalization"]["var_norm"].mean(), 3)
    var_high = round(df["var_norm"].max(), 3)

    print("Derived constants:")
    print(f"  NOISE_BY_TYPE    hospitalization="
          f"{round(df[df['type']=='hospitalization']['noise_orig'].mean(), 3)}"
          f"  mortality={round(df[df['type']=='mortality']['noise_orig'].mean(), 3)}")
    print(f"  NOISE_LEVELS     low={var_low}  high={var_high}")
    print(f"  AUTOCORRELATIONS no_ar1=0.0  with_ar1=0.8"
          f"  (mean hosp={round(df[df['type']=='hospitalization']['ar1'].mean(), 2)}"
          f", mean mort={round(df[df['type']=='mortality']['ar1'].mean(), 2)})")


if __name__ == "__main__":
    main()
