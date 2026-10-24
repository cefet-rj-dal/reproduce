"""
Visualization of generated synthetic series.

Usage:
    python visualize_series.py                             # 1 example per scenario
    python visualize_series.py --type mean                 # filter by anomaly type (mean/trend/variance/none)
    python visualize_series.py --noise high                # filter noise level (low/high)
    python visualize_series.py --ar1 with_ar1              # filter autocorrelation (no_ar1/with_ar1)
    python visualize_series.py --magnitude high            # filter magnitude (low/high)
    python visualize_series.py --name mortality            # filter series type (mortality/hospitalization)
    python visualize_series.py --n 12                      # number of series (default: 9)
    python visualize_series.py --ids 0 5 42                # plot specific series by series_id
    python visualize_series.py --breakpoints               # mark breakpoints with vertical lines
    python visualize_series.py --save                      # save PNG instead of displaying
"""

import argparse
import sys
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path

OUTPUT_DIR = Path("synthetic_series")


def load_data():
    series   = pd.read_csv(OUTPUT_DIR / "series.csv")
    metadata = pd.read_csv(OUTPUT_DIR / "metadata.csv")
    return series, metadata


def filter_metadata(meta, args):
    mask = pd.Series(True, index=meta.index)
    if args.type:
        mask &= meta["anomaly_type"] == args.type
    if args.noise:
        mask &= meta["noise_label"] == args.noise
    if args.ar1:
        mask &= meta["autocorrelation_label"] == args.ar1
    if args.magnitude:
        mask &= meta["magnitude_label"] == args.magnitude
    if args.name:
        mask &= meta["series_type"] == args.name
    return meta[mask]


def select_examples(filtered_meta, n):
    if len(filtered_meta) <= n:
        return filtered_meta["series_id"].tolist()
    groups    = filtered_meta.groupby(["anomaly_type", "series_type"])
    per_group = max(1, n // len(groups))
    selected  = []
    for _, g in groups:
        selected.extend(g["series_id"].head(per_group).tolist())
        if len(selected) >= n:
            break
    return selected[:n]


def plot(series_ids, series_df, meta_df, save=False, mark_breakpoints=False):
    for sid in series_ids:
        data = series_df[series_df["series_id"] == sid].sort_values("time")
        row  = meta_df[meta_df["series_id"] == sid].iloc[0]

        times  = data["time"].values
        values = data["value"].values

        fig, ax = plt.subplots(figsize=(12, 4))
        fig.patch.set_facecolor("white")
        ax.set_facecolor("white")

        ax.plot(times, values, color="black", linewidth=0.8,
                marker="o", markersize=1.8, markerfacecolor="black", markeredgewidth=0)

        if mark_breakpoints and row["n_breaks"] > 0 and pd.notna(row["breakpoint_positions"]):
            for pos in str(row["breakpoint_positions"]).split(";"):
                ax.axvline(int(pos), color="black", linewidth=0.9, linestyle="--", alpha=0.6)

        for spine in ax.spines.values():
            spine.set_linewidth(0.6)
            spine.set_color("#888888")
        ax.tick_params(labelsize=8, color="#888888", labelcolor="#444444", length=3)
        ax.set_xlabel("Time",  fontsize=9, color="#444444")
        ax.set_ylabel("Value", fontsize=9, color="#444444")
        ax.set_xlim(times[0], times[-1])
        ax.grid(False)

        title = f"ID {sid} | {row['series_type']} | {row['anomaly_type']} | q={row['n_breaks']}"
        if row["n_breaks"] > 0 and pd.notna(row["breakpoint_positions"]):
            cps      = "; ".join(str(row["breakpoint_positions"]).split(";"))
            subtitle = (f"true cps: {cps} | noise: {row['noise_label']} | "
                        f"{row['autocorrelation_label']} | magnitude: {row['magnitude_label']}")
        else:
            subtitle = f"noise: {row['noise_label']} | {row['autocorrelation_label']}"

        fig.text(0.125, 0.97, title,    fontsize=9,   color="#222222",
                 transform=fig.transFigure, va="top")
        fig.text(0.125, 0.92, subtitle, fontsize=7.5, color="#555555",
                 transform=fig.transFigure, va="top")

        plt.tight_layout(rect=[0, 0, 1, 0.88])

        if save:
            path = OUTPUT_DIR / f"series_{sid}.png"
            fig.savefig(path, dpi=150, bbox_inches="tight", facecolor="white")
            print(f"Saved: {path}")
            plt.close(fig)
        else:
            plt.show()


def main():
    parser = argparse.ArgumentParser(description="Visualize generated synthetic series.")
    parser.add_argument("--type",        choices=["mean", "trend", "variance", "none"])
    parser.add_argument("--noise",       choices=["low", "high"])
    parser.add_argument("--ar1",         choices=["no_ar1", "with_ar1"])
    parser.add_argument("--magnitude",   choices=["low", "high"])
    parser.add_argument("--name",        choices=["mortality", "hospitalization"])
    parser.add_argument("--n",           type=int, default=9)
    parser.add_argument("--ids",         type=int, nargs="+")
    parser.add_argument("--breakpoints", action="store_true")
    parser.add_argument("--save",        action="store_true")
    args = parser.parse_args()

    series_df, meta_df = load_data()

    if args.ids:
        ids = [i for i in args.ids if i in meta_df["series_id"].values]
        if not ids:
            print("No valid series_id found.", file=sys.stderr)
            sys.exit(1)
    else:
        filtered = filter_metadata(meta_df, args)
        if filtered.empty:
            print("No series match the given filters.", file=sys.stderr)
            sys.exit(1)
        ids = select_examples(filtered, args.n)

    plot(ids, series_df, meta_df, save=args.save, mark_breakpoints=args.breakpoints)


if __name__ == "__main__":
    main()
