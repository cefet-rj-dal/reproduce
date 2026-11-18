import pandas as pd
from pathlib import Path
from scipy.stats import wilcoxon


SMAPE_PATH = Path("smape_series.csv")
CLUSTERS_PATH = Path("cluster_series.csv")


def _load_inputs():
    smape = pd.read_csv(SMAPE_PATH, sep=";", decimal=",")
    clusters = pd.read_csv(CLUSTERS_PATH)
    smape["ts"] = smape["ts"].astype(str).str.strip()
    clusters["ts"] = clusters["ts"].astype(str).str.strip()
    return smape, clusters


def _attach_clusters(smape: pd.DataFrame, clusters: pd.DataFrame) -> pd.DataFrame:
    smape = smape.drop(columns=["cluster"], errors="ignore")
    merged = smape.merge(clusters, on="ts", how="left", validate="m:1")
    return merged


def _format_mean_std(mean: float, std: float) -> str:
    std = 0.0 if pd.isna(std) else float(std)
    return f"{mean:.4f} +- {std:.4f}".replace(".", ",")


def _overlap(a_low, a_high, b_low, b_high):
    return max(a_low, b_low) <= min(a_high, b_high)


def _build_pivot(df: pd.DataFrame) -> pd.DataFrame:
    grouped = df.groupby(["model", "cluster", "seed"], as_index=False)["smape"].mean().rename(columns={"smape": "seed_smape"})
    summary = grouped.groupby(["model", "cluster"])["seed_smape"].agg(mean="mean", std=lambda s: s.std(ddof=0)).reset_index()
    summary["value"] = summary.apply(lambda r: _format_mean_std(r["mean"], r["std"]), axis=1)
    summary["mark"] = ""
    for cluster in summary["cluster"].unique():
        cluster_df = summary[summary["cluster"] == cluster].copy().sort_values("mean")
        winner = cluster_df.iloc[0]
        w_low, w_high = winner["mean"] - winner["std"], winner["mean"] + winner["std"]
        first_place = [idx for idx, row in cluster_df.iterrows() if _overlap(row["mean"] - row["std"], row["mean"] + row["std"], w_low, w_high)]
        summary.loc[first_place, "mark"] += "*"
        remaining = cluster_df.drop(index=first_place)
        if len(remaining):
            second = remaining.iloc[0]
            s_low, s_high = second["mean"] - second["std"], second["mean"] + second["std"]
            second_place = [idx for idx, row in remaining.iterrows() if _overlap(row["mean"] - row["std"], row["mean"] + row["std"], s_low, s_high)]
            summary.loc[second_place, "mark"] += "^"
    summary["value"] += summary["mark"]
    pivot = summary.pivot(index="model", columns="cluster", values="value")
    pivot = pivot.sort_index(axis=0).sort_index(axis=1)
    pivot.columns = [str(col) for col in pivot.columns]
    return pivot


def _format_avg_rank(avg_rank: float) -> str:
    return f"{avg_rank:.4f}".replace(".", ",")


def _format_ci95(ci95: float) -> str:
    ci95 = 0.0 if pd.isna(ci95) else float(ci95)
    return f"{ci95:.4f}".replace(".", ",")


def _build_ranking_pivot(df: pd.DataFrame) -> pd.DataFrame:
    grouped = df.groupby(["ts", "seed", "model"], as_index=False)["smape"].mean()
    seed_counts = grouped.groupby("model")["seed"].nunique()
    single_seed_models = set(seed_counts[seed_counts == 1].index.astype(str))
    seeds = sorted(grouped["seed"].dropna().unique().tolist())
    ts_values = sorted(grouped["ts"].dropna().unique().tolist())
    models = sorted(grouped["model"].dropna().astype(str).unique().tolist())
    seed_avg_ranks = []
    for seed in seeds:
        seed_slice = grouped[grouped["seed"] == seed].copy()
        repeated_rows = []
        for model in single_seed_models:
            model_rows = grouped[grouped["model"].astype(str) == model].copy()
            if model_rows.empty:
                continue
            if model_rows["seed"].iloc[0] != seed:
                model_rows["seed"] = seed
                repeated_rows.append(model_rows)
        if repeated_rows:
            seed_slice = pd.concat([seed_slice] + repeated_rows, ignore_index=True)
        wide = seed_slice.pivot_table(index="ts", columns="model", values="smape", aggfunc="mean")
        wide = wide.reindex(index=ts_values, columns=models)
        wide = wide.dropna(how="all")
        ranks = wide.rank(axis=1, method="average", ascending=True)
        avg_ranks = ranks.mean(axis=0, skipna=True).reset_index()
        avg_ranks.columns = ["model", "avg_rank"]
        avg_ranks["seed"] = seed
        seed_avg_ranks.append(avg_ranks)
    per_seed = pd.concat(seed_avg_ranks, ignore_index=True)
    summary = (per_seed.groupby("model")["avg_rank"].agg(mean="mean", std=lambda s: s.std(ddof=0), n="count").reset_index())
    summary["value"] = summary.apply(
        lambda r: f"{_format_avg_rank(r['mean'])} +- {_format_ci95(0.0 if pd.isna(r['std']) or r['n'] == 0 else 1.96 * float(r['std']) / (r['n'] ** 0.5))}",
        axis=1,
    )
    summary["mark"] = ""
    if not summary.empty:
        summary = summary.sort_values("mean", kind="stable").reset_index(drop=True)
        winner = summary.iloc[0]
        w_low, w_high = winner["mean"] - winner["std"], winner["mean"] + winner["std"]
        first_place = summary.index[
            summary.apply(lambda row: _overlap(row["mean"] - row["std"], row["mean"] + row["std"], w_low, w_high), axis=1,)
        ].tolist()
        summary.loc[first_place, "mark"] += "*"
        remaining = summary.drop(index=first_place)
        if len(remaining):
            second = remaining.iloc[0]
            s_low, s_high = second["mean"] - second["std"], second["mean"] + second["std"]
            second_place = remaining.index[
                remaining.apply(lambda row: _overlap(row["mean"] - row["std"], row["mean"] + row["std"], s_low, s_high), axis=1,)
            ].tolist()
            summary.loc[second_place, "mark"] += "^"
    summary["value"] += summary["mark"]
    pivot = summary.set_index("model")[["value"]]
    return pivot


def _wilcoxon_effect_size(statistic: float, n: int) -> float:
    total_rank = n * (n + 1) / 2
    return 1 - (2 * statistic) / total_rank if total_rank > 0 else float("nan")


def _paired_wilcoxon(df: pd.DataFrame, left_model: str, right_model: str, ) -> dict | None:
    paired = (
        df[df["model"].astype(str).isin([left_model, right_model])]
        .groupby(["ts", "model"], as_index=False)["smape"]
        .min()
        .pivot_table(index=["ts"], columns="model", values="smape", aggfunc="mean",)
    )
    paired = paired.dropna(subset=[left_model, right_model])
    n = len(paired)
    if n == 0: return None
    wt = wilcoxon(paired[left_model], paired[right_model], alternative="less",)
    return {
        "paired_test": f"{left_model} vs {right_model}",
        "n": n,
        "W": float(wt.statistic),
        "p_value": float(wt.pvalue),
        "effect_size": float(_wilcoxon_effect_size(float(wt.statistic), n)),
    }


def _build_wilcoxon_report(df: pd.DataFrame) -> pd.DataFrame:
    paired_tests = [("foresight", "arima"), ("foresight", "mlp")]
    rows = []
    for strategy in ["ro", "sa"]:
        subset = df[df["strategy"].astype(str).str.lower() == strategy].copy()
        for test in paired_tests:
            result = _paired_wilcoxon(subset, *test)
            rows.append({"strategy": strategy, **result,})
    report = pd.DataFrame(rows)
    report["n"] = report["n"].astype(int)
    report["W"] = report["W"].map(lambda x: f"{x:.1f}".replace(".", ","))
    report["p_value"] = report["p_value"].map(lambda x: f"{x:.4g}".replace(".", ","))
    report["effect_size"] = report["effect_size"].map(lambda x: f"{x:.3f}".replace(".", ","))
    return report


def _format_smape_block(cluster: str, table_df: pd.DataFrame) -> list[str]:
    lines = [f"cluster: {cluster}", "model;ro;sa",]
    for _, row in table_df.iterrows():
        lines.append(f"{row['model']};{row['ro']};{row['sa']}")
    lines.append("")
    return lines


def _format_ranking_block(cluster: str, table_df: pd.DataFrame) -> list[str]:
    lines = [f"cluster: {cluster}", "model;ro;sa",]
    for _, row in table_df.iterrows():
        lines.append(f"{row['model']};{row['ro']};{row['sa']}")
    lines.append("")
    return lines


def _format_check_block(cluster: str, strategy: str, table_df: pd.DataFrame) -> list[str]:
    lines = [f"cluster: {cluster}", f"strategy: {strategy}", "ts;seed;model;avg_smape",]
    for _, row in table_df.iterrows():
        lines.append(f"{row['ts']};{row['seed']};{row['model']};{row['avg_smape']:.6f}".replace(".", ","))
    lines.append("")
    return lines


def _format_wilcoxon_block(cluster: str, report_df: pd.DataFrame) -> list[str]:
    lines = [f"cluster: {cluster}", "strategy;paired_test;n;W;p_value;effect_size",]
    for _, row in report_df.iterrows():
        lines.append(f"{row['strategy']};{row['paired_test']};{row['n']};{row['W']};{row['p_value']};{row['effect_size']}")
    lines.append("")
    return lines


def _write_txt_report(path: Path, blocks: list[list[str]]) -> None:
    with path.open("w", encoding="utf-8", newline="\n") as f:
        for block in blocks:
            for line in block:
                f.write(line + "\n")


def main():
    smape, clusters = _load_inputs()
    smape = _attach_clusters(smape, clusters)
    smape.to_csv(SMAPE_PATH, sep=";", decimal=",", index=False)
    smape_blocks = []
    ranking_blocks = []
    wilcoxon_blocks = []
    for cluster in sorted(smape["cluster"].astype(str).unique()):
        cluster_df = smape[smape["cluster"].astype(str) == cluster].copy()
        outputs = []
        ranking_outputs = []
        for strategy in ["ro", "sa"]:
            subset = cluster_df[cluster_df["strategy"].astype(str).str.lower() == strategy].copy()
            pivot = _build_pivot(subset)
            outputs.append(pivot[[cluster]].rename(columns={cluster: strategy}))
            ranking_pivot = _build_ranking_pivot(subset)
            ranking_outputs.append(ranking_pivot.rename(columns={"value": strategy}))
        combined = pd.concat(outputs, axis=1).sort_index()
        smape_blocks.append(_format_smape_block(cluster, combined.reset_index()))
        ranking_combined = pd.concat(ranking_outputs, axis=1).sort_index()
        ranking_blocks.append(_format_ranking_block(cluster, ranking_combined.reset_index()))
        wilcoxon_report = _build_wilcoxon_report(cluster_df)
        wilcoxon_blocks.append(_format_wilcoxon_block(cluster, wilcoxon_report))
    print("Writing summary files")
    #_write_txt_report(Path("smape_clusters.txt"), smape_blocks)
    _write_txt_report(Path("ranking_clusters.txt"), ranking_blocks)
    _write_txt_report(Path("wilcoxon_clusters.txt"), wilcoxon_blocks)


if __name__ == "__main__":
    main()
