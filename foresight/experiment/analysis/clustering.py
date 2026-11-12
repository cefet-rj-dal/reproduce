import numpy as np
import pandas as pd
import umap
import warnings
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import StandardScaler

warnings.filterwarnings("ignore")

MIN_CLUSTERS = 2
MAX_CLUSTERS = 10
MIN_UMAP_COMPONENTS = 2
MAX_UMAP_COMPONENTS = 20
RANDOM_STATE = 100

INPUT_CSV = "ts.csv"
OUTPUT_SERIES_CLUSTERS = "cluster_series.csv"
OUTPUT_CLUSTER_SUMMARY = "cluster_summary.csv"
OUTPUT_CLUSTER_PLOT = "cluster_umap.pdf"

FEATURE_MEANINGS = {
    "zcr": "sign change rate",
    "acf1": "lag-1 linear autocorrelation",
    "acf2": "lag-2 linear autocorrelation",
    "acf_sum_abs_1_10": "overall linear dependence",
    "spectral_entropy": "spectral complexity",
    "dominant_frequency": "dominant oscillation frequency",
}

def _safe_corr(a, b):
    if len(a) < 2 or len(b) < 2:
        return np.nan
    if np.std(a) < 1e-12 or np.std(b) < 1e-12:
        return np.nan
    return float(np.corrcoef(a, b)[0, 1])


def _autocorr(ts, lag):
    if len(ts) <= lag:
        return np.nan
    return _safe_corr(ts[:-lag], ts[lag:])


def _zero_cross_rate(ts):
    signs = np.sign(ts).astype(float)
    signs[signs == 0] = np.nan
    signs = signs[~np.isnan(signs)]
    if len(signs) < 2:
        return 0.0
    return float(np.mean(signs[1:] * signs[:-1] < 0))


def _spectral_features(ts):
    x = np.asarray(ts, dtype=float)
    x = x - np.mean(x)
    fft = np.fft.rfft(x)
    power = np.abs(fft) ** 2
    freqs = np.fft.rfftfreq(len(x), d=1.0)
    power = power[1:]
    freqs = freqs[1:]
    if len(power) == 0 or power.sum() <= 0:
        return {"spectral_entropy": 0.0, "dominant_frequency": 0.0}
    p = power / power.sum()
    spectral_entropy = float(-np.sum(p * np.log(p + 1e-12)) / np.log(len(p)))
    dominant_frequency = float(freqs[np.argmax(power)])
    return {
        "spectral_entropy": spectral_entropy,
        "dominant_frequency": dominant_frequency,
    }


def extract(ts):
    ts = np.asarray(ts, dtype=float)
    ts = ts[~np.isnan(ts)]
    acf10 = np.array([_autocorr(ts, lag) for lag in range(1, 11)], dtype=float)
    features = {
        "zcr": _zero_cross_rate(ts),
        "acf1": float(acf10[0]),
        "acf2": float(acf10[1]),
        "acf_sum_abs_1_10": float(np.nansum(np.abs(acf10))),
    }
    features.update(_spectral_features(ts))
    return features


df = pd.read_csv(INPUT_CSV, header=None)

names = []
rows = []

for i in range(len(df)):
    # column 0 = name, column 1 = size, column 2+ = values
    values = pd.to_numeric(df.iloc[i, 2:], errors="coerce").values
    rows.append(extract(values))
    names.append(str(df.iloc[i, 0]))

Xdf = pd.DataFrame(rows)
Xdf = Xdf.replace([np.inf, -np.inf], np.nan)
Xdf = Xdf.fillna(Xdf.median(numeric_only=True)).fillna(0)
X = StandardScaler().fit_transform(Xdf)

best_result = None

for n_components in range(MIN_UMAP_COMPONENTS, MAX_UMAP_COMPONENTS + 1):
    embedding = umap.UMAP(n_components=n_components, random_state=RANDOM_STATE).fit_transform(X)

    for k in range(MIN_CLUSTERS, MAX_CLUSTERS + 1):
        #clusters = AgglomerativeClustering(n_clusters=k).fit_predict(embedding)
        clusters = KMeans(n_clusters=k, random_state=RANDOM_STATE, n_init=10).fit_predict(embedding)
        if len(np.unique(clusters)) < 2: continue
        sil = float(silhouette_score(embedding, clusters))
        if best_result is None or sil > best_result["silhouette"]:
            best_result = {
                "k": k,
                "umap_components": n_components,
                "silhouette": sil,
                "clusters": clusters,
                "embedding": embedding,
            }

if best_result is None:
    raise RuntimeError("No valid clustering configuration was found.")

clusters = best_result["clusters"]
unique_clusters = sorted(np.unique(clusters))
cluster_map = {c: len(unique_clusters) - 1 - i for i, c in enumerate(unique_clusters)}
clusters = np.array([cluster_map[c] for c in clusters])
series_clusters = pd.DataFrame({"ts": names, "cluster": [f"cluster_{c}" for c in clusters]})
series_clusters.to_csv(OUTPUT_SERIES_CLUSTERS, index=False)
feature_table = pd.DataFrame(StandardScaler().fit_transform(Xdf), columns=Xdf.columns)
feature_table["cluster"] = clusters
global_mean = feature_table.drop(columns=["cluster"]).mean()

summary_rows = []
for c in sorted(np.unique(clusters)):
    subset = feature_table[feature_table.cluster == c]
    centroid = subset.drop(columns=["cluster"]).mean()
    delta = centroid - global_mean

    important = [feat for feat in FEATURE_MEANINGS.keys() if feat in delta.index]
    text = []
    for feat in important:
        direction = "high" if delta[feat] > 0 else "low"
        meaning = FEATURE_MEANINGS.get(feat, feat)
        text.append(f"{meaning}: {direction}")

    summary_rows.append(
        {
            "cluster": f"cluster_{c}",
            "series_count": len(subset),
            "global_silhouette": round(best_result["silhouette"], 4),
            "cluster_description": " | ".join(text),
        }
    )

summary = pd.DataFrame(summary_rows)[
    [
        "cluster",
        "series_count",
        "global_silhouette",
        "cluster_description",
    ]
]
summary.to_csv(OUTPUT_CLUSTER_SUMMARY, index=False)

plot_embedding = best_result["embedding"]
if plot_embedding.shape[1] < 2:
    plot_embedding = np.column_stack([plot_embedding[:, 0], np.zeros(len(plot_embedding))])
else:
    plot_embedding = plot_embedding[:, :2]

cluster_ids = np.asarray(clusters)

plt.figure(figsize=(16, 9))
for c in sorted(np.unique(cluster_ids)):
    mask = cluster_ids == c
    plt.scatter(
        plot_embedding[mask, 0],
        plot_embedding[mask, 1],
        s=60,
        alpha=0.85,
        label=f"Cluster {c}",
    )

#plt.title(f"UMAP clusters | k = {best_result['k']} | silhouette = {round(best_result['silhouette'], 4)}")
plt.xlabel("UMAP 1", fontsize=16, fontweight="bold")
plt.ylabel("UMAP 2", fontsize=16, fontweight="bold")
plt.xticks(fontsize=16)
plt.yticks(fontsize=16)
plt.legend(title="Clusters", loc="best", frameon=True, edgecolor="black", fontsize=16, title_fontsize=16)
plt.tight_layout()
plt.savefig(OUTPUT_CLUSTER_PLOT, bbox_inches="tight")
plt.close()

print()
print("Created files:")
print(f'... {OUTPUT_SERIES_CLUSTERS}')
print(f'... {OUTPUT_CLUSTER_SUMMARY}')
print(f'... {OUTPUT_CLUSTER_PLOT}')
print("Selected UMAP components =", best_result["umap_components"])
print("Selected K =", best_result["k"])
print("Silhouette =", round(best_result["silhouette"], 4))
