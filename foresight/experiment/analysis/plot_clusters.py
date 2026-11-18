import csv
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.gridspec import GridSpec
from sklearn.decomposition import PCA


ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from embedding.ts_enc_emb import EmbeddingEncoder


def cluster0_series(n=220, seed=None):
    rng = np.random.default_rng(seed)
    t = np.arange(n)
    x = np.cumsum(rng.normal(0, 0.05, n))
    x += 0.15 * np.sin(2 * np.pi * t / 140)
    return t, x


def cluster1_series(n=220, seed=None):
    rng = np.random.default_rng(seed)
    t = np.arange(n)
    x = (
        0.35 * np.sin(2 * np.pi * t / 7)
        + 0.25 * np.sin(2 * np.pi * t / 13)
        + 0.20 * np.sin(2 * np.pi * t / 23)
        + rng.normal(0, 0.35, n)
    )
    return t, x


def fourier_transform(signal):
    signal = signal - signal.mean()
    fft = np.fft.rfft(signal)
    magnitude = np.abs(fft)
    frequency = np.fft.rfftfreq(len(signal))
    if magnitude.max() > 0:
        magnitude /= magnitude.max()
    return frequency, magnitude


def load_ts_csv(csv_path):
    data = {}
    with open(csv_path, newline="", encoding="utf-8") as f:
        reader = csv.reader(f)
        for row in reader:
            if not row:
                continue
            name = row[0].strip()
            if not name:
                continue
            size = int(float(row[1]))
            values = []
            for raw in row[2 : 2 + size]:
                raw = raw.strip()
                if raw:
                    values.append(float(raw))
            data[name] = np.asarray(values, dtype=float)
    return data


def _plot_series(ax, t, x, color, title):
    ax.plot(t, x, lw=2, color=color)
    ax.set_title(title, fontsize=12, fontweight="bold")
    ax.set_xlabel("Time")
    ax.set_ylabel("Value")
    ax.grid(alpha=0.2, linewidth=0.7)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


def _plot_fft(ax, freq, mag, color, title):
    ax.plot(freq, mag, lw=2, color=color)
    ax.set_title(title, fontsize=12, fontweight="bold")
    ax.set_xlabel("Frequency")
    ax.set_ylabel("Amplitude")
    ax.set_xlim(0, 0.5)
    ax.grid(alpha=0.2, linewidth=0.7)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


def _plot_real_series(ax, data, series_name, color, title):
    series = data[series_name]
    t = np.arange(len(series))
    ax.plot(t, series, lw=2, color=color)
    ax.set_title(title, fontsize=12, fontweight="bold")
    ax.set_xlabel("Time")
    ax.set_ylabel("Value")
    ax.grid(alpha=0.2, linewidth=0.7)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


def _plot_real_fft(ax, data, series_name, color, title):
    series = data[series_name]
    freq, mag = fourier_transform(series)
    ax.plot(freq, mag, lw=2, color=color)
    ax.set_title(title, fontsize=12, fontweight="bold")
    ax.set_xlabel("Frequency")
    ax.set_ylabel("Amplitude")
    ax.set_xlim(0, 0.5)
    ax.grid(alpha=0.2, linewidth=0.7)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


def _plot_embedding(ax, series, color, title):
    windows = np.stack([series[i : i + 30] for i in range(len(series) - 30 + 1)])
    encoder = EmbeddingEncoder(dim=30, seq=True, encoder=None)
    _, embedded = encoder.get_embeddings(windows, transform="msrff")
    embedding = embedded.to_numpy(dtype=float)
    embedding = PCA(n_components=2, random_state=42).fit_transform(embedding)
    ax.plot(embedding[:, 0], embedding[:, 1], lw=1.5, color=color, alpha=0.85)
    ax.scatter(embedding[:, 0], embedding[:, 1], s=10, color=color, alpha=0.35)
    ax.set_title(title, fontsize=12, fontweight="bold")
    ax.set_xlabel("Dim 1")
    ax.set_ylabel("Dim 2")
    ax.grid(alpha=0.2, linewidth=0.7)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


def build_figure():
    csv_path = Path(__file__).with_name("ts.csv")
    ts_data = load_ts_csv(csv_path)

    t0, c0 = cluster0_series(seed=42)
    t1, c1 = cluster1_series(seed=42)
    f0, s0 = fourier_transform(c0)
    f1, s1 = fourier_transform(c1)

    real0_name = "biocons_brazil"
    real1_name = "biocons_canada"

    fig = plt.figure(figsize=(26, 9), constrained_layout=True)
    gs = GridSpec(2, 6, figure=fig, width_ratios=[1.05, 1.15, 1.15, 1.15, 1.15, 1.15])

    ax00 = fig.add_subplot(gs[0, 0])
    _plot_series(ax00, t0, c0, "#0B5CAD", "Cluster 0 - Example Series")

    ax01 = fig.add_subplot(gs[0, 1])
    _plot_fft(ax01, f0, s0, "#0B5CAD", "Cluster 0 - Fourier Transform")

    ax02 = fig.add_subplot(gs[0, 2])
    _plot_embedding(ax02, c0, "#0B5CAD", "Cluster 0 - Temporal Embedding")

    ax03 = fig.add_subplot(gs[0, 3])
    _plot_real_series(
        ax03,
        ts_data,
        real0_name,
        "#0B5CAD",
        f"Cluster 0 - Real Series ({real0_name})",
    )

    ax04 = fig.add_subplot(gs[0, 4])
    _plot_real_fft(
        ax04,
        ts_data,
        real0_name,
        "#0B5CAD",
        "Cluster 0 - Real Fourier Transform",
    )

    ax05 = fig.add_subplot(gs[0, 5])
    _plot_embedding(
        ax05,
        ts_data[real0_name],
        "#0B5CAD",
        "Cluster 0 - Real Temporal Embedding",
    )

    ax10 = fig.add_subplot(gs[1, 0])
    _plot_series(ax10, t1, c1, "#E67E22", "Cluster 1 - Example Series")

    ax11 = fig.add_subplot(gs[1, 1])
    _plot_fft(ax11, f1, s1, "#E67E22", "Cluster 1 - Fourier Transform")

    ax12 = fig.add_subplot(gs[1, 2])
    _plot_embedding(ax12, c1, "#E67E22", "Cluster 1 - Temporal Embedding")

    ax13 = fig.add_subplot(gs[1, 3])
    _plot_real_series(
        ax13,
        ts_data,
        real1_name,
        "#E67E22",
        f"Cluster 1 - Real Series ({real1_name})",
    )

    ax14 = fig.add_subplot(gs[1, 4])
    _plot_real_fft(
        ax14,
        ts_data,
        real1_name,
        "#E67E22",
        "Cluster 1 - Real Fourier Transform",
    )

    ax15 = fig.add_subplot(gs[1, 5])
    _plot_embedding(
        ax15,
        ts_data[real1_name],
        "#E67E22",
        "Cluster 1 - Real Temporal Embedding",
    )

    fig.suptitle("Cluster Comparison", fontsize=18, fontweight="bold")
    return fig


def main(output_path="clusters.pdf"):
    fig = build_figure()
    fig.savefig(output_path, format="pdf", bbox_inches="tight")
    plt.close(fig)
    return output_path


if __name__ == "__main__":
    main()
