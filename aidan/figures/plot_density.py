import zipfile
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def add_curve(ax, x, col='red', lab=''):
    mean, std = np.mean(x), np.std(x)
    x_vals = np.linspace(min(x), max(x), 1000)
    pdf = (1 / (np.sqrt(2 * np.pi) * std)) * np.exp(-0.5 * ((x_vals - mean) / std) ** 2)
    mask = x_vals >= -2
    x_vals = x_vals[mask]
    pdf = pdf[mask]
    ax.plot(x_vals, pdf, color=col, label=lab, lw=3)


with zipfile.ZipFile('combined_results.zip', 'r') as zip_ref:
    with zip_ref.open(zip_ref.namelist()[0]) as file:
        df = pd.read_csv(file, sep=';', decimal=',')

df = df[df['instance'].isin(['aidan', 'baseline', 'naive'])]
df['smape'] = np.log(df['smape'].where(df['smape'] > 0.05, np.nan))
df['instance'] = df['instance'].replace({
    'aidan': 'AIDAN',
    'baseline': 'Baseline',
    'naive': 'Naive'
})

ranges = ['AIDAN', 'Baseline', 'Naive']
colors = ['#4E79A7', '#F28E2B', '#59A14F']
palette_dict = dict(zip(ranges, colors))

fig, ax = plt.subplots(figsize=(8, 5))
for i, model in enumerate(ranges):
    x = df['smape'][df['instance'] == model]
    add_curve(ax, x, col=colors[i], lab=model)

ax.set_xlabel('log(SMAPE)', fontweight='bold', fontsize=20)
ax.set_ylabel('Density', fontweight='bold', fontsize=20)
ax.tick_params(axis='x', labelsize=20)
ax.tick_params(axis='y', labelsize=20)
ax.legend(fontsize=20, frameon=False, title=None)

plt.tight_layout()
plt.savefig('figures/fig_smape_density.pdf')
plt.show()
