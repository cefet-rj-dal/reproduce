import zipfile
import pandas as pd
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

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

plt.figure(figsize=(8, 5))
sns.violinplot(
    y='instance',
    x='smape',
    hue='instance',
    data=df[df['instance'].isin(ranges)],
    palette=palette_dict,
    linewidth=1.5,
    legend=False
)

for i, model in enumerate(ranges):
    median_val = df[df['instance'] == model]['smape'].median()
    plt.text(
        median_val-0.45,
        i-0.1,
        f'{median_val:.2f}',
        verticalalignment='center',
        fontsize=18,
        fontweight='bold',
        color='black'
    )

plt.xlabel('log(SMAPE)', fontsize=20, fontweight='bold')
plt.ylabel('', fontsize=20, fontweight='bold')
plt.tick_params(axis='x', labelsize=20)
plt.tick_params(axis='y', labelsize=20)

plt.tight_layout()
plt.savefig('figures/fig_smape_violin.pdf')
plt.show()
