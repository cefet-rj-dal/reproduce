import zipfile
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
from matplotlib import colormaps


def mean_by(df, strategy, col):
    return (df[df['strategy'] == strategy].groupby(col)['smape'].mean().reset_index())

def add_reference_lines(ax, refs):
    ax.axhline(refs['arima'],        linestyle='--', linewidth=2.5, color='#d62728', label='ARIMA')
    ax.axhline(refs['arima-garch'],  linestyle='--', linewidth=2.5, color='#ff7f0e', label='ARIMA-GARCH')
    ax.axhline(refs['ses'],          linestyle='--', linewidth=2.5, color='#8c564b', label='Exponential Smoothing')

def format_augment(df, col='augment'):
    out = df.copy()
    out = pd.concat([out[out[col] == 'none'], out[out[col] != 'none']], ignore_index=True)
    out[col] = out[col].replace('none', 'No DA')
    out[col] = out[col].apply(lambda x: x.capitalize() if x != 'No DA' else x)
    return out

def format_model(df, col='model'):
    out = df.copy()
    out[col] = out[col].str.upper()
    return out

def get_colors(valores, cmap_name='summer'):
    cmap = colormaps[cmap_name]
    norm = plt.Normalize(vmin=np.min(valores), vmax=np.max(valores))
    return cmap(norm(valores))

with zipfile.ZipFile('combined_results.zip', 'r') as zip_ref:
    with zip_ref.open(zip_ref.namelist()[0]) as file:
        df = pd.read_csv(file, sep=';', decimal=',')

ref_instances = ['arima', 'arima-garch', 'ses']
df_ref = df[(df['instance'].isin(ref_instances)) & (df['test_size'] > 0)]
ref_lines = (
    df_ref
    .groupby(['strategy', 'instance'])['smape']
    .mean()
    .unstack()
)
ref_ro = ref_lines.loc['ro']
ref_sa = ref_lines.loc['sa']

df = df[~df['instance'].isin(['aidan', 'baseline', 'naive', 'arima', 'arima-garch', 'ses'])]
ro_da = format_augment(mean_by(df, 'ro', 'augment'))
ro_dn = mean_by(df, 'ro', 'preprocess')
ro_ml = format_model(mean_by(df, 'ro', 'model'))
sa_da = format_augment(mean_by(df, 'sa', 'augment'))
sa_dn = mean_by(df, 'sa', 'preprocess')
sa_ml = format_model(mean_by(df, 'sa', 'model'))

fig, axs = plt.subplots(2, 3, figsize=(18, 13))
formatter = FuncFormatter(lambda y, _: f'{int(y)}')
for ax in axs.flat:
    ax.yaxis.set_major_formatter(formatter)

colors_da_ro = get_colors(ro_da['smape'])
axs[0, 0].bar(ro_da['augment'], ro_da['smape'], color=colors_da_ro)
axs[0, 0].text(0.5, 0.97, 'DA', transform=axs[0, 0].transAxes, ha='center', va='top', fontsize=32)
axs[0, 0].set_ylabel('SMAPE (%)', size=22, labelpad=15)
axs[0, 0].set_ylim(0, 30)
for i, val in enumerate(ro_da['smape']):
    axs[0, 0].text(i, val + 0.8, f'{val:.2f}', ha='center', fontsize=20, rotation=45)
axs[0, 0].set_xticks(range(len(ro_da['augment'])))
axs[0, 0].set_xticklabels(ro_da['augment'], fontsize=20, rotation=45)
axs[0, 0].tick_params(axis='y', labelsize=18)
axs[0, 0].set_yticks(range(0, 30, 5))
add_reference_lines(axs[0, 0], ref_ro)

colors_dn_ro = get_colors(ro_dn['smape'])
axs[0, 1].bar(ro_dn['preprocess'], ro_dn['smape'], color=colors_dn_ro)
axs[0, 1].text(0.5, 0.97, 'DN', transform=axs[0, 1].transAxes, ha='center', va='top', fontsize=32)
axs[0, 1].set_ylim(0, 30)
for i, val in enumerate(ro_dn['smape']):
    axs[0, 1].text(i, val + 0.8, f'{val:.2f}', ha='center', fontsize=20, rotation=45)
axs[0, 1].set_xticks(range(len(ro_dn['preprocess'])))
axs[0, 1].set_xticklabels(ro_dn['preprocess'], fontsize=20, rotation=45)
axs[0, 1].tick_params(axis='y', labelsize=18)
axs[0, 1].set_yticks(range(0, 30, 5))
add_reference_lines(axs[0, 1], ref_ro)

colors_ml_ro = get_colors(ro_ml['smape'])
axs[0, 2].bar(ro_ml['model'], ro_ml['smape'], color=colors_ml_ro)
axs[0, 2].text(0.5, 0.97, 'ML', transform=axs[0, 2].transAxes, ha='center', va='top', fontsize=32)
axs[0, 2].set_ylim(0, 30)
for i, val in enumerate(ro_ml['smape']):
    axs[0, 2].text(i, val + 0.8, f'{val:.2f}', ha='center', fontsize=20, rotation=45)
axs[0, 2].set_xticks(range(len(ro_ml['model'])))
axs[0, 2].set_xticklabels(ro_ml['model'], fontsize=20, rotation=45)
axs[0, 2].tick_params(axis='y', labelsize=18)
axs[0, 2].set_yticks(range(0, 30, 5))
add_reference_lines(axs[0, 2], ref_ro)

colors_da_sa = get_colors(sa_da['smape'])
axs[1, 0].bar(sa_da['augment'], sa_da['smape'], color=colors_da_sa)
axs[1, 0].text(0.5, 0.97, 'DA', transform=axs[1, 0].transAxes, ha='center', va='top', fontsize=32)
axs[1, 0].set_ylabel('SMAPE (%)', size=22, labelpad=15)
axs[1, 0].set_ylim(0, 30)
for i, val in enumerate(sa_da['smape']):
    axs[1, 0].text(i, val + 0.8, f'{val:.2f}', ha='center', fontsize=20, rotation=45)
axs[1, 0].set_xticks(range(len(sa_da['augment'])))
axs[1, 0].set_xticklabels(sa_da['augment'], fontsize=20, rotation=45)
axs[1, 0].tick_params(axis='y', labelsize=18)
axs[1, 0].set_yticks(range(0, 30, 5))
add_reference_lines(axs[1, 0], ref_sa)

colors_dn_sa = get_colors(sa_dn['smape'])
axs[1, 1].bar(sa_dn['preprocess'], sa_dn['smape'], color=colors_dn_sa)
axs[1, 1].text(0.5, 0.97, 'DN', transform=axs[1, 1].transAxes, ha='center', va='top', fontsize=32)
axs[1, 1].set_ylim(0, 30)
for i, val in enumerate(sa_dn['smape']):
    axs[1, 1].text(i, val + 0.8, f'{val:.2f}', ha='center', fontsize=20, rotation=45)
axs[1, 1].set_xticks(range(len(sa_dn['preprocess'])))
axs[1, 1].set_xticklabels(sa_dn['preprocess'], fontsize=20, rotation=45)
axs[1, 1].tick_params(axis='y', labelsize=18)
axs[1, 1].set_yticks(range(0, 30, 5))
add_reference_lines(axs[1, 1], ref_sa)

colors_ml_sa = get_colors(sa_ml['smape'])
axs[1, 2].bar(sa_ml['model'], sa_ml['smape'], color=colors_ml_sa)
axs[1, 2].text(0.5, 0.97, 'ML', transform=axs[1, 2].transAxes, ha='center', va='top', fontsize=32)
axs[1, 2].set_ylim(0, 30)
for i, val in enumerate(sa_ml['smape']):
    axs[1, 2].text(i, val + 0.8, f'{val:.2f}', ha='center', fontsize=20, rotation=45)
axs[1, 2].set_xticks(range(len(sa_ml['model'])))
axs[1, 2].set_xticklabels(sa_ml['model'], fontsize=20, rotation=45)
axs[1, 2].tick_params(axis='y', labelsize=18)
axs[1, 2].set_yticks(range(0, 30, 5))
add_reference_lines(axs[1, 2], ref_sa)

axs[0, 0].text(0.01, 1.05, 'Rolling Origin:', ha='left', va='center',
               transform=axs[0, 0].transAxes, size=24, color='black', fontweight='bold')
axs[1, 0].text(0.01, 1.05, 'Steps Ahead:', ha='left', va='center',
               transform=axs[1, 0].transAxes, size=24, color='black', fontweight='bold')

for valores, ax in [
    (ro_da['smape'], axs[0, 0]),
    (ro_dn['smape'], axs[0, 1]),
    (ro_ml['smape'], axs[0, 2]),
    (sa_da['smape'], axs[1, 0]),
    (sa_dn['smape'], axs[1, 1]),
    (sa_ml['smape'], axs[1, 2]),
]:
    idx = np.argmin(valores)
    ax.patches[idx].set_edgecolor('black')
    ax.patches[idx].set_linewidth(3)

for ax in axs.flat:
    for spine in ax.spines.values():
        spine.set_edgecolor('darkgrey')

handles, labels = axs[0, 0].get_legend_handles_labels()
fig.legend(handles, labels, loc='lower center', ncol=3, fontsize=24, frameon=False, bbox_to_anchor=(0.5, 0.025))
plt.tight_layout(rect=[0, 0.08, 1, 1])
plt.subplots_adjust(left=0.06, wspace=0.1, hspace=0.4)
plt.savefig('figures/fig_smape_methods.pdf')
plt.show()
