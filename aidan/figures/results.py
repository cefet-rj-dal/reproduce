import zipfile
import numpy as np
import pandas as pd
from scipy.stats import wilcoxon

# Effect Size
def effect_size(diff):
    diff = np.array(diff)
    n = len(diff)
    
    # Positive differences get positive ranks, negative get negative ranks
    ranks = np.abs(diff).argsort().argsort() + 1
    ranks = ranks.astype(float)
    ranks = ranks * np.sign(diff)
    W = np.sum(ranks[ranks > 0])  # sum of positive ranks
    
    # Expected mean and variance under H0
    mean_W = n * (n + 1) / 4
    var_W = n * (n + 1) * (2 * n + 1) / 24
    
    # Effect size
    Z = (W - mean_W) / np.sqrt(var_W)
    r = Z / np.sqrt(n)
    
    return r

# Bootstrap CI
def ci(diff, stat_func, n_boot=5000):
    diff = np.array(diff)
    boot_vals = []
    
    for _ in range(n_boot):
        sample = np.random.choice(diff, size=len(diff), replace=True)
        boot_vals.append(stat_func(sample))
    
    boot_vals = np.array(boot_vals)
    low = np.nanpercentile(boot_vals, 2.5)
    high = np.nanpercentile(boot_vals, 97.5)
    
    return low, high
  
# Wilcoxon
def wilcoxon_test(df):
    pairs = [('aidan', 'baseline'), ('aidan', 'naive')]
    results = []

    for a, b in pairs:
        x = df[df['instance'] == a]['smape'].values
        y = df[df['instance'] == b]['smape'].values
        diff = y - x  # positive = sMAPE better
        
        stat, p = wilcoxon(diff, alternative='greater')
        r = effect_size(diff) 
        ci_low, ci_high = ci(diff, effect_size)
        results.append([f"{a} vs {b}", stat, p, r, ci_low, ci_high])

    return pd.DataFrame(results, columns=[
        'pair', 'wilcox_stat', 'p_value',
        'effect_size', 'CI_95_low', 'CI_95_high'
    ])

# Results
def results(strategy, output):
    tmp = df[df['strategy'] == strategy][['dataset', 'instance', 'smape']]
    tmp = tmp.pivot(index='dataset', columns='instance', values='smape')
    tmp = tmp[['aidan', 'baseline', 'naive']]
    tmp.to_csv(output)

# Load Data
with zipfile.ZipFile('combined_results.zip', 'r') as zip_ref:
    with zip_ref.open(zip_ref.namelist()[0]) as file:
        df = pd.read_csv(file, sep=';', decimal=',')

df = df[df['instance'].isin(['aidan', 'baseline', 'naive'])]
df = df[df['test_size'] > 3]
df = df.groupby(['dataset', 'strategy', 'instance'], as_index=False)['smape'].mean()

wilcoxon_test(df).to_csv('figures/wilcoxon_test.csv', index=False)

results('ro', 'figures/smape_results_ro.csv')
results('sa', 'figures/smape_results_sa.csv')
