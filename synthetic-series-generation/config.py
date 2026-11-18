"""
Simulation parameters for synthetic series generation.
Edit this file to change any aspect of the experiment.
"""

PARAMETERS = {
    "n_replicates": 30,
    "output_dir"  : "synthetic_series",

    "min_spacing": 12,

    "lengths": {
        "mortality"      : 288,
        "hospitalization": 180,
    },

    "noise_by_type": {
        "hospitalization": 0.278,
        "mortality"      : 0.021,
    },

    "magnitude_multiples": {
        "low" : 0.5,
        "high": 2.0,
    },

    "noise_levels": {
        "low" : 0.002,
        "high": 0.006,
    },

    "autocorrelations": {
        "no_ar1"  : 0.0,
        "with_ar1": 0.8,
    },

    "change_types": ["mean", "trend", "variance"],
}
