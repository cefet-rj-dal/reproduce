# Flight Delay Association Rules

Reproduction materials for Alice Sternberg's work:

> Sternberg, A., Carvalho, D., Murta, L., Soares, J., and Ogasawara, E. (2016). **An analysis of Brazilian flight delays based on frequent patterns**. *Transportation Research Part E: Logistics and Transportation Review*, **95**, 282-298. [DOI: 10.1016/j.tre.2016.09.013](https://doi.org/10.1016/j.tre.2016.09.013).

This directory contains the recovered flight dataset, R and SQL research scripts, archived association rules, and a portable R runner. The materials were recovered from `Alumni/alumni-mestrado/2016-asternberg` and checked against the supplied published paper.

**Reproduction status:** the September 2015 experiment can be rerun from the included data. Exact reproduction of the final paper is not currently possible: the recovered dataset is an earlier version, and the final integrated flight/weather dataset and complete preprocessing pipeline were not found in the supplied directory. Do not report results from this archive as a replication of the published tables.

## Contents

| Path | Purpose |
| --- | --- |
| `scripts/reproduce.R` | New portable runner: audit the recovered files, mine association rules, and compare them with archived results. |
| `data/kif.RData` | Original September 2015 `kif` data frame: 3,006,995 rows and 31 factor columns. |
| `data/columns.csv` | Original-to-English column mapping and descriptions. |
| `legacy/*.R` | Four recovered research scripts; comments translated into English, original logic and identifiers retained. |
| `legacy/SQLs/*.sql` | Six original PostgreSQL view definitions. |
| `reference/september-2015/rules.csv` | 221,192 archived rules, including support, confidence, and lift. |
| `reference/september-2015/rules.RData` | The same experiment's serialized `rulesatraso` arules object, including mining metadata. |
| `reference/apriori/rules-3-{2,3,4}.csv` | Rule exports from another experiment version, with English attribute names; its input dataset was not recovered. |
| `reference/apriori/attributes.csv`, `support-summary.csv` | English CSV exports of the supporting workbooks' first worksheets. |
| `reference/workbooks/` | Original attribute inventory and support/length experiment summary, renamed in English. |
| `provenance.csv` | Source-relative paths, byte counts, SHA-256 checksums, and import transformations. |
| `output/` | Generated audits, rules, comparison reports, and R session details; ignored by Git. |

The imported files occupy approximately 96 MB. No raw flight observations were invented or substituted. Archived data, rule expressions, database identifiers, and workbook cells retain their original encoding and labels for traceability; the runner exposes English column names and output. The English dictionary explains the original fields. The paper itself is not redistributed here.

## Run the recovered experiment

Use R with `arules` and its dependencies. The audit uses base R only. From this directory:

```sh
Rscript -e "install.packages('arules', repos='https://cloud.r-project.org')"
Rscript scripts/reproduce.R audit
Rscript scripts/reproduce.R mine 2
```

The script also works when invoked by absolute path from another directory. Input and output paths are resolved relative to the script, so no original drive letters or database connection are required.

The audit writes a data dictionary with observed factor values, missing-value counts, dataset size, delay rates, and counts by year, airline, and airport. It also checks all four archived rule CSV files for valid numerical metrics.

The first mining run uses the **entire recovered dataset**, with maximum rule length two. To regenerate all archived rule lengths:

```sh
Rscript scripts/reproduce.R mine 4
```

Allow several GB of free memory for the full 3-million-row transaction matrix and rule mining. The runner does not sample observations. Each run writes to `output/maxlen-2/` or `output/maxlen-4/`:

- `rules.csv` and `rules.rds`: newly mined rules with English attribute names;
- `parameters.csv`: actual support, confidence, maximum length, and dataset size;
- `top-rules.txt`: the 20 highest-lift rules;
- `archive-comparison.csv`: rule-set equality and largest absolute difference in support, confidence, or lift;
- `session-info.txt`: installed R and package versions.

The comparison normalizes column names and antecedent item order, then compares against the corresponding lengths of the archived CSV. A mismatch produces a nonzero exit status after saving the diagnostic results. Repeating a run overwrites its generated output directory's files.

### Parameters and expected archived results

The recovered `rulesatraso` object records support `0.001`, confidence `0.272626326282551`, and 3,006,995 transactions. The runner computes that confidence from the recovered departure-delay indicator. It permits both departure-delay and arrival-delay consequents, rule lengths one through the requested maximum, and preserves all archived categories. Length-one rules have empty antecedents and are retained because they occur in the archive.

| Exact rule length, including consequent | Archived count |
| --- | ---: |
| 1 | 2 |
| 2 | 477 |
| 3 | 34,930 |
| 4 | 185,783 |
| Total | 221,192 |

Thus the length-two run should match 479 archived rules, including the two empty-antecedent rules. The full run should match 221,192. These are historical experiment counts, **not Table 5 of the paper**. No final-paper lift filter is applied by this runner.

## Published methodology and differences from the archive

The following study specifications come from Sections 3-4 and Tables 3-5 of the supplied paper:

- Period: January 2009 through February 2015; domestic, regular flights.
- Airlines: TAM, Gol, Avianca, and Azul.
- Airports: SBSP, SBGR, SBBR, SBGL, SBRJ, SBCF, SBKP, SBSV, SBCT, SBPA, SBRF, SBFZ, SBVT, SBFL, SBBE, SBGO, and SBEG.
- Sources: ANAC VRA flight operations, Weather Underground observations matched by airport and nearest observation to the scheduled departure/arrival time, and national/regional holidays.
- Final dataset: **2,818,898 flights**, with 12 indexed and four original attributes for the main analysis.
- Departure delay: cancellations and delays greater than 15 minutes. The prose also uses "at least fifteen minutes"; the recovered preprocessing script uses the strict `> 15` comparison and treats missing delay times as delayed.
- Indexing: temporal categories, wind direction, Beaufort wind categories, IFR/VFR visibility, and three-level temperature, humidity, pressure, and airport-delay bins.
- Airport delay level: fraction of delayed departures and arrivals in the preceding hour, followed by binning.
- Main mining: Apriori (`arules`), departure-delay consequent, confidence `0.2276`, support approximately `0.00077` (one occurrence per day over six years), lengths two through four.
- Rule analysis: lift-based selection, using the third quartile and improvement over less restrictive rules. Table 5 reports 64, 2,367, and 21,906 generated rules of lengths two, three, and four; interesting-rule counts are 64, 1,230, and 6,662. The available scripts do not recover the complete selection procedure needed to regenerate those counts.

| Property | Published study | Recovered September dataset |
| --- | --- | --- |
| Airline codes | Four selected airlines | 13 observed codes |
| Flight rows | 2,818,898 | 3,006,995 |
| Departure delay fraction | 22.76% | Approximately 27.2626% |
| Attributes | Final semantic categories, including visibility/weather conditions | 31 factor columns, including numerical bin identifiers and wet-bulb temperature |
| Original continuous measurements | Used for preprocessing | Not included |
| Saved mining support | Approximately 0.00077 | 0.001 |
| Saved consequents | Departure delay for main analysis | Departure and arrival delay |

The numerical bin labels in `kif` cannot be mapped back to the paper's low/medium/high categories without their original boundaries. In particular, missing visibility and weather-condition attributes cannot be reconstructed from cloud-cover bins. Changing thresholds or relabeling these bins would not turn this archive into the final dataset.

The `APRIORI` CSV files are a separate version: their empty-antecedent departure-delay rule has confidence approximately `0.2713486`. The support-summary workbook refers to **2,798,002** records. Neither matches the final paper or the September dataset; do not mix their rules and inputs.

## Recovered preprocessing code

These files document the research process; they are **not a standalone raw-data pipeline** and are not called by `scripts/reproduce.R`.

| File | Behavior and prerequisites |
| --- | --- |
| `legacy/atraso15_completo.R` | Requires existing `v` flight and `aeroporto` airport objects. Creates delay flags, computes preceding-hour airport delay fractions, and assigns time/weather bins. Requires an unavailable `remove_outliers()` helper and `curvature.max()`. Saves to the historical absolute path `/home/anac/voo_new/voo.RData`. |
| `legacy/fight-quantis.R` | Explores quantile bins for dry-bulb temperature in an existing `k_f` object; also requires `remove_outliers()`. |
| `legacy/fight-binning.R` | Converts existing `k_f` delay fractions to three bins, using 0.1538462 and 0.3333333; these differ from the later preprocessing script's 0.1176471 and 0.30. |
| `legacy/fight-apriori.R` | Requires `flight_apriori`, `arules`, `data.table`, and `tic()`/`toc()` helpers. Sweeps 16 supports (`365 * 6 * s / N`) and maximum lengths two through four, fixes `delay_dep=1` as consequent, and prints a summary. It does not save rule exports or implement the complete published lift selection. |
| `legacy/SQLs/` | PostgreSQL views over the unavailable `voo_r` relation. Create `v_window_departure` before `v_window`, then `v_high_percentage`; the travel views independently depend on `voo_r`. Ownership statements require the historical `anac` role. |

Original filenames and identifiers are kept so the source mapping remains unambiguous. The SQL files contain no database dump or connection credentials. Some SQL delay definitions differ from the paper's 15-minute threshold; these views should not be assumed to implement the published final preprocessing. The R rolling-window code also limits its search to the preceding 300 rows and reassembles airport subsets; it has been preserved rather than silently corrected.

## What is still needed for exact paper reproduction

The supplied research directory does not include:

1. The final 2,818,898-row integrated dataset (or the exact ANAC, Weather Underground, and holiday snapshots from which it was built).
2. The database schema/data for `voo_r`, the raw `v`/`k_f` objects, or a script producing the final `flight_apriori` object.
3. Complete cleaning, outlier removal, weather matching, and final feature-selection code, including the missing helper definitions.
4. The final rule exports, complete lift-filter implementation, and scripts generating all published figures, tables, and specialized arrival-delay analyses.

Recovering the final indexed dataset would permit rerunning the main Apriori stage using the paper's parameters. Reproducing the entire workflow from raw records also requires the source snapshots and missing preprocessing code. The current runner deliberately reproduces the available historical experiment and checks it against its own archived results.

## Provenance and verification

`provenance.csv` records paths relative to the supplied Alice research directory. Original data and reference imports are byte-for-byte copies; R script comments were translated, and their original and destination hashes are both recorded. The two derived workbook CSV exports are separately identified as transformations. Redundant Excel rule workbooks, thesis drafts, presentations, and compiled LaTeX files were excluded. The two small supporting workbooks are retained as original evidence; use their English CSV exports for reading, while the original workbook cells remain unchanged.

To inspect the serialized historical mining metadata directly:

```r
library(arules)
archive <- new.env()
load("reference/september-2015/rules.RData", envir = archive)
summary(archive$rulesatraso)
```

See [validation.md](validation.md) for the checks actually performed on this reproduction package.
