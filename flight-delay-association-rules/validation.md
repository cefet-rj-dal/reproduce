# Validation record

Validation performed on 2026-09-16 using R 4.5.0, arules 1.7.14, and Matrix 1.7-5 on Windows 11 x64.

## Integrity and source review

- Verified source and destination SHA-256 checksums for all 20 entries in `provenance.csv` (18 imported files and two derived CSV exports).
- Parsed all four legacy R scripts and the new runner successfully with R 4.5.0.
- Checked the dataset and mining metadata against the supplied publication, especially Sections 3-4 and Tables 3-5. The recovered experiment is a different version from the final publication.
- Exported the first worksheets of the supporting workbooks: 27 attribute descriptions and six support-experiment rows. Empty worksheets were not exported; the original workbooks remain available.

## Dataset and reference audit

`Rscript scripts/reproduce.R audit` completed successfully:

| Measurement | Observed value |
| --- | ---: |
| Flight rows | 3,006,995 |
| Factor columns | 31 |
| Departure years | 2009-2015 |
| Observed airline codes | 13 |
| Departure / arrival airports | 17 / 17 |
| Departure delay fraction | 0.272626326282551 |
| Arrival delay fraction | 0.289690205670445 |
| Missing cells across all factor columns | 1,377,803 |
| September archived rules | 221,192 |
| APRIORI `rules-3-2.csv` rows | 198 |
| APRIORI `rules-3-3.csv` rows | 10,606 |
| APRIORI `rules-3-4.csv` rows | 121,987 |

The audit validates support/confidence ranges and finite support, confidence, and lift values in all four reference CSV files. Missing factor values are preserved; the transaction conversion omits missing items without discarding their flight rows. No sampling, imputation, or reconstruction of unknown bin boundaries is performed.

## Mining comparison

The length-two run completed successfully on the full dataset, invoked by absolute script path from outside this directory. It regenerated all 479 archived rules of lengths one and two, with the same rule set and a maximum absolute support/confidence/lift difference of `4.884981e-15` (comparison tolerance: `1e-10`). The warning that the configured maximum rule length was reached is expected.

## Scope of validation

The legacy preprocessing scripts and PostgreSQL views were inspected but not executed: their input objects, database, and helper functions are absent. The published final dataset, lift selection, specialized arrival-delay experiments, and figures/tables were not reproduced. Checks of the recovered archive must not be interpreted as validation of the published results.

The installed R environment emitted locale warnings and warnings that some packages were built under a newer R patch release. Actual package versions and execution results are recorded in the generated `output/*/session-info.txt` files.
