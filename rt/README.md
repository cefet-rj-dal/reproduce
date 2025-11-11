# RT: Resilient Transformation for Anomaly Detection in Heteroscedastic Time Series

This repository contains the implementation and experimental results of the article "RT: Resilient Transformation for Anomaly Detection in Heteroscedastic Time Series", which proposes a transformation technique (RT) designed to enhance the detection of contextual anomalies in time series with non-constant variance.

## Summary
Heteroscedastic time series exhibit non-constant variance over time, which undermines classical anomaly detection methods based on fixed thresholds. RT modifies the original series using CEEMD decomposition, selection of high-frequency components based on roughness, differentiation, and normalization by local dispersion. This transformation equalizes variance and highlights pointwise deviations. Evaluations using the Yahoo! S5 dataset (via Harbinger) show that RT enhances the performance of classical methods and can be combined with thresholding to form RTAD.

## Repository Structure

### `.R` scripts
- [0-rt-transform.R](0-rt-transform.R): Defines the function that performs the RT transformation on a time series.
- [2-dispersion-measures.R](2-dispersion-measures.R): Implements methods for computing local volatility (instantaneous dispersion).
- [1-experiment-evaluate.R](1-experiment-evaluate.R): Executes anomaly detection methods on the series and computes evaluation metrics.
- [hanr_rtad.R](hanr_rtad.R): RTAD implementation within the Harbinger framework.
- [3-fig-1.R](3-fig-1.R), [4-fig-2.R](4-fig-2.R), [5-fig-3.R](5-fig-3.R), [6-fig-4.R](6-fig-4.R), [7-fig-5.R](7-fig-5.R), [8-fig-6.R](8-fig-6.R), [9-tables-4-5.R](9-tables-4-5.R): Scripts used to generate figures and tables from the article.

### `.csv` files
- [metrics_hetero.csv](metrics_hetero.csv): Evaluation metrics for methods applied to heteroscedastic series, including both the original and RT-transformed versions.
- [metrics_homo.csv](metrics_homo.csv): Evaluation metrics for methods applied to homoscedastic series, including both the original and RT-transformed versions.
- [metrics_dispersion-measures.csv](metrics_dispersion-measures.csv): Performance of RTAD with different local dispersion measures.
- [metrics_thresholds.csv](metrics_thresholds.csv): Performance of RTAD using different thresholding strategies.

## Authors
- Laís Baroni (CEFET/RJ)
- Gustavo Guedes
- Eduardo Mendes
- Carlos Eduardo Mello
- Esther Pacitti
- Fabio Porto
- Rafaelli Coutinho
- Eduardo Ogasawara

