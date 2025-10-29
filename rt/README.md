# RT: Resilient Transformation for Anomaly Detection in Heteroscedastic Time Series

This repository contains the implementation and experimental results of the article **"RT: Resilient Transformation for Anomaly Detection in Heteroscedastic Time Series"**, which proposes a transformation technique (RT) designed to enhance the detection of contextual anomalies in time series with non-constant variance.

## Summary

Heteroscedastic time series exhibit non-constant variance over time, which undermines the effectiveness of classical anomaly detection methods based on fixed thresholds. This issue particularly affects the identification of contextual anomalies, whose detection depends on local variability.

To address this challenge, this work proposes the **Resilient Transformation (RT)**, which modifies the original series using CEEMD decomposition, selection of high-frequency components based on roughness, differentiation, and normalization by local dispersion. This transformation equalizes variance and highlights pointwise deviations.

Evaluations using the Yahoo! S5 dataset (via the Harbinger framework) show that RT enhances the performance of classical methods and can be combined with thresholding to form an autonomous detection method, **RTAD**. Sensitivity analyses indicate that the use of standard deviation and a Gaussian threshold yields the best results.

RT can be applied both as a preprocessing step or as a complete detection procedure.

---

## 📁 Repository Structure

### `.R` scripts

- **`cod_RT.R`**: Defines the function that performs the RT transformation on a time series.
- **`cod_dispersion-measures.R`**: Implements methods for computing local volatility (instantaneous dispersion).
- **`cod_exp-eva.R`**: Executes anomaly detection methods on the series and computes evaluation metrics.
- **`hanr_RTAD.R`**: RTAD implementation within the Harbinger framework.
- **`FIG1.R`, `FIG2.R`, `FIG3.R`, `FIG4.R`, `FIG5.R`, `FIG6.R` and `TAB4&5`**: Scripts used to generate figures and tables from the article.

### `.csv` files

- **`metrics_hetero.csv`**: Evaluation metrics for methods applied to heteroscedastic series, including both the original and RT-transformed versions.
- **`metrics_homo.csv`**: Evaluation metrics for methods applied to homoscedastic series, including both the original and RT-transformed versions.
- **`metrics_dispersion-measures.csv`**: Performance of RTAD with different local dispersion measures.
- **`metrics_thresholds.csv`**: Performance of RTAD using different thresholding strategies.

---

## 👥 Authors

- **Laís Baroni**  
  Federal Center for Technological Education of Rio de Janeiro (CEFET/RJ)  
  [lais.baroni@eic.cefet-rj.br](mailto:lais.baroni@eic.cefet-rj.br)
- **Gustavo Guedes**
- **Eduardo Mendes**
- **Carlos Eduardo Mello**
- **Esther Pacitti**
- **Fabio Porto**
- **Rafaelli Coutinho**
- **Eduardo Ogasawara**
