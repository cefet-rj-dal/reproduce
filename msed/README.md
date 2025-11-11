Multi-Scale Event Detection (MSED)

Paper
- De Salles, D. S., Gea, C., Mello, C. E., Assis, L., Coutinho, R., Bezerra, E., Ogasawara, E. Multi-Scale Event Detection in Financial Time Series. Computational Economics, 2024. DOI: 10.1007/s10614-024-10582-9.

Team
- Diego Silva de Salles (CEFET/RJ), Eduardo Ogasawara (CEFET/RJ), Eduardo Bezerra (CEFET/RJ), Rafaelli Coutinho (CEFET/RJ), Carlos E. Mello (UNIRIO), Cristiane Gea (CEFET/RJ).

Abstract (summary)
Information published in communication media (e.g., government transitions, economic crises, corruption scandals) can drive uncertainty in financial time series at different time scales and manifest as anomalies or change points. MSED detects these events by decomposing the series into IMFs (CEEMD), applying specialized detectors per component group, and validating against EPU labels.

Scripts (semantic order)
- [1-stocks-brazil.R](1-stocks-brazil.R): Evaluate Brazilian stock indices (multiscale detection vs EPU labels).
- [2-stocks-china.R](2-stocks-china.R): Evaluate Chinese stock indices (multiscale detection vs EPU labels).
- [3-stocks-usa.R](3-stocks-usa.R): Evaluate US stock indices (multiscale detection vs EPU labels).
- [4-currencies.R](4-currencies.R): Evaluate currency pairs vs BRL (multiscale detection vs EPU labels).
- [5-synthetic-examples-anomaly.R](5-synthetic-examples-anomaly.R): Synthetic anomaly examples (Harbinger) with multiple soft windows.
- [6-synthetic-cp-variance.R](6-synthetic-cp-variance.R): Synthetic variance change-point examples with multiple soft windows.

Core modules (documented)
- [multi-scale-detect.R](multi-scale-detect.R): Core multiscale pipeline. CEEMD decomposition, event-type routing (anomaly/cp/cp_variance), and SoftED-style evaluation.
- [componentes.R](componentes.R): CEEMD runner + energy-ratio based component grouping (oat, stf, sst, ve, lt).
- [moghtaderi-filter.R](moghtaderi-filter.R): Moghtaderi energy-ratio helpers and split-index logic; builds grouped components from CEEMD results.
- [anomalies.R](anomalies.R): Harbinger-based anomaly wrapper for short-term fluctuation component.
- [functions-models.R](functions-models.R): Helpers for basic outlier detection and SoftED soft-window matching over detected events.
- [utils.R](utils.R): Legacy utility helpers (kept for compatibility); prefer the versions in `moghtaderi-filter.R` when applicable.
- [harbinger-v1.R](harbinger-v1.R): Harbinger wrappers (anomalize, EventDetectR, seminalChangePoint v1/v2, changeFinder v3) with unified I/O.

Run
- From the `msed` folder in R/RStudio, run any of the numbered scripts above (they depend on [multi-scale-detect.R](multi-scale-detect.R) and datasets under [dataset/](dataset/)).
- Outputs: each script writes a CSV with metrics under [files/](files/).

Other methods
- Additional detectors used for comparison are under [other_detection_methods/](other_detection_methods/).

Examples (Harbinger)
- Markdown examples converted from notebooks are available in this folder (illustrative, not required for MSED runs):
  - [har_arima.md](har_arima.md), [har_change_finder_arima.md](har_change_finder_arima.md), [har_change_finder_ets.md](har_change_finder_ets.md), [har_change_point.md](har_change_point.md), [har_datasets.md](har_datasets.md), [har_elm.md](har_elm.md), [har_garch.md](har_garch.md), [har_lstm.md](har_lstm.md), [har_mlp.md](har_mlp.md), [har_rf.md](har_rf.md), [har_softed.md](har_softed.md), [fbiad.md](fbiad.md)

