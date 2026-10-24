"""
Monkey-patch for a GutenTAG off-by-one bug in AnomalyTrend.generate().

int(N*0.2) + int(N*0.8) can differ from N by 1 (e.g. N=143 → 142), causing
an assertion error during generation. Fixed by computing transition_length as
the remainder rather than a separate floor division.
"""

import numpy as _np
from scipy.stats import norm as _norm
from sklearn.preprocessing import MinMaxScaler as _MMS
from gutenTAG.anomalies.types.trend import AnomalyTrend, AnomalyProtocol
from gutenTAG.base_oscillations import RandomModeJump as _RMJ


def _trend_generate_fixed(self, anomaly_protocol: AnomalyProtocol) -> AnomalyProtocol:
    if anomaly_protocol.base_oscillation_kind == _RMJ.KIND:
        self.logger.warn_false_combination(
            self.__class__.__name__, anomaly_protocol.base_oscillation_kind
        )
        return anomaly_protocol

    length            = anomaly_protocol.end - anomaly_protocol.start
    plateau_length    = int(length * 0.8)
    transition_length = length - plateau_length  # avoids the off-by-one

    start_transition = _norm.pdf(_np.linspace(-3, 0, transition_length), scale=1.05)
    amplitude_bell   = _np.concatenate(
        [start_transition / start_transition.max(), _np.ones(plateau_length)]
    )
    amplitude_bell = (
        _MMS(feature_range=(0, 1))
        .fit_transform(amplitude_bell.reshape(-1, 1))
        .reshape(-1)
    )

    self.trend.length = length
    self.trend.generate_timeseries_and_variations(anomaly_protocol.ctx.to_bo())
    timeseries  = self.trend.timeseries
    timeseries *= amplitude_bell
    end_point   = timeseries[-1]

    anomaly_protocol.base_oscillation.trend_series[
        anomaly_protocol.start : anomaly_protocol.end
    ] += timeseries
    anomaly_protocol.base_oscillation.trend_series[anomaly_protocol.end :] += end_point
    return anomaly_protocol


AnomalyTrend.generate = _trend_generate_fixed
