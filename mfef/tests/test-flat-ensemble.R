library(harbinger)
library(daltoolbox)

script_expressions <- parse(file = "scripts/02-run-mfef-synthetic.R")
definitions_needed <- c(
  "%||%",
  "hcp_frozen_detection",
  "fit.hcp_frozen_detection",
  "detect.hcp_frozen_detection",
  "frozen_bank_to_cache",
  "frozen_bank_from_cache",
  "build_flat_ensemble"
)

for (expression in script_expressions) {
  if (
    is.call(expression) &&
      identical(expression[[1]], as.name("<-")) &&
      as.character(expression[[2]]) %in% definitions_needed
  ) {
    eval(expression, envir = .GlobalEnv)
  }
}

series <- sin(seq_len(60) / 6)
frozen_at <- function(position) {
  events <- rep(FALSE, length(series))
  events[position] <- TRUE
  hcp_frozen_detection(data.frame(event = events))
}

ae_bank <- list(
  WAYPOINT_ED = frozen_at(15),
  WAYPOINT_LSTM = frozen_at(16),
  WAYPOINT_CONV = frozen_at(14)
)
cf_bank <- list(
  CF_ARIMA = frozen_at(30),
  CF_LR = frozen_at(31),
  CF_GARCH = frozen_at(29),
  CF_ETS = frozen_at(30)
)
static_bank <- list(
  BINSEG = frozen_at(45),
  PELT = frozen_at(44),
  GFT = frozen_at(46),
  SCP = frozen_at(45),
  BOCPD = frozen_at(44)
)

cache_roundtrip <- frozen_bank_from_cache(
  frozen_bank_to_cache(c(ae_bank, cf_bank, static_bank))
)
stopifnot(
  length(cache_roundtrip) == 12L,
  identical(cache_roundtrip$WAYPOINT_ED$events, ae_bank$WAYPOINT_ED$events),
  identical(cache_roundtrip$CF_ARIMA$score, cf_bank$CF_ARIMA$score),
  identical(cache_roundtrip$PELT$events, static_bank$PELT$events)
)

flat_model <- build_flat_ensemble(ae_bank, cf_bank, static_bank)
flat_model <- fit(flat_model, series)
flat_detection <- detect(
  flat_model,
  series,
  time_tolerance = 3,
  threshold = 0.90,
  threshold_type = "percentile",
  use_nms = TRUE
)

stopifnot(
  nrow(flat_detection) == length(series),
  "event" %in% names(flat_detection)
)

cat(
  "FLAT_12_DETECTOR_TEST_OK events=",
  sum(flat_detection$event),
  "\n",
  sep = ""
)
