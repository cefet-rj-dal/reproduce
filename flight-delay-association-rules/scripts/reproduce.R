# Run from any working directory: Rscript path/to/scripts/reproduce.R audit|mine [maxlen]
# This runner reproduces the recovered September 2015 experiment, not the final paper.
args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."),
                      mustWork = TRUE)
mode <- if (length(args)) args[1] else "audit"
if (!mode %in% c("audit", "mine")) stop("Mode must be audit or mine.")
maxlen <- if (length(args) >= 2L) suppressWarnings(as.integer(args[2])) else 2L
if (is.na(maxlen) || !maxlen %in% 2:4) stop("Maximum rule length must be 2, 3, or 4.")
out <- file.path(root, "output", if (mode == "audit") "audit" else paste0("maxlen-", maxlen))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
env <- new.env()
load(file.path(root, "data", "kif.RData"), envir = env)
if (!exists("kif", envir = env, inherits = FALSE)) stop("Missing archived kif object.")
flights <- env$kif
dictionary <- read.csv(file.path(root, "data", "columns.csv"), stringsAsFactors = FALSE)
stopifnot(is.data.frame(flights), nrow(flights) == 3006995L,
          identical(names(flights), dictionary$original))
names(flights) <- dictionary$english
rm(env)
baseline <- mean(flights$delay_dep == "1")
if (!is.finite(baseline)) stop("Departure delay values contain missing data.")
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))

if (mode == "audit") {
  profile <- data.frame(dictionary,
    class = vapply(flights, function(x) class(x)[1], character(1)),
    missing = vapply(flights, function(x) sum(is.na(x)), integer(1)),
    observed_values = vapply(flights, function(x)
      paste(sort(unique(as.character(x))), collapse = " | "), character(1)))
  write.csv(profile, file.path(out, "data-profile.csv"), row.names = FALSE)
  metrics <- data.frame(metric = c("rows", "columns", "departure_delay_fraction",
                                    "arrival_delay_fraction"),
    value = c(nrow(flights), ncol(flights), baseline, mean(flights$delay_arr == "1")))
  write.csv(metrics, file.path(out, "dataset-summary.csv"), row.names = FALSE)
  for (field in c("year_dep", "carrier", "airport_dep", "airport_arr")) {
    write.csv(as.data.frame(table(flights[[field]], useNA = "ifany")),
              file.path(out, paste0(field, "-counts.csv")), row.names = FALSE)
  }
  for (path in c("reference/september-2015/rules.csv",
                paste0("reference/apriori/rules-3-", 2:4, ".csv"))) {
    rules <- read.csv(file.path(root, path), row.names = 1, stringsAsFactors = FALSE)
    stopifnot(all(c("rules", "support", "confidence", "lift") %in% names(rules)),
              all(is.finite(as.matrix(rules[c("support", "confidence", "lift")]))),
              all(rules$support > 0 & rules$support <= 1),
              all(rules$confidence > 0 & rules$confidence <= 1))
    cat(path, ":", nrow(rules), "archived rules\n")
  }
  print(metrics)
  cat("Audit complete. This archive differs from the final publication.\n")
} else {
  if (!requireNamespace("arules", quietly = TRUE))
    stop("Install arules first: install.packages('arules')")
  # Renaming columns preserves categories and missing values; no re-binning or sampling.
  transactions <- methods::as(flights, "transactions")
  rm(flights)
  gc()
  rules <- arules::apriori(transactions,
    parameter = list(support = 0.001, confidence = baseline,
                     minlen = 1L, maxlen = maxlen, target = "rules", maxtime = 0),
    appearance = list(rhs = c("delay_dep=1", "delay_arr=1"), default = "lhs"),
    control = list(verbose = TRUE))
  result <- methods::as(rules, "data.frame")
  write.csv(result, file.path(out, "rules.csv"), row.names = FALSE)
  saveRDS(rules, file.path(out, "rules.rds"))
  top <- rules[head(order(arules::quality(rules)$lift, decreasing = TRUE), 20)]
  writeLines(capture.output(arules::inspect(top)),
             file.path(out, "top-rules.txt"))
  write.csv(data.frame(support = 0.001, confidence = baseline, minlen = 1,
                       maxlen = maxlen, transactions = length(transactions),
                       rules = length(rules)), file.path(out, "parameters.csv"), row.names = FALSE)
  # Compare rule sets and numerical quality measures, independent of output ordering.
  reference <- read.csv(file.path(root, "reference/september-2015/rules.csv"),
                        row.names = 1, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(dictionary))) {
    reference$rules <- gsub(paste0(dictionary$original[i], "="),
                            paste0(dictionary$english[i], "="),
                            reference$rules, fixed = TRUE)
  }
  rule_key <- function(x) vapply(strsplit(x, " => ", fixed = TRUE), function(parts) {
    lhs <- strsplit(gsub("[{}]", "", parts[1]), ",", fixed = TRUE)[[1]]
    paste(paste(sort(trimws(lhs)), collapse = ","), parts[2], sep = " => ")
  }, character(1))
  reference_length <- 1L + vapply(strsplit(reference$rules, " => ", fixed = TRUE),
    function(parts) {
      lhs <- gsub("[{}]", "", parts[1])
      if (!nzchar(lhs)) 0L else length(strsplit(lhs, ",", fixed = TRUE)[[1]])
    }, integer(1))
  reference <- reference[reference_length <= maxlen, ]
  keys <- rule_key(result$rules)
  ref_keys <- rule_key(reference$rules)
  matched <- match(ref_keys, keys)
  same_set <- !anyDuplicated(keys) && !anyDuplicated(ref_keys) &&
    length(keys) == length(ref_keys) && !anyNA(matched)
  error <- if (same_set) max(abs(as.matrix(reference[c("support", "confidence", "lift")]) -
    as.matrix(result[matched, c("support", "confidence", "lift")]))) else NA_real_
  comparison <- data.frame(generated_rules = nrow(result), archived_rules = nrow(reference),
    same_rule_set = same_set, max_absolute_metric_error = error,
    matches_archive = same_set && is.finite(error) && error < 1e-10)
  write.csv(comparison, file.path(out, "archive-comparison.csv"), row.names = FALSE)
  print(comparison)
  writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
  if (!comparison$matches_archive) stop("Mining finished, but differs from the archived rules. See comparison output.")
}
cat("Results:", out, "\n")
