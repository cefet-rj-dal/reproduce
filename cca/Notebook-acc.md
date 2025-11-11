# Contextual–Compositional Associations for NMR Monitoring (Markdown Export)

This Markdown file summarizes the companion notebook `Notebook-acc.ipynb` with executable R code blocks. Outputs and rich widgets are omitted for brevity.

Purpose: exploratory monitoring of Neonatal Mortality Rate (NMR) under anomalies using a contextual–compositional approach.

Data: expects a preloaded data.frame (e.g., `df` from `df.RData`) with columns such as `local` (facility id), `time` (Date), `tmn` (NMR), and optional anomaly flags (`anom.p`, `anom.t`, `anom.c`).

Guide: 1) Load packages and data; 2) Inspect dataset; 3) Group facilities by NMR patterns (e.g., zero‑rate proportion); 4) Visualize examples and anomaly markers; 5) Summaries/exports.

## 1. Setup

```r
suppressPackageStartupMessages({
  library("ggplot2")
  library("dplyr")
  library("reshape")
  library("tidyr")
})
```

## 2. Load Data

Adjust the path to your local environment as needed.

```r
load("~/acc/df.RData")
stopifnot(exists("df"))

data <- df
```

## 3. Quick Inspection

```r
# Glimpse first rows and structure
head(data)
str(data)
```

## 4. Zero‑Rate Grouping (illustrative)

This mirrors the logic detailed in `1-classify-cnes-by-nmr-zeros.R`.

```r
library(dplyr)

by_facility <- data %>%
  group_by(local) %>%
  summarize(count = n(), zeros = sum(tmn == 0, na.rm = TRUE), .groups = "drop") %>%
  mutate(perc = zeros / count)

set.seed(123)
km <- kmeans(by_facility$perc, centers = 3)
by_facility$cat <- km$cluster

# Relabel clusters so 1 has highest zero-rate
by_facility$cat <- gsub("2", "4", by_facility$cat)
by_facility$cat <- gsub("3", "2", by_facility$cat)
by_facility$cat <- gsub("4", "3", by_facility$cat)

table(by_facility$cat)

# Merge back to the time series
classified <- merge(data, by_facility[, c("local","cat")], by = "local", all.x = TRUE)
```

## 5. Visualization Examples

```r
# Example facility IDs are dataset-specific; adjust as needed
ex_ids <- c("2290227","2268922","5042488")

df_list <- lapply(ex_ids, function(id) {
  tmp <- subset(data, local == id, select = c(time, tmn))
  nm  <- paste0(id)
  names(tmp) <- c("time", nm)
  tmp
})

plot_df <- Reduce(function(x,y) merge(x, y, by = "time", all = TRUE), df_list)
plot_long <- reshape2::melt(plot_df, id = c("time"))

ggplot(plot_long, aes(x = time, y = value, group = variable)) +
  geom_line() +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "#8C9EFF"),
        axis.text = element_text(size = 12)) +
  xlab("") + ylab("NMR")
```

Annotate anomalies if the flags exist in your data frame:

```r
id <- ex_ids[[1]]
sb <- subset(data, local == id)

ggplot(sb, aes(x = time, y = tmn)) +
  geom_line() +
  geom_vline(xintercept = sb$time[sb$anom.p %in% TRUE], linetype = "dashed", color = "blue", size = .7) +
  geom_vline(xintercept = sb$time[sb$anom.t %in% TRUE], linetype = "dashed", color = "red",  size = .7) +
  geom_vline(xintercept = sb$time[sb$anom.c %in% TRUE], linetype = "dashed", color = "green",size = .7) +
  ggtitle(id)
```

## 6. Save/Export (optional)

```r
# saveRDS(by_facility, file = "cnes_zero_rate_clusters.rds")
# write.csv(by_facility, file = "cnes_zero_rate_clusters.csv", row.names = FALSE)
```

---

For a more complete and scripted version of the grouping/plots, see `1-classify-cnes-by-nmr-zeros.R`.

*** End of Markdown export ***
