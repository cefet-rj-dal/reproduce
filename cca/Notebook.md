# Contextual–Compositional Analysis (Markdown Export)

This Markdown file summarizes the companion notebook `Notebook.ipynb` with executable R code blocks. Outputs/tables are omitted for brevity.

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

## 3. Inspect Data

```r
head(data)
summary(data)
```

## 4. Example Feature Engineering and Grouping

```r
by_facility <- data %>%
  group_by(local) %>%
  summarize(count = n(), zeros = sum(tmn == 0, na.rm = TRUE), .groups = "drop") %>%
  mutate(perc = zeros / count)

set.seed(123)
km <- kmeans(by_facility$perc, centers = 3)
by_facility$cat <- km$cluster
```

## 5. Join Back and Visualize

```r
classified <- merge(data, by_facility[, c("local","cat")], by = "local", all.x = TRUE)

library(reshape2)

ex_ids <- head(unique(classified$local), 3)

df_list <- lapply(ex_ids, function(id) {
  tmp <- subset(classified, local == id, select = c(time, tmn))
  names(tmp) <- c("time", paste0(id))
  tmp
})

plot_df <- Reduce(function(x,y) merge(x, y, by = "time", all = TRUE), df_list)
plot_long <- melt(plot_df, id = c("time"))

ggplot(plot_long, aes(x = time, y = value, group = variable)) +
  geom_line() +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  theme(legend.position = "none") +
  xlab("") + ylab("NMR")
```

## 6. Save Artifacts (optional)

```r
# saveRDS(by_facility, file = "by_facility.rds")
# write.csv(by_facility, file = "by_facility.csv", row.names = FALSE)
```

---

For richer context, see the Jupyter notebooks and the scripted workflow in `1-classify-cnes-by-nmr-zeros.R`.
