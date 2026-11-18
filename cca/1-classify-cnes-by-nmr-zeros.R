#!/usr/bin/env Rscript
# Classifying maternity hospitals by NMR zero-rate pattern
# -------------------------------------------------------
# This script groups maternity hospitals (CNES units) by the proportion
# of months with zero neonatal mortality rate (NMR). It then:
# - computes per-facility counts and zero counts
# - clusters facilities (k=3) by zero-rate percentage
# - reorders cluster labels so 1 has the highest zero rate, 3 the lowest
# - shows example time series per cluster and draws anomaly markers
#
# Data inputs
# - Expects an object `rmrj` in memory (loaded from an RData file) with columns:
#   local (facility id), time (Date), tmn (NMR), anom.p, anom.t, anom.c
# - Note: the original code loaded from a user path (e.g., "~/LH/rmRJ.RData").
#   Adjust your data loading before running, e.g.:
#   load("path/to/rmRJ.RData"); stopifnot(exists("rmrj"))
#
# Outputs
# - Data frame with cluster assignment per facility (cnes_cat)
# - Example plots with time series per cluster

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  library(stats)
})

if (!exists("rmrj")) {
  stop("Data object `rmrj` not found. Load it before running this script.")
}

# Source dataset (renamed for clarity)
data_in <- rmrj

# Per-facility sample size (number of months)
df <- data_in %>% group_by(local) %>% mutate(count = n(), na.rm = TRUE)
df <- unique(subset(df, select = c(local, count)))

# Per-facility number of zero NMR months
zr <- data_in %>% group_by(local) %>% filter(tmn == 0) %>% mutate(zeros = n(), na.rm = TRUE)
zr <- unique(subset(zr, select = c(local, zeros)))

df <- merge(x = df, y = zr, by = "local", all.x = TRUE)
df$zeros[is.na(df$zeros)] <- 0

df$perc <- df$zeros / df$count

# K-means clustering by zero-rate percentage (k = 3)
set.seed(123)
cat <- kmeans(df$perc, centers = 3)
df$cat <- cat$cluster
print(table(df$cat))

# Relabel clusters so 1 has highest zero-rate and 3 the lowest
cnes_cat <- df
cnes_cat$cat <- gsub("2", "4", cnes_cat$cat)
cnes_cat$cat <- gsub("3", "2", cnes_cat$cat)
cnes_cat$cat <- gsub("4", "3", cnes_cat$cat)
print(table(cnes_cat$cat))

# Merge back cluster to original dataset for per-observation labeling
df_full <- merge(x = data_in, y = cnes_cat, by = "local", all.x = TRUE)

# Plot one example time series from each cluster (IDs are dataset-specific)
c3 <- subset(data_in, local == "2290227", select = c(time, tmn))
names(c3) <- c("time", "HOSPITAL ESTADUAL ADAO PEREIRA NUNES (2290227)")
c2 <- subset(data_in, local == "2268922", select = c(time, tmn))
names(c2) <- c("time", "HOSPITAL MUNICIPAL DESEMBARGADOR LEAL JUNIOR (2268922)")
c1 <- subset(data_in, local == "5042488", select = c(time, tmn))
names(c1) <- c("time", "MATERNIDADE MUNICIPAL DRA ALZIRA REIS VIEIRA FERREIRA (5042488)")

df_plot <- merge(merge(c1, c2, by = "time", all = TRUE), c3, by = "time", all = TRUE)
df_melt <- melt(df_plot, id = c("time"))

p <- ggplot(df_melt, aes(x = time, y = value, group = variable)) +
  geom_line() +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "#8C9EFF"),
        axis.text = element_text(size = 12)) +
  xlab("") + ylab("NMR")
print(p)

# Visualize all time series for one cluster (cat == 1), marking anomalies (if provided)
ids_c1 <- unique(subset(df_full, cat == 1)$local)
for (i in ids_c1) {
  sb <- subset(data_in, local == i)
  a <- ggplot(sb, aes(x = time, y = tmn)) +
    geom_line() +
    geom_vline(xintercept = sb$time[sb$anom.p == TRUE], linetype = "dashed",
               color = "blue", size = .7) +
    geom_vline(xintercept = sb$time[sb$anom.t == TRUE], linetype = "dashed",
               color = "red", size = .7) +
    geom_vline(xintercept = sb$time[sb$anom.c == TRUE], linetype = "dashed",
               color = "green", size = .7) +
    ggtitle(sb$local)
  print(a)
}

