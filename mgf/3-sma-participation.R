###############################################################################
# MGF Experiment 3 — Small Course Participation Sweep
#
# Purpose
#   Evaluate how increasing social edges affects metrics in a small course
#   setting; plots betweenness/closeness results across growth steps.
#
# Requirements
#   igraph, ggplot2, gridExtra; sources MGF core/datasets/graphics
###############################################################################

source("MGF.R")
source("MGF-Datasets.R")
source("MGF-Graphics.R")

trials <- list()
trials[[1]] <- generate_scenario(groups=3, vertexes=30, medges=60, sedges=25)
trials[[2]] <- generate_scenario(groups=3, vertexes=30, medges=60, sedges=45)
trials[[3]] <- generate_scenario(groups=3, vertexes=30, medges=60, sedges=55)

for (i in 1:length(trials)) {
  trials[[i]] <- compare_networks(trials[[i]])
}

colors <- c("darkblue", "darkred", "darkgreen", "orange")
bac <- betweenness.correlation.analysis(trials)
grf <- plot.series(bac, label_series="Config (k:v:m:s):", label_x="growth", label_y="betweenness - spearman.test", colors=colors)
grf <- grf + scale_x_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format())
plot1 <- grf + scale_y_continuous(breaks = seq(0, 1, 0.1))

ca <- closeness.analysis(trials)
grf <- plot.series(ca, label_series="Config (k:v:m:s):", label_x="growth", label_y="closeness - wilcox.test", colors=colors)
grf <- grf + scale_x_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format())
plot2 <- grf + scale_y_continuous(breaks = seq(0, 1, 0.1))

cac <- closeness.correlation.analysis(trials)
grf <- plot.series(cac, label_series="Config (k:v:m:s):", label_x="growth", label_y="closeness - spearman.test", colors=colors)
grf <- grf + scale_x_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format())
plot3 <- grf + scale_y_continuous(breaks = seq(0, 1, 0.1))

grid.arrange(plot1, plot2, plot3, nrow=3)

