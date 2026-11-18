###############################################################################
# MGF Experiment 1 — Toy Example
#
# Purpose
#   Small illustrative run that builds two networks and visualizes structures
#   and distributions using the MGF helpers.
#
# Requirements
#   igraph, ggplot2, gridExtra; sources MGF core/datasets/graphics
###############################################################################

source("MGF.R")
source("MGF-Graphics.R")
source("MGF-Datasets.R")

groups <- 2
vertexes <- 10
medges <- 10
sedges <- 10

trial <- generate_scenario(groups, vertexes, medges, sedges, seq(0.0, 1, 0.25))
trial <- compare_networks(trial)

net <- trial$networks[[1]]
weight <- binning(ceiling(E(net$Am)$weight), 5)$bins_factor
par(mar = c(0, 0, 0, 0), mfrow = c(3,3))
plot(net$Am, edge.width = weight, edge.color = "darkblue", vertex.color = "lightblue")

net <- trial$networks[[2]]
plot(net$Bm, edge.width = weight, edge.color = "darkgreen", vertex.color = "lightgreen")

plot(net$ABm, edge.width = weight, edge.color = "darkred", vertex.color = "pink")

for (i in 3:length(trial$networks)) {
  net <- trial$networks[[i]]
  weight <- binning(ceiling(E(net$Bm)$weight), 5)$bins_factor
  plot(net$Bm, edge.width = weight, edge.color = "darkgreen", vertex.color = "lightgreen")
}

for (i in 3:length(trial$networks)) {
  net <- trial$networks[[i]]
  weight <- binning(ceiling(E(net$ABm)$weight), 5)$bins_factor
  plot(net$ABm, edge.width = weight, edge.color = "darkred", vertex.color = "pink")
}

colors <- c("darkblue", "orange")
dad <- degree.distribution(trial, 1)
grf <- plot.distribution(dad, labx="degree", laby="frequence")
plot(grf)

dab <- degree.boxplot(trial, 1)
plot1 <- plot.boxplot(dab, labx="growth", laby="degree")

bab <-  betweenness.boxplot(trial, 1)
plot2 <- plot.boxplot(bab, labx="growth", laby="betweenness")

cab <-  closeness.boxplot(trial, 1)
plot3 <- plot.boxplot(cab, labx="growth", laby="closeness")

grid.arrange(plot1, plot2, plot3, ncol=3)

bace <-  betweenness.correlation.exploratory(trial, 1)
grf <- plot.series.facet(bace, labx = "Gm", laby = "Gc")
plot(grf)

cace <-  closeness.correlation.exploratory(trial, 1)
grf <- plot.series.facet(cace, labx = "Gm", laby = "Gc")
plot(grf)

