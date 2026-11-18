###############################################################################
# MGF — Plotting Helpers
#
# Purpose
#   ggplot2-based helpers for time series, boxplots, bars, stacked bars,
#   and degree distributions used in MGF experiments.
#
# Requirements
#   ggplot2, scales, gridExtra; MGF.R (for datasets with expected columns)
###############################################################################

source("MGF.R")

loadlibrary("ggplot2")
loadlibrary("scales")
loadlibrary("gridExtra")

plot.series <- function(series, label_series=" ", label_x="x", label_y="y", colors=NULL) {
  grf <- ggplot(data=series, aes(x = x, y = value, colour=variable))
  grf <- grf + geom_line() + geom_point(data=series, aes(x = x, y = value, colour=variable), size=0.5)
  if (!is.null(colors)) {
    grf <- grf + scale_color_manual(values=colors)
  }
  grf <- grf + labs(color=label_series)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y) 
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) 
  grf <- grf + theme(legend.position = "bottom") + theme(legend.key = element_blank()) 
  return(grf)
}

plot.series.facet <- function(series, labels=NULL, labx="x", laby="y") {
  tx <- series
  tx <- tx[with(tx, order(variable, x)), ]
  mycolors = rep("darkblue", 10)
  
  myplot <- ggplot(tx, aes(x=x, y=value, color = variable)) +
    geom_point() +
    geom_smooth(method=lm) +
    theme_bw(base_size = 10) +  
    theme(panel.grid.minor = element_blank()) +
    scale_color_manual(values=mycolors) +
    xlab(labx) + 
    ylab(laby)
  if (!is.null(labels)) {
    myplot <- myplot + 
      scale_x_continuous(labels=labels) + 
      scale_y_continuous(labels=labels)
  }
  myplot <- myplot + 
    theme(legend.position = "none") +
    theme(strip.background = element_rect(fill="orange")) +
    facet_wrap(~variable, ncol = 5) 
  return (myplot)
}

plot.boxplot <- function(series, labx = "x", laby = "y", colors = NULL) {
  grf <- ggplot(aes(y = value, x = variable), data = series)
  if (!is.null(colors)) {
    grf <- grf + geom_boxplot(color = colors)
  }
  else {
    grf <- grf + geom_boxplot()
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + xlab(labx)
  grf <- grf + ylab(laby)
  return(grf)
}

plot.bar <- function(series, group=FALSE, colors=NULL) {
  if (group) {
    grf <- ggplot(series, aes(x, value, fill=variable)) + geom_bar(stat = "identity",position = "dodge")
    if (!is.null(colors)) {
      grf <- grf + scale_fill_manual("legend", values = colors)
    }
  }
  else {  
    grf <- ggplot(series, aes(variable, value))
    if (!is.null(colors)) {
      grf <- grf + geom_bar(stat = "identity",fill=colors)
    }
    else {  
      grf <- grf + geom_bar(stat = "identity")
    }    
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + scale_x_discrete(limits = unique(series$x))
  return(grf)
}

plot.stackedbar <- function(series, colors=NULL) {
  grf <- ggplot(series, aes(x=x, y=value, fill=variable)) + geom_bar(stat="identity", colour="white")
  if (!is.null(colors)) {
    grf <- grf + scale_fill_manual("legend", values = colors)
  }
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom")
  grf <- grf + scale_x_discrete(limits = unique(series$x))
  return(grf)
}


plot.distribution <- function(mpvalue, labx="x", laby="y") {
  t <- mpvalue
  tb <- t(table(t))
  tx <- NULL
  for(i in 1:ncol(tb)){
    z <- as.matrix(nrow=nrow(tb),ncol=3,cbind(as.double(rownames(tb)),as.double(tb[,i]),rep(as.double(colnames(tb)[i]),nrow(tb))))
    row.names(z) <- NULL
    z <- data.frame(z)
    colnames(z) <- c("degree", "qtd", "growth")
    z <- z[z$qtd > 0,]
    z <- within(z, sumqtd <- cumsum(qtd))
    z$qtd <- (z$qtd + sum(z$qtd) - z$sumqtd)/sum(z$qtd)
    z$sumqtd <- NULL
    tx <- rbind(tx, z)
  }
  tx <- data.frame(tx)
  mycolors = rep("darkblue", length(unique(tx$growth)))
  tx$growth = as.factor(tx$growth*100)
  
  grf <- ggplot(tx, aes(x=degree, y=qtd, color = growth)) +
    geom_point() +
    theme_bw(base_size = 10) + 
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    theme(strip.background = element_rect(fill="orange")) +
    scale_color_manual(values=mycolors) +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    xlab(labx) + 
    ylab(laby) +     
    annotation_logticks() +
    facet_wrap(~growth, ncol = 5) 
  
  return(grf)

}
