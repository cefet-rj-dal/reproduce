###############################################################################
# Mixed Graph Framework (MGF) — Core Functions
#
# Purpose
#   Helpers to generate, weight, transform, and analyze graphs representing
#   complementary communication networks (e.g., email vs social). Provides
#   metrics, hub/authority transforms, pruning/merging, and analysis helpers.
#
# Requirements
#   igraph, poweRlaw
###############################################################################

loadlibrary <- function(x) {
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("igraph")
loadlibrary("poweRlaw")

graph_create <- function(vertexes, edges) {
  # Create a directed graph with power-law degree distribution
  graph <- static.power.law.game(vertexes, edges, Inf)
  return(graph)
}

graph_add_weights <- function(graph) {
  graph <- graph_metrics(graph)
  E(graph)$weight <- rep(1, length(E(graph)))
  adj <- get.adjacency(graph, attr="weight", sparse = FALSE)
  for (i in 1:length(graph$degree) ) {
    p = runif(ncol(adj), min=1, max=1000)
    if (graph$degree[i] > 0)
      adj[i,] <- p*adj[i,]/graph$degree[i]
  }
  graph <- graph.adjacency(adj, mode = "directed", weighted = TRUE)
  # recompute metrics
  graph <- graph_metrics(graph)
  return(graph)
}

graph_metrics <- function(graph) {
  graph$centrality <- centralization.degree(graph)$res
  tmp <- data.frame(vertex = c(1:length(graph$centrality)), centrality = graph$centrality)
  o = order(tmp$centrality, decreasing = TRUE)
  tmp <- tmp[o, ]
  graph$centrality_max <- tmp$vertex
  graph$c_eigen <- eigen_centrality(graph)$vector
  graph$diameter <- diameter(graph)
  graph$betweenness <- betweenness(graph)
  graph$closeness <- closeness(graph)
  graph$degree <- degree(graph)
  graph$strength <- graph.strength(graph)
  if (is.null(E(graph)$weight))
    graph$matrix <- get.adjacency(graph, sparse = FALSE)
  else
    graph$matrix <- get.adjacency(graph, attr = "weight", sparse = FALSE)
  return (graph)
}

graph_hub <- function(graph) {
  x <- graph$matrix 
  y <- t(graph$matrix)
  matrix <- x %*% y
  matrix[ row(matrix) == col(matrix) ] <- 0  
  rep <- graph.adjacency(matrix, mode = "directed", weighted = TRUE)
  rep <- graph_metrics(rep)
  return (rep)
}

graph_authority <- function(graph) {
  x <- graph$matrix 
  y <- t(graph$matrix)
  matrix <- y %*% x
  matrix[ row(matrix) == col(matrix) ] <- 0  
  rep <- graph.adjacency(matrix, mode = "directed", weighted = TRUE)
  rep <- graph_metrics(rep)
  return (rep)
}

graph_multiply <- function(graph, factor) {
  matrix <- factor*graph$matrix 
  rep <- graph.adjacency(matrix, mode = "directed", weighted = TRUE)
  rep <- graph_metrics(rep)
  return (rep)
}

graph_reciprical <- function(graph) {
  matrix <- graph$matrix 
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if ((!is.na(matrix[i,j])) && (matrix[i,j] != 0))
        matrix[i,j] = 1.0/matrix[i,j]
      else
        matrix[i,j] = 0
    }
  }
  rep <- graph.adjacency(matrix, mode="directed", weighted=TRUE)
  rep <- graph_metrics(rep)
  return (rep)
}

graph_prune <- function(graphB, i) {
  mB <- graphB$matrix
  v <- 0
  vector <- c(mB[mB>0])
  if (length(vector) > 0) {
    v <- quantile(vector, i)
  }
  mB[mB > v] <- 0
  graphB <- graph.adjacency(mB, mode = "directed", weighted = TRUE)
  graphB <- graph_metrics(graphB)
  return (graphB)
}

graph_merge <- function(graphA, graphB) {
  mgeral <- graphA$matrix + graphB$matrix
  geral <- graph.adjacency(mgeral, mode = "directed", weighted = TRUE)
  geral <- graph_metrics(geral)
  return (geral)
}

analyze_metrics <- function(graphA, graphB, graphAB, label)
{
  # Use reciprocal weights so that larger weights imply tighter distances
  graphA <- graph_reciprical(graphA)
  graphB <- graph_reciprical(graphB)
  graphAB <- graph_reciprical(graphAB)
  
  vA <- graphA$closeness
  vB <- graphB$closeness
  vAB <- graphAB$closeness
  
  closeness <- data.frame(vA, vB, vAB)
  
  vA <- graphA$betweenness
  vB <- graphB$betweenness
  vAB <- graphAB$betweenness
  
  betweenness <- data.frame(vA, vB, vAB)
  
  ghubA <- graph_hub(graphA)
  ghubAB <- graph_hub(graphAB)
  
  vA <- ghubA$c_eigen
  vAB <- ghubAB$c_eigen
  
  hub <- data.frame(vA, vAB)
  
  return(list(closeness=closeness, betweenness=betweenness, hub=hub))
}

compare_networks <- function(dataset) {
  networks = dataset$networks
  for(k in (1:length(networks)))
  {
    graphAm <- networks[[k]]$Am
    graphBm <- networks[[k]]$Bm
    label <- networks[[k]]$label
    graphABm <- graph_merge(graphAm, graphBm)
    lmetrics <- analyze_metrics(graphAm, graphBm, graphABm, label)
    networks[[k]] <- list(Am = graphAm, Bm = graphBm, ABm = graphABm, closeness = lmetrics$closeness, betweenness = lmetrics$betweenness, hub = lmetrics$hub, label=label)
  }
  label = dataset$label
  return (list(networks = networks,label=label))
} 

betweenness.analysis = function(trials, pvalue=0.05) {
  m = length(trials)
  if (m < 1)
    return(NULL)
  threshold = rep(0,m)
  n = length(trials[[1]]$networks)  
  ratio = 1/(n-1)
  
  vlabel = c(1:(m+1))
  merge = NULL
  for (j in 1:m) {
    mpvalue = matrix(nrow=n, ncol=3)
    es = trials[[j]]
    vlabel[j] = es$label
    for(i in 1:n) {
      e = es$networks[[i]]
      betweenness = e$betweenness
      
      w_A_AB = wilcox.test(betweenness$vA, betweenness$vAB,  alternative="two.sided", exact=FALSE, conf.level=0.95)
      mpvalue[i, 1] = 0 + ratio*(i-1)
      mpvalue[i, 2] = w_A_AB$p.value
      mpvalue[i, 3] = j
    }
    merge = rbind(merge, mpvalue)
  }
  mpvalue = matrix(nrow=n, ncol=3)
  for(i in 1:n) {
    mpvalue[i, 1] = 0 + ratio*(i-1)
    mpvalue[i, 2] = pvalue
    mpvalue[i, 3] = m+1
  }
  merge = rbind(merge, mpvalue)
  
  vlabel[m+1] = paste(round(pvalue*100),"%",sep="")
  
  mpvalue = data.frame(x=as.double(merge[,1]), value=as.double(merge[,2]), variable=as.factor(merge[,3]))
  levels(mpvalue$variable) = vlabel
  return(mpvalue)
}

betweenness.correlation.analysis <- function(trials, pvalue=0.05) {
  m <- length(trials)
  if (m < 1)
    return(NULL)
  threshold <- rep(0,m)
  n <- length(trials[[1]]$networks)
  ratio <- 1/(n-1)
  
  vlabel = c(1:(m+1))
  merge <- NULL
  for (j in 1:m) {
    mpvalue <- matrix(nrow=n, ncol=3)
    es <- trials[[j]]
    vlabel[j] = es$label
    for(i in 1:n) {
      e <- es$networks[[i]]
      betweenness <- e$betweenness
      w_A_AB <- cor.test(betweenness$vA, betweenness$vAB, method="spearman", alternative="two.sided", exact=FALSE, conf.level=0.95)
      mpvalue[i, 1] = 0 + ratio*(i-1)
      mpvalue[i, 2] = w_A_AB$p.value
      if (is.na(mpvalue[i, 2])) {
        if (i > 1)
          mpvalue[i, 2] = mpvalue[i-1, 2]
        else
          mpvalue[i, 2] = 0
      }
      mpvalue[i, 3] = j
    }
    merge = rbind(merge, mpvalue)
  }
  
  
  mpvalue <- matrix(nrow=n, ncol=3)
  for(i in 1:n) {
    mpvalue[i, 1] = 0 + ratio*(i-1)
    mpvalue[i, 2] = pvalue
    mpvalue[i, 3] = m+1
  }
  merge = rbind(merge, mpvalue)
  
  vlabel[m+1] = paste(ceiling(pvalue*100),"%", sep="")
  
  mpvalue = data.frame(x=as.double(merge[,1]), value=as.double(merge[,2]), variable=as.factor(merge[,3]))
  levels(mpvalue$variable) = vlabel  
  return(mpvalue)
}

betweenness.boxplot <- function(trial, step) {
  es <- trial
  n <- length(es$networks)  
  e <- es$networks[[1]]
  v <- length(e$betweenness$vA)
  ratio <- 1/(n-1)
  
  merge <- NULL
  mpvalue <- matrix(nrow=v, ncol=2)
  i <- 1
  qtd <- 1
  while(i <= n) {
    e <- es$networks[[i]]
    mpvalue[, 1] = rep(e$label, v)
    mpvalue[, 2] = e$betweenness$vAB
    
    i <- i + step
    qtd <- qtd + 1
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- data.frame(variable=as.factor(merge[,1]),value=as.double(merge[,2]))
  return(mpvalue)
}


betweenness.correlation.exploratory <- function(trial, step) {
  es <- trial
  n <- length(es$networks)  
  e <- es$networks[[1]]
  v <- length(e$ABm$betweenness)
  ratio <- 1/(n-1)
  
  merge <- NULL
  mpvalue <- matrix(nrow=v, ncol=3)
  i <- 1
  while(i <= n) {
    e <- es$networks[[i]]
    mpvalue[, 1] = e$label
    mpvalue[, 2] = e$Am$betweenness
    mpvalue[, 3] = e$ABm$betweenness
    
    i <- i + step
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- data.frame(variable=as.factor(merge[,1]),x=as.double(merge[,2]), value=as.double(merge[,3]))
  return(mpvalue)
}


closeness.analysis <- function(trials, pvalue=0.05) {
  m <- length(trials)
  if (m < 1)
    return(NULL)
  threshold <- rep(0,m)
  n <- length(trials[[1]]$networks)  
  ratio <- 1/(n-1)
  
  vlabel = c(1:(m+1))
  merge <- NULL
  for (j in 1:m) {
    mpvalue <- matrix(nrow=n, ncol=3)
    es <- trials[[j]]
    vlabel[j] = es$label
    for(i in 1:n) {
      e <- es$networks[[i]]
      closeness <- e$closeness
      w_A_AB <- wilcox.test(closeness$vA, closeness$vAB,  alternative="two.sided", exact=FALSE, conf.level=0.95)
      mpvalue[i, 1] = 0 + ratio*(i-1)
      mpvalue[i, 2] = w_A_AB$p.value
      if (is.na(mpvalue[i, 2])) {
        if (i > 1)
          mpvalue[i, 2] = mpvalue[i-1, 2]
        else
          mpvalue[i, 2] = 0
      }
      mpvalue[i, 3] = j
    }
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- matrix(nrow=n, ncol=3)
  for(i in 1:n) {
    mpvalue[i, 1] = 0 + ratio*(i-1)
    mpvalue[i, 2] = pvalue
    mpvalue[i, 3] = m+1
  }
  merge = rbind(merge, mpvalue)
  
  vlabel[m+1] = paste(round(pvalue*100),"%",sep="")
  
  mpvalue = data.frame(x=as.double(merge[,1]), value=as.double(merge[,2]), variable=as.factor(merge[,3]))
  levels(mpvalue$variable) = vlabel
  return(mpvalue)
}

closeness.correlation.analysis <- function(trials, pvalue=0.05) {
  m <- length(trials)
  if (m < 1)
    return(NULL)
  threshold <- rep(0,m)
  n <- length(trials[[1]]$networks)  
  ratio <- 1/(n-1)
  
  vlabel = c(1:(m+1))
  merge <- NULL
  for (j in 1:m) {
    mpvalue <- matrix(nrow=n, ncol=3)
    es <- trials[[j]]
    vlabel[j] = es$label
    for(i in 1:n) {
      e <- es$networks[[i]]
      closeness <- e$closeness
      
      w_A_AB <- cor.test(closeness$vA, closeness$vAB, method="spearman", alternative="two.sided", exact=FALSE, conf.level=0.95)
      
      mpvalue[i, 1] = 0 + ratio*(i-1)
      mpvalue[i, 2] = w_A_AB$p.value
      if (is.na(mpvalue[i, 2])) {
        if (i > 1)
          mpvalue[i, 2] = mpvalue[i-1, 2]
        else
          mpvalue[i, 2] = 0
      }
      mpvalue[i, 3] = j
    }
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- matrix(nrow=n, ncol=3)
  for(i in 1:n) {
    mpvalue[i, 1] = 0 + ratio*(i-1)
    mpvalue[i, 2] = pvalue
    mpvalue[i, 3] = m+1
  }
  merge = rbind(merge, mpvalue)
  
  vlabel[m+1] = paste(ceiling(pvalue*100),"%", sep="")
  
  mpvalue = data.frame(x=as.double(merge[,1]), value=as.double(merge[,2]), variable=as.factor(merge[,3]))
  levels(mpvalue$variable) = vlabel  
  return(mpvalue)
}

closeness.boxplot <- function(trial, step) {
  es <- trial
  n <- length(es$networks)  
  e <- es$networks[[1]]
  v <- length(e$closeness$vA)
  ratio <- 1/(n-1)
  
  merge <- NULL
  mpvalue <- matrix(nrow=v, ncol=2)
  i <- 1
  qtd <- 1
  while(i <= n) {
    e <- es$networks[[i]]
    mpvalue[, 1] = rep(e$label, v)
    mpvalue[, 2] = e$closeness$vAB
    
    i <- i + step
    qtd <- qtd + 1
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- data.frame(variable=as.factor(merge[,1]),value=as.double(merge[,2]))
  return(mpvalue)
}

closeness.correlation.exploratory <- function(trial, step) {
  es <- trial
  n <- length(es$networks)  
  e <- es$networks[[1]]
  v <- length(e$ABm$closeness)
  ratio <- 1/(n-1)
  
  merge <- NULL
  mpvalue <- matrix(nrow=v, ncol=3)
  i <- 1
  while(i <= n) {
    e <- es$networks[[i]]
    mpvalue[, 1] = e$label
    mpvalue[, 2] = e$Am$closeness
    mpvalue[, 3] = e$ABm$closeness
    
    i <- i + step
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- data.frame(variable=as.factor(merge[,1]),x=as.double(merge[,2]), value=as.double(merge[,3]))
  return(mpvalue)
}


degree.boxplot <- function(trial, step) {
  es <- trial
  n <- length(es$networks)  
  e <- es$networks[[1]]
  v <- length(e$ABm$degree)
  ratio <- 1/(n-1)

  merge <- NULL
  mpvalue <- matrix(nrow=v, ncol=2)

  i <- 1
  qtd <- 1
  while(i <= n) {
    e <- es$networks[[i]]
    mpvalue[, 1] = rep(e$label, v)
    mpvalue[, 2] = e$ABm$degree
    qtd <- qtd + 1
    i <- i + step
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- data.frame(variable=as.factor(merge[,1]),value=as.double(merge[,2]))
  return(mpvalue)
}

degree.distribution <- function(trial, step) {
  es <- trial
  n <- length(es$networks)  
  e <- es$networks[[1]]
  v <- length(e$ABm$degree)
  ratio <- 1/(n-1)

  merge <- NULL
  mpvalue <- matrix(nrow=v, ncol=2)
  i <- 1
  while(i <= n) {
    e <- es$networks[[i]]
    mpvalue[, 1] = rep(e$label, v)
    mpvalue[, 2] = e$ABm$degree
    
    i <- i + step
    merge = rbind(merge, mpvalue)
  }
  mpvalue <- data.frame(x=as.double(merge[,1]),y=as.double(merge[,2]))
  return(mpvalue)
}






