###############################################################################
# MGF — Dataset and Scenario Generation Helpers
#
# Purpose
#   Compose “email” and “social” networks and build scenario sweeps over a
#   growth parameter to study complementarity. Includes a simple binning helper.
#
# Requirements
#   MGF.R (core), igraph
###############################################################################

source("MGF.R")

# Creates a new Email Network, where i is the number of subnetworks
# vertexes is the number of vertexes of each subnetwork and
# edges is the number of edges of each subnetwork
create_social <- function(vertexes, edges) {
  social <- graph_create(vertexes, edges)
  social <- graph_add_weights(social)
  social <- graph_metrics(social)
}

create_email <- function(groups, vertexes, edges) {
  lst <- list()
  dv <- trunc(vertexes / groups)
  de <- trunc(edges / groups)
  i <- 1
  while(i < groups)
  {
    g <- graph_create(dv,de)
    vertexes <- vertexes - dv
    edges <- edges - de
    lst[[i]] <- g
    i <- i + 1
  }
  g <- graph_create(vertexes,edges)
  lst[[i]] <- g
  

  union <- NULL
  if (length(lst) > 1) {
    for (i in 1:length(lst) ) {
      graph <- lst[[i]]  
      graph <- graph_metrics(graph)
      lst[[i]] <- graph
      if (is.null(union)) 
        union <- graph
      else 
        union <- union + graph
    }
    sumi = 0
    for (i in 1:(length(lst)-1)) {
      graphi <- lst[[i]]
      sumj = sumi + length(V(graphi))
      for (j in (i+1):length(lst) ) {
        graphj <- lst[[j]]
        union[sumi + graphi$centrality_max[1] , sumj + graphj$centrality_max[1]] <- 1
        sumj = sumj + length(V(graphj))
      }
      sumi = sumi + length(V(graphi))
    }
    
    union$origin <- length(lst)    
    
  }
  else
  {
    union <- lst[[1]]
    union$origin <- 1
  }

  email <- union
  email <- graph_add_weights(email)
  email <- graph_metrics(email)
  email$origin <- i
  return (email)
}


generate_scenario <- function(groups, vertexes, medges, sedges, steps = seq(0.0, 1, 0.01)) {
  set.seed(0) 
  label <- paste(groups, ":", vertexes, ":", medges, ":", sedges, sep = "")
  graphA <- create_email(groups, vertexes, medges)
  graphB <- create_social(vertexes, sedges)
  
  p.closeness <- NULL
  p.betweenness <- NULL
  p.hub <- NULL
  networks <- list()
  k <- 0
  
  for (i in steps) {
    k <- k + 1
    
    graphAm <- graphA
    graphBm <- graph_multiply(graphB, i)
    graphBm <- graph_prune(graphBm, i)
    
    networks[[k]] <- list(Am = graphAm, Bm = graphBm, label = paste(i))
  }
  return (list(networks = networks,label=label))
} 

binning = function(v, n) {
  p = seq(from = 0, to = 1, by = 1 / n)
  q = quantile(v, p)
  qf = matrix(c(q[1:(length(q) - 1)], q[2:(length(q))]), ncol = 2)
  vp = cut(v, q, FALSE, include.lowest = TRUE)
  m = tapply(v, vp, mean)
  vm = m[vp]
  mse = mean((v - vm) ^ 2, na.rm = TRUE)
  return (list(
    binning = m,
    bins_factor = vp,
    q = q,
    qf = qf,
    bins = vm,
    mse = mse
  ))
}

