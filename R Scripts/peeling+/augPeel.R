# take the graph, output bins based on peeling as well as the mean of the ages of the nodes they were connected to

augPeel <- function(G) {
  
  # no side effect
  g = G
  
  ## retain vertex ids
  V(g)$name = seq(1, gorder(g), 1)
  
  ## get the results from the peel
  peel = peeling(G)
  
  vertexToAge = list()
  ## get the guessed ages from the peel 
  ## (lower the number, the older)
  for (i in 1:length(peel)) {
    for (j in 1:length(peel[[i]])) {
      vertexToAge[peel[[i]][j]] = i
    }
  }
  
  aux <- function(i) {
    vertexToAge[i]
  }
  
  ## tiebreak the bins using where they were connected to
  bins = list()
  t = 1
  for (i in 1:length(peel)) {
    currpeel = peel[[i]]
    neighborAge = c()
    for (j in 1:length(currpeel)) {
      neighborMean = mean(unlist(sapply(neighbors(G, currpeel[j]), aux)), na.rm = TRUE)
      neighborAge[j] = neighborMean
    }
    while(length(currpeel) > 0) {
      ## find the max age
      maxAge = min(neighborAge)
      idx = which(neighborAge == maxAge)
      bins[[t]] = currpeel[idx]
      t = t + 1
      currpeel = currpeel[-idx]
      neighborAge = neighborAge[-idx]
    }
  }
  bins
}
