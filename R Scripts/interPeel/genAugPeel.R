# take the graph, output bins based on peeling as well as the mean of the ages of the nodes they were connected to

genAugPeel <- function(G, p) {
  
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
    while(length(neighborAge) > 0) {
      ## find the max age
      minAge = min(neighborAge)
      if (length(neighborAge) == 1 && is.na(neighborAge[1])) {
        break
      }
      
      if (minAge > (p * mean(neighborAge))) {
        bins[[t]] = currpeel
        t = t + 1
        break
      }
      idx = which(neighborAge == minAge)
      bins[[t]] = currpeel[idx]
      t = t + 1
      currpeel = currpeel[-idx]
      neighborAge = neighborAge[-idx]
    }
  }
  bins
}
