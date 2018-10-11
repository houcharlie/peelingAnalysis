# take the graph, output bins based on peeling as well as the mean of the ages of the nodes they were connected to

dualGenPeel <- function(G, p) {
  
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
    maxBins = list()
    
    # tiebreak from top of bin and bottom of bin
    while(length(neighborAge) > 0) {
      ## find the max age, min age
      minAge = min(neighborAge)
      maxAge = max(neighborAge)
      if (length(neighborAge) == 1 && is.na(neighborAge[1])) {
        break
      }
      
      # when the length is 1, this will trigger
      if (minAge > ((1 - p) * mean(neighborAge)) || maxAge < ((1 + p)*mean(neighborAge))) {
        bins[[t]] = currpeel
        t = t + 1
        break
      }
      if (minAge == maxAge) {
        bins[[t]] = currpeel
        t = t + 1
        break
      }
      idxminus = which(neighborAge == minAge)
      idxplus = which(neighborAge == maxAge)
      bins[[t]] = currpeel[idxminus]
      t = t + 1
      
      # set up the ones that are being tiebroke at the top
      maxBins = append(currpeel[idxplus], maxBins)
      
      # update 
      currpeel = currpeel[setdiff(seq_along(currpeel), append(idxplus, idxminus))]
      neighborAge = neighborAge[setdiff(seq_along(neighborAge), append(idxplus, idxminus))]
      
    }
    if (length(maxBins) > 0) {
      bins = append(bins, maxBins)
      t = t + length(maxBins)
    }
  }
  bins
}
