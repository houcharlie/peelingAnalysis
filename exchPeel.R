exchPeel <- function(G, z) {
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
  
  ## move outliers around
  bins = peel
  
  for (i in 1:length(peel)) {
    if (length(bins[[i]]) < 1)
      next
    
    
    currbin = bins[[i]]
    neighborAge = c()
    # find the mean age of the neighbors of each vertex
    for (j in 1:length(currbin)) {
      neighborMean = mean(unlist(sapply(neighbors(G, currbin[j]), aux)), na.rm = TRUE)
      neighborAge[j] = neighborMean
    }
    
    # setup to find outliers
    mu = mean(neighborAge)
    stddev = std(neighborAge)
    
    if (is.na(stddev))
      next
    if (stddev == 0)
      next
    
    if (i < length(peel)) {
      # identify the high outliers
      idx = which((neighborAge - mu)/stddev > z)

      # take from current bin the high outliers, and put them in the bin above
      nextbin = bins[[i + 1]]
      nextbin = append(nextbin, currbin[idx])
      currbin = currbin[setdiff(seq_along(currbin), idx)]

      # remove the outlier indices from the neighborAge vector to match with currbin
      neighborAge = neighborAge[setdiff(seq_along(neighborAge), idx)]
      
      # put into the bins
      bins[[i + 1]] = nextbin
      bins[[i]] = currbin
    }
    if (i > 1) {
      # identify low outliers
      idx = which((neighborAge - mu)/stddev < -z)
      
      prevbin = bins[[i - 1]]
      prevbin = append(prevbin, currbin[idx])
      currbin = currbin[setdiff(seq_along(currbin), idx)]
      
      neighborAge = neighborAge[setdiff(seq_along(neighborAge), idx)]
      
      bins[[i - 1]] = prevbin
      bins[[i]] = currbin
    }
  }
  
  bins
}
