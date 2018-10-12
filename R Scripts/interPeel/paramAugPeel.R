paramAugPeel <- function(G, z) {
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
  bins = peel
  ## tiebreak the bins using where they were connected to
  newbins = list()
  t = 1
  for (i in 1:length(peel)) {
    currbin = bins[[i]]
    neighborAge = c()
    # find the mean age of the neighbors of each vertex
    for (j in 1:length(currbin)) {
      neighborMean = mean(unlist(sapply(neighbors(G, currbin[j]), aux)), na.rm = TRUE)
      neighborAge[j] = neighborMean
    }
    if (length(currbin) < 1) {
      next
    }
    # setup to find outliers
    mu = mean(neighborAge)
    stddev = std(neighborAge)
    
    if (is.na(stddev)) {
      newbins[[t]] = currbin
      t = t + 1
      next
    }
    if (stddev == 0) {
      newbins[[t]] = currbin
      t = t + 1
      next
    }
    # identify the high outliers
    idxplus = which((neighborAge - mu)/stddev > z)
    # identify the low outliers
    idxminus = which((neighborAge - mu)/stddev < -z)
    # the remaining indices
    idxremain = setdiff(seq_along(currbin), append(idxplus,idxminus))
    if (length(idxminus > 0)) {
      newbins[[t]] = currbin[idxminus]
      t = t + 1
    }
    if (length(idxremain > 0)) {
      newbins[[t]] = currbin[idxremain]
      t = t + 1
    }
    if (length(idxplus > 0)) {
      newbins[[t]] = currbin[idxplus]
      t = t + 1
    }
    
  }
  newbins
}
