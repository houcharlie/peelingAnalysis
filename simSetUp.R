# take bins, take number of guesses

guessed <- function(bins) {
  guesses = 0
  for (i in 1:length(bins)) {
    if (i >= length(bins))
      break
    for (j in (i+1):length(bins)) {
      guesses = guesses + length(bins[[i]])*length(bins[[j]])
    }
  }
  guesses
}

# find the number of inversions

inversions <- function(bins) {
  inversions = 0
  for(i in 1:length(bins)) {
    
    if (i >= length(bins))
      break
    for(j in (i+1):length(bins)) {
      inversions = inversions + invert(bins[[i]], bins[[j]])
    }
  }
  inversions
}

# helper function

invert <- function(a, b) {
  inversions = 0
  for (i in 1:length(a)) {
    inversions = inversions + length(which(b < a[i]))
  }
  inversions
}

# input: bins and numbers of nodes
# output: [total recall, precision]

empirExp <- function(bins, n) {
  guesses = guessed(bins) 
  incorrect = inversions(bins)
  total = choose(n, 2)
  c((guesses - incorrect)/total,
    (guesses - incorrect)/guesses)
  
}

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

# take graph, peel it into bins
peeling <- function(G) {
  # no side effect
  g = G
  
  ## retain vertex ids
  V(g)$name = seq(1, gorder(g), 1)
  
  ## the partial order bins
  bins = list()
  t = 1
  while (gorder(g) > 0) {
    MinDeg = min(degree(g))
    bins[[t]] = which(degree(g) == MinDeg)
    g = delete_vertices(g, bins[[t]])
    bins[[t]] = as.numeric(names(bins[[t]]))
    t = t + 1
  }
  bins = rev(bins)
  bins
}