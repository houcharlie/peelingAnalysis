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
