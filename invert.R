# helper function

invert <- function(a, b) {
  inversions = 0
  for (i in 1:length(a)) {
    inversions = inversions + length(which(b < a[i]))
  }
  inversions
}
