empirExp <- function(bins, n) {
  guesses = guessed(bins) 
  incorrect = inversions(bins)
  total = choose(n, 2)
  c((total - incorrect)/guesses,
    (guesses - incorrect)/guesses)
  
}
