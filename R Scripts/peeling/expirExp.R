# input: bins and numbers of nodes
# output: [total recall, precision]

empirExp <- function(bins, n) {
  guesses = guessed(bins) 
  incorrect = inversions(bins)
  total = choose(n, 2)
  c((guesses - incorrect)/total,
    (guesses - incorrect)/guesses)
  
}
