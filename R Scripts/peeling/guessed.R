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

