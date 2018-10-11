simDualBoundary <- function(nodes, para, start, end, n, samp) {
  recall = list()
  precision = list()
  graphs = list()
  
  for (i in 1:samp) {
    graphs[[i]] = sample_pa(nodes, power = 1, m = para, directed = FALSE)
  }
  
  for (t in c(1,seq(start, end, length.out = n))) {
    recallSum = 0
    precisionSum = 0
    div = samp
    for (i in 1:samp) {
      bins = dualGenPeel(graphs[[i]], t)
      measures = empirExp(bins, nodes)
      if (is.na(measures[1]) || is.na(measures[2])) {
        div = div - 1
        next
      }
      recallSum = recallSum + measures[1]
      precisionSum = precisionSum + measures[2]
    }
    if (is.na(recallSum) || is.na(precisionSum)) {
      next
    }
    recall = c(recall, recallSum/div)
    precision = c(precision, precisionSum/div)
  }
  plot(recall, precision, ann = FALSE)
  
  
  title(main = "Efficiency Boundary")
  title(xlab = "Recall")
  title(ylab = "Precision")
}
