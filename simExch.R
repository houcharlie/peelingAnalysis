simExch <- function(nodes, para, start, end, n) {
  recall = list()
  precision = list()
  currG = sample_pa(nodes, power = 1, m = para,
                    directed = FALSE)
  for (z in logspace(start, end, n)) {
    bins = exchPeel(currG, z)
    measures = empirExp(bins, nodes)
    recall = c(recall, measures[1])
    precision = c(precision, measures[2])
    # print(c(measures, t))
  }
  recall = recall[!is.na(recall)]
  precision = precision[!is.na(precision)]
  peelBins = peeling(currG)
  peelRes = empirExp(peelBins, nodes)
  plot(peelRes[1], peelRes[2], ann = FALSE, col = "red", pch = 19)
  lines(recall, precision, ann = FALSE)
  
 
  
  title(main = "Exchange Results")
  title(xlab = "Recall")
  title(ylab = "Precision")
}