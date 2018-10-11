simBoundary <- function(nodes, para, start, end, n) {
  recall = list()
  precision = list()
  currG = sample_pa(nodes, power = 1, m = para,
                    directed = FALSE)
  for (t in logspace(start, end, n)) {
    bins = genAugPeel(currG, t)
    measures = empirExp(bins, nodes)
    recall = c(recall, measures[1])
    precision = c(precision, measures[2])
   # print(c(measures, t))
  }
  recall = recall[!is.na(recall)]
  precision = precision[!is.na(precision)]
  plot(recall, precision, ann = FALSE)
  title(main = "Efficiency Boundary")
  title(xlab = "Recall")
  title(ylab = "Precision")
}