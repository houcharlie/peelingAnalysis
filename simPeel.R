# simulate the peeling algorithm
# inputs: max of nodes, the m parameter

simPeel <- function(nodeMax, para) {
  recall = list()
  precision = list()
  for (i in seq(1,nodeMax,100)) {
    currG = sample_pa(i, power = 1, m = para,
                      directed = FALSE)
    bins = peeling(currG)
    measures = empirExp(bins, i)
    recall = c(recall, measures[1])
    precision = c(precision, measures[2])
  } 
  plot(seq(1,nodeMax,100)[which(!is.na(recall))], recall[!is.na(recall)], 
       xlab = 'Number of nodes', ylab = 'Recall')
  plot(seq(1,nodeMax,100)[which(!is.na(precision))], precision[!is.na(precision)],
       xlab = 'Number of nodes', ylab = 'Precision')
}

