# simulates the augmented peeling algorithm
# input: the maximum nodes, the m parameter

simAugPeel <- function(nodeMax, para) {
  recall = list()
  precision = list()
  origRecall = list()
  origPrecision = list()
  for (i in seq(1,nodeMax,100)) {
    currG = sample_pa(i, power = 1, m = para,
                      directed = FALSE)
    bins = augPeel(currG)
    origBins = peeling(currG)
    
    measures = empirExp(bins, i)
    origMeasures = empirExp(origBins, i)
    
    origRecall = c(origRecall, origMeasures[1])
    origPrecision = c(origPrecision, origMeasures[2])
    recall = c(recall, measures[1])
    precision = c(precision, measures[2])
  } 
  
  AugRecall = recall[!is.na(recall)]
  AugPrecision = precision[!is.na(precision)]
  OrigRecall = origRecall[!is.na(origRecall)]
  OrigPrecision = origPrecision[!is.na(origPrecision)]
  
  
  plot(seq(1,nodeMax,100)[which(!is.na(recall))], AugRecall, 
       type = "o", col = "blue", ann = FALSE, ylim = range(OrigRecall, OrigPrecision, AugRecall, AugPrecision))
  lines(seq(1,nodeMax,100)[which(!is.na(precision))], AugPrecision, type = "o", pch = 22,
        lty = 2, col = "red")
  lines(seq(1,nodeMax,100)[which(!is.na(origRecall))], OrigRecall, type = "o", pch = 22,
        lty = 2, col = "green")
  lines(seq(1,nodeMax,100)[which(!is.na(origPrecision))], OrigPrecision, type = "o", pch = 22,
        lty = 2, col = "pink")
  title(main = "Arrival order recovery")
  title(xlab = "Nodes")
  title(ylab = "Percent")
  legend("bottomleft", c("AugRecall","AugPrecision", "OrigRecall", "OrigPrecision"), cex=0.8, 
         col=c("blue","red", "green", 'pink'), pch=21:22, lty=1:2);
}

