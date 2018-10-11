simGenAugPeel <- function(nodeMax, para, t) {
  recall = list()
  precision = list()
  origRecall = list()
  origPrecision = list()
  genRecall = list()
  genPrecision = list()
  for (i in seq(1,nodeMax,100)) {
    currG = sample_pa(i, power = 1, m = para,
                      directed = FALSE)
    bins = augPeel(currG)
    origBins = peeling(currG)
    genBins = genAugPeel(currG, t)
    
    measures = empirExp(bins, i)
    origMeasures = empirExp(origBins, i)
    genMeasures = empirExp(genBins, i)
    origRecall = c(origRecall, origMeasures[1])
    origPrecision = c(origPrecision, origMeasures[2])
    
    genRecall = c(genRecall, genMeasures[1])
    genPrecision = c(genPrecision, genMeasures[2])
    
    recall = c(recall, measures[1])
    precision = c(precision, measures[2])
  } 

  AugRecall = recall[!is.na(recall)]
  AugPrecision = precision[!is.na(precision)]
  OrigRecall = origRecall[!is.na(origRecall)]
  OrigPrecision = origPrecision[!is.na(origPrecision)]
  GenRecall = genRecall[!is.na(genRecall)]
  GenPrecision = genPrecision[!is.na(genPrecision)]
  
  
  plot(seq(1,nodeMax,100)[which(!is.na(recall))], AugRecall, 
       type = "o", col = "blue", ann = FALSE, ylim = range(OrigRecall, OrigPrecision, AugRecall, AugPrecision))
  lines(seq(1,nodeMax,100)[which(!is.na(precision))], AugPrecision, type = "o", pch = 22,
        lty = 2, col = "blue")
  lines(seq(1,nodeMax,100)[which(!is.na(origRecall))], OrigRecall, type = "o", col = "red")
  lines(seq(1,nodeMax,100)[which(!is.na(origPrecision))], OrigPrecision, type = "o", pch = 22,
        lty = 2, col = "red")
  lines(seq(1,nodeMax,100)[which(!is.na(genRecall))], GenRecall, type = "o", col = "green")
  lines(seq(1,nodeMax,100)[which(!is.na(genPrecision))], GenPrecision, type = "o", pch = 22,
        lty = 2, col = "green")
  
  title(main = "Arrival order recovery")
  title(xlab = "Nodes")
  title(ylab = "Percent")
}
