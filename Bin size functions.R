n = 1000
para = 5
G = sample_pa(n, power = 1, m = para, directed = FALSE)
bins = peeling(G)
binsizes = as.numeric(sapply(bins, length))
binnumbers = as.numeric(seq(1, length(binsizes), 1))
diff.model = lm(log(binsizes) ~ binnumbers)
timevalues = seq(0, length(binsizes), .1)
predictions = exp(predict(diff.model, data.frame(binnumbers = timevalues)))
semilogy(binnumbers, binsizes, pch = 16)
plot(binnumbers, log(binsizes))

