G = sample_pa(n = 4, power = 1, m = 2, directed = FALSE)
plot(G)
bins = peeling(G)
print(bins)
for (i in 1:5) {
  G = sample_pa(n = 4 + i, power = 1, m = 2, directed = FALSE, start.graph = G)
  plot(G)
  bins = peeling(G)
  print(bins)
}
