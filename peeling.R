# take graph, peel it into bins
peeling <- function(G) {
  # no side effect
  g = G
  
  ## retain vertex ids
  V(g)$name = seq(1, gorder(g), 1)
  
  ## the partial order bins
  bins = list()
  t = 1
  while (gorder(g) > 0) {
    MinDeg = min(degree(g))
    bins[[t]] = which(degree(g) == MinDeg)
    g = delete_vertices(g, bins[[t]])
    bins[[t]] = as.numeric(names(bins[[t]]))
    t = t + 1
  }
  bins = rev(bins)
  bins
}
