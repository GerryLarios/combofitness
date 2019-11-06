get_best_comb <- function(result, concept = "mse") {
  ordered <- result[order(result[concept]), ]
  return(as.list(ordered[1,]))
}
