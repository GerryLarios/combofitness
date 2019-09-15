get_best_comb <- function(result) {
  ordered <- result[order(result$mse), ]
  return(as.list(ordered[1,]))
}
