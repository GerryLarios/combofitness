#' @title Classify the output of a linear network in binary.
#'
#' @description Returns the binary data of a data set
#' @param data a data.frame thats represents the sample
#' @export
#' @keywords
#' @seealso
#' @return the clasification
#' @aliases
#' @examples

classify_binary <- function(predicted) {
  yy_mean <- mean(predicted)
  yhat <- matrix(0, length(predicted), 1)
  yhat[which(predicted > yy_mean)] <- 1
  yhat[which(predicted <= yy_mean)] <- 0
  return(yhat)
}
