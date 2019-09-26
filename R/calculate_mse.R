#' @title Returns the MSE between the sample data and the predicted
#'
#' @description Returns the Mean Square Error between the sample data and the predicted
#' @param data a data.frame thats represents the sample
#' @param predicted a data.frame thats represents the predicted data
#' @export
#' @keywords
#' @seealso
#' @return the mse from two samples.
#' @aliases
#' @examples

calculate_mse <- function(predicted, data_target) {
  return(mean((data_target[, ncol(data_target)] - predicted)^2))
}
