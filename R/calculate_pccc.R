#' @title PCCC between the sample data and the predicted
#'
#' @description Returns the PCCC between the sample data and the predicted
#' @param data a data.frame thats represents the sample
#' @param predicted a data.frame thats represents the predicted data
#' @export
#' @keywords
#' @seealso
#' @return the mse from two samples.
#' @aliases
#' @examples

calculate_pccc <- function(predicted, data_target) {
  return(1 - mean(data_target != predicted))
}
