#' @title The binary data of a data set
#'
#' @description Returns the binary data of a data set
#' @param data a data.frame thats represents the sample
#' @export
#' @keywords
#' @seealso
#' @return the mse from two samples.
#' @aliases
#' @examples

create_binary <- function(data_target) {
  current_mean <- mean(as.matrix(data_target))
  return(ifelse(data_target > current_mean, 1, 0))
}
