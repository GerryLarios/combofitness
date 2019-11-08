#' @title Creates a new dataset whitout the target.
#'
#' @description Returns the binary data of a data set
#' @param data a data.frame thats represents the sample
#' @export
#' @keywords
#' @seealso
#' @return the clasification
#' @aliases
#' @examples

data_no_target <- function(main_data, target) {
  return(main_data[which(names(main_data) != target)])
}

