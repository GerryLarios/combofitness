  #' @title Returns the a list of combinations of hyperparameters
#'
#' @description Returns the a list of combinations given by arrays
#' @param learning_rate an array that represents the differents learning rates of the networks.
#' @param num_hidden an array that represents the differents numbers of hidden layers of the networks.
#' @param num_neurons an array that represents the differents numbers of neurons of each hidden layer of the networks
#' @param dropout_hidden an array that represents the percentage of dropout in the hidden layers of the network
#' @export
#' @keywords
#' @seealso
#' @return the list of combinations for combofitnets
#' @aliases
#' @examples

build_combinations <- function(
  learning_rate = c(0.01, 0.04, 0.06, 0.1, 0.14),
  num_hidden = c(1, 2, 3, 4, 5),
  num_neurons = c(100, 200, 400, 500),
  dropout_hidden = seq(0.95, 0.5, -0.05)
  ) {
  return(
    as.list(
      expand.grid(
        learning_rate = learning_rate,
        num_hidden = num_hidden,
        num_neurons = num_neurons,
        dropout_hidden = dropout_hidden
        )
      )
    )
}
