#' @title Combinatios of hyperparameters to train neural networks
#'
#' @description Create a report and returns the best combination of hyperparameters to train a neural network.
#' @param data_main a data.frame that represents the sample
#' @param target a data.frame that represents the target of the sample
#' @param data_trn a data.frame that represents the train sample, if is not given is calculated from the data by 80%.
#' @param data_tst a data.frame that represents the testing sample, if is not given is calculated from the data by 20%.
#' @param combinations an list that represents the differents combinations to build the deeplearning network, if is not given is created by default.
#' @export
#' @keywords
#' @seealso
#' @return the report of the neural network.
#' @aliases
#' @examples

combofitnets <- function(data_main, target, output = "linear", combinations, data_trn, data_tst, data_vld) {

  if(missing(data_trn) || missing(data_tst) || missing(data_vld)) {
    samples <- create_data_samples(data_main)
    data_trn <- data_main[samples == 1, ]
    data_tst <- data_main[samples == 2, ]
    data_vld <- data_main[samples == 3, ]
  }

  if(missing(combinations)) {
    combinations <- build_combinations()
  }
  results <- data.frame()
  num_comb <- length(combinations$learning_rate)
  best_net <-list()
  current_mse <- 100
  for (i in 1:num_comb) {
    id <- i
    learning_rate <- combinations$learning_rate[i]
    num_hidden <- combinations$num_hidden[i]
    num_neurons <- combinations$num_neurons[i]
    dropout_hidden <- combinations$dropout_hidden[i]
    net <- nn.train(
      x = as.matrix(data_main),
      y = as.matrix(target),
      learningrate = learning_rate,
      hidden = rep.int(num_neurons, num_hidden),
      hidden_dropout = dropout_hidden,
      output = output
    )
    mse <- calculate_mse(data_target = data_tst, nn.predict(nn = net, x = data_tst))
    if(mse < current_mse) {
      best_net <- net
      current_mse <- mse
    }
    print(paste(toString(i)," - ", toString(mse)))
    result <- data.frame(id, learning_rate, num_hidden, num_neurons, dropout_hidden, mse)
    results <- rbind(results, result)
  }
  results <- sorting_result(results)
  create_table_results(results, name = toString(paste("report", output, sep = "_")))
  mse_vld <- calculate_mse(data_target = data_vld, nn.predict(nn = best_net, x = data_vld))
  print(paste("VALIDATE - MSE: ", toString(mse_vld)))
  print(get_best_comb(results))
  return(best_net)
}
