#' @title Combinatios of hyperparameters to train neural networks with binary outputs
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

fitnet_binary <- function(
  data_main,
  target,
  output = "binary",
  combinations,
  data_trn,
  data_tst,
  data_vld
  ) {
  # take the samples
  if(missing(data_trn) || missing(data_tst) || missing(data_vld)) {
    samples <- create_data_samples(data_main)
    data_trn <- data_main[samples == 1, ]
    data_tst <- data_main[samples == 2, ]
    data_vld <- data_main[samples == 3, ]
  }

  # build combinations
  if(missing(combinations))
    combinations <- build_combinations()

  # instance the report data frame
  results <- data.frame()
  num_comb <- length(combinations$learning_rate)
  best_net <-list()

  current_pccc <- 0

  for (i in 1:num_comb) {
    # append current hyperparameters
    id <- i
    learning_rate <- combinations$learning_rate[i]
    num_hidden <- combinations$num_hidden[i]
    num_neurons <- combinations$num_neurons[i]
    dropout_hidden <- combinations$dropout_hidden[i]

    # treaning net
    net <- nn.train(
      x = as.matrix(data_no_target(data_trn, target)),
      y = as.matrix(data_trn[target]),
      learningrate = learning_rate,
      hidden = rep.int(num_neurons, num_hidden),
      hidden_dropout = dropout_hidden
    )

    #pccc testingt
    pccc <- calculate_pccc(predicted = classify_binary(nn.predict(nn = net, x = data_no_target(data_tst, target))), data_target = data_tst[target])

    # pccc validate
    pccc_validate <- calculate_pccc(predicted = classify_binary(nn.predict(nn = net, x = data_no_target(data_vld, target))), data_target = data_vld[target])

    # change to best pccc (testing)
    if(pccc > current_pccc) {
      best_net <- net
      current_pccc <- pccc
    }

    # Printe current results
    print(paste(toString(i)," - ", toString(pccc)))

    # Make data frame report
    result <- data.frame(id, learning_rate, num_hidden, num_neurons, dropout_hidden, pccc, pccc_validate)
    results <- rbind(results, result)

  }

  # make csv report
  results <- sorting_result(results, "pccc", F)
  create_table_results(results, name = toString(paste("report", output, sep = "_")))

  # make validation of the best net
  pccc_vld <- calculate_pccc(predicted = classify_binary(nn.predict(nn = best_net, x = data_no_target(data_vld, target))), data_target = data_vld[target])

  # print best net
  print("BEST NET")
  print(paste("VALIDATE - pccc: ", toString(pccc_vld)))

  return(results)
}
