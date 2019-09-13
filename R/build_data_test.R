build_data_test <- function(data_main, train_sample) {
  return(data_main[-train_sample, ])
}
