take_train_sample <- function(main_data, porcent = 0.8) {
  num_rows <- nrow(main_data)
  train <- sample(1:num_rows, porcent * num_rows)
  return(train)
}
