create_data_samples <- function(data_main, prob = c(0.6, 0.2, 0.2)) {
  return(
    sample(1:3,
           size = nrow(data_main),
           replace = T,
           prob = prob
           )
    )
}
