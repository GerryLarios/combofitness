sorting_result <- function(result, concept = "mse", ascendig = T) {

  if(ascendig)
    return(result[order(result[concept]), ])
  else
    return(result[order(-result[concept]), ])

}
