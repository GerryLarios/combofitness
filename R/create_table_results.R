create_table_results <- function(results, name = "results.csv") {
  write.table(results, file = name, row.names=FALSE, na="", col.names=TRUE, sep=",")
}
