create_table_results <- function(results, name = "results") {
  write.table(results,
              file = paste(name, "csv", sep = "."),
              row.names = FALSE,
              na = "",
              col.names = TRUE,
              sep = ","
            )
}
