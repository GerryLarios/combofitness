maize_data$YB <- ifelse(maize_data$Yield > mean(maize_data$Yield), 1, 0)
