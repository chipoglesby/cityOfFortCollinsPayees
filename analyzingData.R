library(dplyr)

vendorSummary <- data %>% group_by(glvendor) %>%
  summarize(
    payments = sum(glamount), 
    count = length(glvendor),
    averagePayment = 
      round(sum(glamount)/ length(glvendor),2)) %>%
  arrange(desc(averagePayment))

vendorTop10 <- top_n(vendorSummary, 10, payments)