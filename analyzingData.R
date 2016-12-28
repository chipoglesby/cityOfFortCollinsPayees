source("cleaningData.R")
library(ggplot2)
library(lubridate)

data = fetchAndCleanData()
data$year = year(data$gldate)
data$month = month(data$gldate,label=TRUE)
data$day = day(data$gldate)
data$dayOfWeek = wday(data$gldate,label=TRUE)

##What Days of the Month are the Checks Cut?
p = ggplot(data, aes(day, fill = month))
p + geom_density(alpha = 0.5)
#Looks like the end of the month - overwhelmingly

#Who Were the Top Vendors of 2015?
vendorSummary <- data %>% group_by(glvendor) %>%
  filter(year == 2015) %>%
  summarize(
    payments = sum(glamount), 
    count = length(glvendor),
    averagePayment = 
      round(sum(glamount)/ length(glvendor),2)) %>%
  arrange(desc(averagePayment))

vendorTop10 <- top_n(vendorSummary, 10, payments)

p = ggplot(vendorTop10,aes(x=reorder(glvendor,averagePayment),y=averagePayment))
p + geom_bar(stat='identity') + coord_flip() + labs(title='Average Payments to Vendor')


#Top Explanations for 2015 + 2015
explanationSummary <- data %>% group_by(glexplanation) %>%
  summarize(
    payments = sum(glamount), 
    count = length(glexplanation),
    averagePayment = 
      round(sum(glamount)/ length(glexplanation),2)) %>%
  filter(is.na(glexplanation) == FALSE) %>%
  arrange(desc(averagePayment))

explanationTop10 <- top_n(explanationSummary, 15, payments)

p = ggplot(explanationTop10,aes(x=reorder(glexplanation,averagePayment),y=averagePayment))
p + geom_bar(stat='identity') + coord_flip() + labs(title='Average Payments by Explanation')


#Top Explanations for 2015 + 2015
explanationSummary <- data %>% 
  filter(grepl('energy purchases',glexplanation) == FALSE) %>% 
  group_by(glexplanation) %>%
  summarize(
    payments = sum(glamount), 
    count = length(glexplanation),
    averagePayment = 
      round(sum(glamount)/ length(glexplanation),2)) %>%
  filter(is.na(glexplanation) == FALSE) %>%
  arrange(desc(averagePayment))

head(explanationSummary,15)
tail(explanationSummary,25)

explanationTop10 <- top_n(explanationSummary, 50, payments)

p = ggplot(explanationTop10,aes(x=reorder(glexplanation,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip() + labs(title='Payments by Explanation')

#how much were 
a = data[data$glexplanation=='educational programs',]
a = a[is.na(a)==FALSE,]

#Who Were the Top Vendors of 2015?
vendorSummary <- a %>% group_by(glvendor) %>%
  summarize(
    payments = sum(glamount), 
    count = length(glvendor),
    averagePayment = 
      round(sum(glamount)/ length(glvendor),2)) %>%
  arrange(desc(averagePayment))

vendorTop10 <- top_n(vendorSummary, 100, payments)

p = ggplot(vendorTop10,aes(x=reorder(glvendor,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip() + labs(title='Average Payments to Vendor')

plot(data$gldate,data$glamount)


df = data %>% group_by(year,glfund) %>% summarise(payments = sum(glamount))
p = ggplot(df,aes(x=year,y=payments))
p + geom_bar(stat='identity') + facet_wrap(~glfund)
