source("cleaningData.R")
library(ggplot2)
library(lubridate)
library(tidyverse)
library(scales)

#data = fetchAndCleanData()
data$year <- year(data$gldate)
data$month <- month(data$gldate,label=TRUE)
data$day <- day(data$gldate)
data$dayOfWeek = wday(data$gldate,label=TRUE)

##What Days of the Month are the Checks Cut?
p = ggplot(data, aes(day, fill = month))
p + geom_density(alpha = 0.5) + scale_y_continuous(name="% of Payments by Day of Month", labels = percent) + ggtitle('Payments by Day of Month')
#Looks like the end of the month - overwhelmingly

##What Days of the Week are the Checks Cut?
p = ggplot(data, aes(dayOfWeek,fill=factor(year),col=factor(year)))
p + geom_bar(position='dodge') + scale_y_continuous(name="Payment Count", labels = comma)

#geom_density(alpha = 0.5)
#Almost everything happens during a weekday (not surprising)
#Thursday is oddly low and Friday oddly high...
#Weirdly the trend was both years and Tuesday grew in 2015 and dropped even beyond 2014 on Thursdays

#Who Were the Top Vendors?
vendorSummary = data %>% group_by(glvendor) %>%
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
explanationSummary = data %>% group_by(glexplanation) %>%
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
explanationSummary = data %>% 
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

explanationTop10 = top_n(explanationSummary, 10, payments)

p = ggplot(explanationTop10,aes(x=reorder(glexplanation,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip() + labs(title='Payments by Explanation')

#how much were 
a = data[data$glexplanation=='educational programs',]
a = a[is.na(a)==FALSE,]

#Who Were the Top Vendors of 2015?
vendorSummary = a %>% group_by(glvendor) %>%
  summarize(
    payments = sum(glamount), 
    count = length(glvendor),
    averagePayment = 
      round(sum(glamount)/ length(glvendor),2)) %>%
  arrange(desc(averagePayment))

vendorTop10 <- top_n(vendorSummary, 10, payments)

p = ggplot(vendorTop10,aes(x=reorder(glvendor,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip() + labs(title='Average Payments to Vendor')

#plot(data$gldate,data$glamount)


df = data %>%
  group_by(SERVICEAREANAME) %>%
  summarise(payments = sum(glamount))
p = ggplot(df,aes(x=reorder(SERVICEAREANAME,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip()




df = data %>%
  group_by(DEPTNAME) %>%
  summarise(payments = sum(glamount)) %>%
  arrange(desc(payments)) %>%
  top_n(20)
p = ggplot(df,aes(x=reorder(DEPTNAME,payments),y=payments))
p + geom_bar(stat='identity') + 
  labs(title='Fig 4. Spending By Department - Top 20',x='') + 
  scale_y_continuous(name="", 
                     labels = dollar) + 
  coord_flip()

df = data %>%
  group_by(GLFUND) %>%
  summarise(payments = sum(glamount)) %>%
  arrange(desc(payments))
p = ggplot(df,aes(x=reorder(GLFUND,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip()

df = data %>%
  filter(GLFUND == 'Golf Fund') %>%
  group_by(glvendor) %>%
  summarise(payments = sum(glamount)) %>%
  arrange(desc(payments)) %>%
  top_n(25)
p = ggplot(df,aes(x=reorder(glvendor,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip()

# df = data %>%
#   filter(GLFUND == 'Keep Fort Collins Great Fund') %>%
#   group_by(glvendor) %>%
#   summarise(payments = sum(glamount)) %>%
#   arrange(desc(payments))
# p = ggplot(df,aes(x=reorder(glvendor,payments),y=payments))
# p + geom_bar(stat='identity') + coord_flip()


df = data %>%
  filter(DEPTNAME == 'Patrol') %>%
  group_by(glvendor) %>%
  summarise(payments = sum(glamount)) %>%
  arrange(desc(payments)) %>%
  top_n(30)
p = ggplot(df,aes(x=reorder(glvendor,payments),y=payments))
p + geom_bar(stat='identity') + coord_flip()
#Scandal with #1 paid vendor = redflex traffic systems
#http://www.chicagotribune.com/news/watchdog/redlight/ct-red-light-cameras-ceo-guilty-met-20150819-story.html


