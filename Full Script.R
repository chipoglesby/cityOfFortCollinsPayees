library(rvest)
library(jsonlite)
library(gdata)
library(dplyr)

landingPage = read_html("http://www.fcgov.com/opendata/")

links = landingPage %>%
  html_nodes('a') %>%
  html_attr('href')

glServiceAreaLink = "https://s3.amazonaws.com/fortcollins-opendata/glservicearea.json"
glFundLink = "https://s3.amazonaws.com/fortcollins-opendata/glfund.json"
glDepartmentLink = "https://s3.amazonaws.com/fortcollins-opendata/gldepartment.json"
glLedgerLinks = links[grepl("https://s3.amazonaws.com/fortcollins-opendata/generalledger-",links)==TRUE]

df = data.frame(do.call(rbind,fromJSON(glLedgerLinks[1])))

for(i in 2:length(glLedgerLinks)){
  df = rbind(df,do.call(rbind,fromJSON(glLedgerLinks[i])))
}

funds = data %>% group_by(GLFUND)

