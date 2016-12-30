library(rvest)
library(jsonlite)
library(gdata)
library(dplyr)

fetchAndCleanData = function(){
  landingPage <- read_html("http://www.fcgov.com/opendata/")
  
  links <- landingPage %>%
    html_nodes('a') %>%
    html_attr('href')
  
  glServiceArea <- fromJSON("https://s3.amazonaws.com/fortcollins-opendata/glservicearea.json")
  glFund <- fromJSON("https://s3.amazonaws.com/fortcollins-opendata/glfund.json")
  glDepartment <- fromJSON("https://s3.amazonaws.com/fortcollins-opendata/gldepartment.json")
  glLedgerLinks <- links[grepl("https://s3.amazonaws.com/fortcollins-opendata/generalledger-",links) == TRUE]
  
  data <- fromJSON(glLedgerLinks[1])
  
  for(i in 2:length(glLedgerLinks)){
    data <- bind_rows(data, fromJSON(glLedgerLinks[i]))
  }
  
  # Lowercase column names
  names(data) <- tolower(names(data))
  
  # Lowercase vendor info
  data$glvendor <- tolower(data$glvendor)
  data$glexplanation <- tolower(data$glexplanation)
  
  # Clean up data types
  data$glfund <- as.factor(data$glfund)
  data$glamount <- as.numeric(data$glamount)
  data$gldeptno <- as.factor(data$gldeptno)
  data$glserviceareaid <- as.factor(data$glserviceareaid)
  data$glpo <- as.factor(data$glpo)
  data$gldate <- as.Date(data$gldate)
  
  # Columns are null and can be dropped
  
  data$glvisadesc <- NULL
  data$glvisacardholder <- NULL
  
  library(gdata)
  data = merge(data,glServiceArea,by.x='glserviceareaid',by.y='SERVICEAREAID')
  data$SERVICEAREANAME = trim(data$SERVICEAREANAME,'right')
  data = merge(data,glFund,by.x='glfund',by.y='GLFUND')
  data$GLFUND = trim(data$GLFUND,'right')
  data = merge(data,glDepartment,by.x='gldeptno',by.y='DEPTNO')
  data$DEPTNAME = trim(data$DEPTNAME,'right')
  
  return(data)
}