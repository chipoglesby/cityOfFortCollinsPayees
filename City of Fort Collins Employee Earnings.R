library(tidyverse)
library(scales)
library(gridExtra)
library(ggpubr)
library(ggthemes)

data = read_csv('2016 City Employee Earnings  City of Fort Collins.csv')
data$`2016 Earnings` <- as.numeric(gsub('[$,]', '', data$`2016 Earnings`))

summary(data)
ggplot(data, aes(`2016 Earnings`)) + geom_histogram() + 
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(labels = dollar)
  
ggplot(data, aes(`2016 Earnings`)) + geom_density() + 
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(labels = dollar)



department_size = data %>% 
  group_by(`Department`) %>% 
  summarise(spending = sum(`2016 Earnings`), jobs = n()) %>% 
  mutate(per_job_spending = spending / jobs)
head(department_size %>% arrange(-per_job_spending))
ggplot(department_size %>% 
         top_n(20,per_job_spending), 
       aes(x=reorder(Department,per_job_spending), y=per_job_spending)) + 
  geom_bar(stat='identity') + 
  coord_flip()

department_spending = data %>% 
  group_by(`Department`) %>% 
  summarise(spending = sum(`2016 Earnings`), jobs = n()) %>% 
  mutate(per_job_spending = spending / jobs)
head(department_spending %>% arrange(-per_job_spending))

job_title_size = data %>% 
  group_by(`Job Title`) %>% 
  summarise(spending = sum(`2016 Earnings`), jobs = n()) %>% 
  mutate(per_job_spending = spending / jobs)
head(job_title_size %>% arrange(-per_job_spending))
ggplot(job_title_size %>% 
         top_n(20,per_job_spending), 
       aes(x=reorder(`Job Title`,per_job_spending), y=per_job_spending)) + 
  geom_bar(stat='identity') + 
  coord_flip()

job_title_spending = data %>% 
  group_by(`Job Title`) %>% 
  summarise(spending = sum(`2016 Earnings`)) %>% 
  arrange(-spending)
head(job_title_spending)


grouped_stats = data %>%
  group_by(`Job Title`, Department) %>%
  summarise(`Gross 2016 Earnings` = sum(`2016 Earnings`), `Number of Jobs` = n()) %>%
  mutate(`Average Job Earnings` = `Gross 2016 Earnings` /  `Number of Jobs`) %>%
  arrange(-`Average Job Earnings`)
grouped_stats = grouped_stats[1:20,]
head(grouped_stats)

ggplot(grouped_stats, 
       aes(x=reorder(`Job Title`,
                     `Average Job Earnings`), 
           y=`Average Job Earnings`,
           fill=`Department`)) + 
  geom_bar(stat='identity', position='dodge') + 
  coord_flip()





visualizeData = function(data, jobTitle = NA, department = NA, row_limit = 5, sortedBy = 'Number_of_Jobs'){
  tmp = data
  if(!is.na(jobTitle)){
    tmp = tmp %>% filter(grepl(jobTitle,`Job Title`))
  }
  if(!is.na(department)){
    tmp = tmp %>% filter(grepl(department,`Department`))
  }
  
  
  df_limits = tmp %>%
    group_by(`Job Title`, Department) %>%
    summarise(`Gross_2016_Earnings` = sum(`2016 Earnings`), `Number_of_Jobs` = n()) %>%
    mutate(`Average_2016_Earnings` = `Gross_2016_Earnings` /  `Number_of_Jobs`) %>%
    arrange_(.dots = paste0("desc(",sortedBy,")"))
  
  df_limits = df_limits$`Job Title`[1:row_limit]
  print(df_limits)
  
  df = tmp %>% filter(`Job Title` %in% df_limits)

  p = ggplot(df, aes(x = `Job Title`, 
                     y = `2016 Earnings`, 
                     col = `Department`)) + 
    geom_boxplot() + 
    scale_y_continuous(labels = dollar) + 
    ggtitle("City of Fort Collins Employee Earnings") +
    theme_economist()
  
  stable = desc_statby(df %>% rename(Job_Title = `Job Title`), measure.var = "2016 Earnings",
                       grps = c("Job_Title", "Department"))
  stable = stable %>%
    rename(`Job Title` = Job_Title) %>%
    mutate(Jobs = length,
           Mean = dollar(round(mean,0)),
           Median = dollar(round(median,0)),
           Minimum = dollar(round(min,0)),
           Maximum = dollar(round(max,0))) %>%
    select(`Job Title`,Department, Jobs, Mean, Median, Minimum, Maximum) %>%
    arrange(-Jobs)
  
  stable.p <- ggtexttable(stable, rows = NULL, theme = ttheme('lBlack'))
  
  ggarrange(p, stable.p, 
            ncol = 1, nrow = 2,
            heights = c(0.5, 0.5))
}

visualizeData(data, 
              jobTitle = 'FIREFIGHTER I')

visualizeData(data, 
              jobTitle = 'POLICE CORPORAL|POLICE LIEUTENANT|POLICE SERGEANT|POLICE OFFICER', 
              row_limit = 10,
              sortedBy = "Gross_2016_Earnings")

visualizeData(data, 
              department = 'RECREATION',
              row_limit = 10)

