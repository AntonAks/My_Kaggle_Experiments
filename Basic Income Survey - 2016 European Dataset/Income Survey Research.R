library(data.table)
library(ggplot2)
library(plotly)

income_data <- fread(paste0(getwd(),"/Basic Income Survey - 2016 European Dataset/basic_income_dataset_dalia.csv"))

# DISTRIBUSION BY AGE + GENDER
ggplot(data = income_data, aes(x = age)) + 
  geom_histogram(aes(y = ..density..)
                 ,col = 'black'
                 ,fill = 'white') +
  geom_density(alpha = 0.2, fill='#FF6666') + 
  facet_grid(gender ~.)

# DISTRIBUSION BY AGE + RURAL
ggplot(data = income_data, aes(x = age)) + 
  geom_histogram(aes(y = ..density..)
                 ,col = 'black'
                 ,fill = 'white') +
  geom_density(alpha = 0.2, fill='#FF6666') + 
  facet_grid(rural ~.)

# DISTRIBUSION BY AGE + EDUCATION

income_data[dem_education_level == 'no', dem_education_level:="no"]
income_data[dem_education_level == 'low', dem_education_level:="3. low"]
income_data[dem_education_level == 'medium', dem_education_level:="2. medium"]
income_data[dem_education_level == 'high', dem_education_level:="1. high"]

ggplotly(
  ggplot(data = income_data, aes(x = age, fill = dem_education_level)) + 
    geom_histogram(bins = 50
                 ,col = 'black'
                 ,alpha = 0.7)
)


ggplotly(
  ggplot(data = income_data, aes(x = age, fill = dem_education_level)) + 
    geom_histogram(bins = 50
                   ,position = "fill"
                   ,col = 'black'
                   ,alpha = 0.5)
)



prop.table(table(income_data$dem_education_level, income_data$age_group),margin = 2)
prob_t <-  data.table(prop.table(table(income_data$dem_education_level, income_data$age_group),margin = 2))

names(prob_t) <- c('dem_education_level', 'age_group', 'probability')

ggplotly(
ggplot(data = prob_t, aes(x = age_group, y = probability, fill = dem_education_level)) + 
  geom_bar(stat = 'identity'
           ,alpha=0.7
           ,col = 'black')
)

