library(ggplot2)
library(xgboost)
library(data.table)


train_data <- fread("~/R/My_Kaggle_Experiments/Mercedes-Benz Greener Manufacturing/train.csv", stringsAsFactors = T)
train_data$type <- 'train'
test_data <- fread("~/R/My_Kaggle_Experiments/Mercedes-Benz Greener Manufacturing/test.csv", stringsAsFactors = T)
test_data$type <- 'test'

full_data <- rbindlist(l = list(train_data, test_data), use.names = T, fill = T)


ggplot(data = full_data[type =='train',], aes(x = y,fill = type)) +
  geom_histogram(col = 'black'
                 ,bins = 120)



xgb.model <- xgboost(data = data.matrix(train_data[,c(2,11:20)])
                     ,label = y) 
