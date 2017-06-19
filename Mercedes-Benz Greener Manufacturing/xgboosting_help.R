library(xgboost)
library(data.table)


mtcars$n <- c(1:nrow(mtcars))

train <- mtcars[mtcars$n %% 2 ==0,]
test <- mtcars[mtcars$n %% 2 ==1,]

setDT(train)
setDT(test)


labels <- train$mpg
ts_labels <- test$mpg

new_tr <- model.matrix(~.+0,data = train[,-c("mpg"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c("mpg"),with=F])

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label = ts_labels)



