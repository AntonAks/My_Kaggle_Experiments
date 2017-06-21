library(ggplot2)
library(xgboost)
library(data.table)
library(purrr)

train_data <- fread("~/R/My_Kaggle_Experiments/Mercedes-Benz Greener Manufacturing/train.csv", stringsAsFactors = T)
train_data$type <- 1
test_data <- fread("~/R/My_Kaggle_Experiments/Mercedes-Benz Greener Manufacturing/test.csv", stringsAsFactors = T)
test_data$type <- 0


# 0.55592 ####

train_df_model <- train_data
y_train <- train_df_model$y
train_df_model$y <- NULL

#Row binding train & test set for feature engineering
train_test <- rbindlist(l = list(train_df_model, test_data),use.names = T)
#ntrain <- nrow(train_df_model)

features <- names(train_data)

#convert character into integer
for (f in features) {
  if (is.character(train_test[[f]])) {
    levels = sort(unique(train_test[[f]]))
    train_test[[f]] = as.integer(factor(train_test[[f]],levels = levels))
  }
}

#splitting whole data back again
train_x <- train_test[type==1,]
test_x <- train_test[type==0,]

train_x$type <- NULL
test_x$type <- NULL

#convert into numeric for XGBoost implementation
train_x[] <- map(train_x, as.numeric)
test_x[] <- map(test_x, as.numeric)

dtrain <- xgb.DMatrix(as.matrix(train_x),label = y_train)
dtest <- xgb.DMatrix(as.matrix(test_x))


##xgboost parameters
xgb_params <- list(colsample_bytree = 0.7 #how many variables to consider for each tree
                   , subsample = 0.7 #how much of the data to use for each tree
                   , booster = "gbtree"
                   , max_depth = 10 #how many levels in the tree
                   , eta = 0.05 #shrinkage rate to control overfitting through conservative approach
                   , eval_metric = "rmse" 
                   , objective = "reg:linear"
                   , gamma = 0
                   , num_parallel_tree = 5
                   )

#tuning
xgbcv <- xgb.cv(params = xgb_params
                 , data = dtrain
                 ,nrounds = 100
                , nfold = 5
                , showsd = T
                , stratified = T
                , print.every.n = 10
                , early.stop.round = 20
                , maximize = F)



#train data
set.seed(51)
gb_dt <- xgb.train(params = xgb_params
                   , data = dtrain
                   , nrounds = 102
                   , print_every_n = 10
                   , early_stopping_rounds = 10
                   , maximize = F
                   , watchlist = list(train=dtrain))




test_preds <- predict(gb_dt,dtest)
result <- data.table(test_data[,1], 'y' = test_preds)

check_res <- fread("Mercedes-Benz Greener Manufacturing/test_sub.csv")
check_res$y_pred <- result$y
check_res[,check:= abs(y - y_pred) / y]
e <- mean(check_res$check)


# Save results
file_name <- as.character(gsub(' ','_', paste0('submission_', date(), '.csv')))
write_csv(x = result, paste0('Mercedes-Benz Greener Manufacturing/', file_name))







