library(ggplot2)
library(plotly)
library(xgboost)
library(data.table)
library(purrr)
library(readr)

train_data <- fread("~/R/My_Kaggle_Experiments/Mercedes-Benz Greener Manufacturing/train.csv", stringsAsFactors = T)
train_data$type <- 1
test_data <- fread("~/R/My_Kaggle_Experiments/Mercedes-Benz Greener Manufacturing/test.csv", stringsAsFactors = T)
test_data$type <- 0

ggplotly(
ggplot(data = train_data, aes(x = y)) + 
  geom_histogram(bins = 100, colour = 'black', fill = 'white')
)


train_df_model <- train_data
y_train <- train_df_model$y
train_df_model$y <- NULL

#Row binding train & test set for feature engineering
train_test <- rbindlist(l = list(train_df_model, test_data),use.names = T)
#ntrain <- nrow(train_df_model)
features <- names(train_data)


train_test <- train_test[,-c(378,377,376,375, 364, 359
                             , 358, 357, 346, 332, 312
                             , 303, 300, 281, 8, 9, 10, 11
                             , 17, 19, 23, 26, 27, 29, 32
                             , 33, 35, 48, 40, 52, 53, 58
                             , 55, 71, 79, 70, 72, 74, 75)]


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
xgb_params <- list(booster = "gbtree" 
                   , objective = "reg:linear"
                   , eta=0.005 # 0.005 - BEST
                   , gamma=0
                   , max_depth=4 # 4 - BEST
                   , subsample=0.7 # 0.95 - BEST
                   , min_child_weight = 3 # ??? try to comment
                   , base_score=mean(y_train)
)



#tuning
xgbcv <- xgb.cv(params = xgb_params,
                data = dtrain,
                nrounds = 1000,
                nfold = 5,
                print_every_n = 10, 
                early_stopping_rounds = 20,
                maximize = F,
                prediction = F)



#train data
set.seed(12)
gb_dt <- xgb.train(params = xgb_params
                   , data = dtrain
                   , nrounds = 769
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

write_csv(x = result, 'Mercedes-Benz Greener Manufacturing/final_submission.csv')





