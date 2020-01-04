library(caret)
library(MLmetrics)
library(xgboost)
library(tidyverse)
library(magrittr)
source("helpers.r")


df <- get_training_df_clean()


set.seed(25)
number_of_folds <- 10
folds <- createFolds(df$target, k = number_of_folds)


f <- function( eta, nrounds, max_depth) {
  
  auc <- vector(mode = "numeric", length = number_of_folds)
  
  for(fold_index in c(1:number_of_folds)){
    training <- df[-folds[[fold_index]],]
    test <- df[folds[[fold_index]],]
    
    df_model_matrix <- model.matrix(target ~ .-1, training)
    dtrain <- xgb.DMatrix(df_model_matrix, label = training$target)

    param <- list(max_depth = max_depth, eta = eta, verbose = 0, nthread = 2)
    
    fit <- xgb.train(param, dtrain, nrounds = nrounds)
    
    
    df_model_matrix <- model.matrix(target ~ .-1, test)
    dtest <- xgb.DMatrix(df_model_matrix, label = test$target)
    y_probabilities <- predict(fit, dtest)

    y_true <- ifelse(test$target == "no_disease", 0, 1)
    
    auc[fold_index] <- AUC(y_true = y_true, y_pred = y_probabilities)
    
    
  }
  
  return(mean(auc))
  
}


result <- tibble(auc = vector(mode = "numeric"),
                 eta = vector(mode = "numeric"),
                 nrounds = vector(mode = "numeric"),
                 max_depth = vector(mode = "numeric"))



etas <- c(0.1, 0.2, 0.3, 0.5, 0.75)
nroundss <- c(5, 25, 50, 100, 150)
max_depths <- c(2, 3, 4, 5, 8)


  for(max_depth in max_depths){
    for(eta in etas) {
      for(nrounds in nroundss){
        
        auc <- f( eta, nrounds,  max_depth)
        result %<>% 
          add_row(auc = auc, 
                  eta = eta,
                  nrounds = nrounds,
                  max_depth = max_depth)
        
        print(auc)
      }
    }
  }




result %>%
  arrange(desc(auc))
