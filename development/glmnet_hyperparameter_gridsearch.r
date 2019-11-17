library(caret)
library(MLmetrics)
library(glmnet)
library(tidyverse)
library(magrittr)
source("helpers.r")

df <- get_training_df_clean()


set.seed(25)
number_of_folds <- 10
folds <- createFolds(df$target, k = number_of_folds)

f <- function(alpha, lambda) {
  
  auc <- vector(mode = "numeric", length = number_of_folds)
  
  for(fold_index in c(1:number_of_folds)){
    training <- df[-folds[[fold_index]],]
    test <- df[folds[[fold_index]],]
    
    
    model <- model.matrix(target ~ ., training)[,-1]
    y <- ifelse(training$target == "no_disease", 0, 1)
    fit <- glmnet(model, y, alpha = alpha, family = "binomial", lambda = lambda, standardize = TRUE)
    
    x_test  <- model.matrix(target ~ ., test)[,-1]
    y_probabilities <- predict(fit, x_test,  type="response")
    y_true <- ifelse(test$target == "no_disease", 0, 1)
    
    auc[fold_index] <- AUC(y_true = y_true, y_pred = y_probabilities)
    
    
  }
  
  return(mean(auc))
  
}


result <- tibble(auc = vector(mode = "numeric"),
                 alpha = vector(mode = "numeric"),
                 lambda = vector(mode = "numeric"))

alphas <-  seq(1, 0, -0.1)
lambdas <- seq(1, 0, -0.1)
  
for(alpha in alphas) {
  for(lambda in lambdas){
    
    result %<>% 
      add_row(auc = f(alpha, lambda), 
              alpha = alpha,
              lambda = lambda)
  }
}
  


result %>%
  arrange(desc(auc))



