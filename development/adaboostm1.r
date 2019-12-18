library(caret)
library(MLmetrics)
library(fastAdaboost)
library(tidyverse)
library(magrittr)
source("helpers.r")


df <- get_training_df_clean()


set.seed(25)
number_of_folds <- 10
folds <- createFolds(df$target, k = number_of_folds)
auc <- vector(mode = "numeric", length = number_of_folds)

for(fold_index in c(1:number_of_folds)){
  training <- df[-folds[[fold_index]],]
  test <- df[folds[[fold_index]],]
  
  
  training <- as.data.frame(training)
  
  fit <- adaboost(target ~ ., training, nIter = 100)
  y_probabilities <- predict(fit, test)$prob[,2]
  
  
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  auc[fold_index] <- AUC(y_true = y_true, y_pred = y_probabilities)
   print(auc[fold_index])

  
}

cat("\n")
print(mean(auc))








