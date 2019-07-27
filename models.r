library(tidyverse)
library(precrec)
library(MLmetrics)
library(caret)
source("utils.r")

visualize_metrics <- function(df, fit, threshold) {
  y_probabilities <- unname(predict(fit, df,  type="response"))
  y_predicted <- ifelse(y_probabilities > threshold, 1, 0)
  y_true <- ifelse(df$target == "no_disease", 0, 1)
  
  accuracy <- Accuracy(y_true = y_true, y_pred = y_predicted)
  precision <- Precision(y_true = y_true, y_pred = y_predicted, positive = "1")
  recall <- Recall(y_true = y_true, y_pred = y_predicted, positive = "1")
  f_1 <- F1_Score(y_true = y_true, y_pred = y_predicted, positive = "1")
  confusion_matrix <- ConfusionMatrix(y_true = y_true, y_pred = y_predicted)
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  
  print(paste("Metrics for fit with threshold", threshold))
  print("Confusion Matrix    : ")
  print(confusion_matrix)
  print(paste("Accuracy  : ", round(accuracy, 3)))
  print(paste("Precision : ", round(precision, 3)))
  print(paste("Recall    : ", round(recall, 3)))
  print(paste("F1        : ", round(f_1, 3)))
  print(paste("AUC       : ", round(auc, 3)))
  
  precrec_obj <- evalmod(scores = y_probabilities, labels = y_true)
  autoplot(precrec_obj)
}

df_raw <- read_csv("data.csv")
heart <- get_df(df_raw)
heart <- get_training_df(heart)
heart_test <- get_test_df(heart)

y_true <- ifelse(heart_test$target == "no_disease", 0, 1)

fit <- glm(target ∼.,
        data=heart ,family =binomial(link = "logit"))


y_probabilities <- unname(predict(fit, heart_test,  type="response"))
auc <- AUC(y_true = y_true, y_pred = y_probabilities)
print(auc)

#folds <- createFolds(heart$target)


      