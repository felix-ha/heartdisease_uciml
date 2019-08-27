rm(list=ls())


library(caret)
source("utils.r")
heart <- get_opt_df()


set.seed(25)
number_of_folds <- 10
folds <- createFolds(heart$target, k = number_of_folds)

log_reg <- function(fold_index) {
  training <- heart[-folds[[fold_index]],]
  test <- heart[folds[[fold_index]],]
  
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  fit <- glm(target ~ chol, data=training, family =binomial(link = "logit"))
  y_probabilities <- unname(predict(fit, test,  type="response"))
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  
}


fold_indicies <- c(1:number_of_folds)
auc_log_reg <- map_dbl(fold_indicies, log_reg)

median(auc_log_reg)

#invoke_map_dbl(models, f, fold_index = fold_indicies)
