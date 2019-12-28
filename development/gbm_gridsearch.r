library(caret)
library(MLmetrics)
library(tidyverse)
library(magrittr)
library(gbm)
source("helpers.r")


df <- get_training_df_clean()


set.seed(25)
number_of_folds <- 10
folds <- createFolds(df$target, k = number_of_folds)





calculate_auc <- function(fold_index, n.trees, interaction.depth, n.minobsinnodes) {

  
  training <- df[-folds[[fold_index]],]
  test <- df[folds[[fold_index]],]
  
  training$target <- ifelse(training$target == "no_disease", 0, 1)
  
  fit <- gbm(target ~ ., data = training, distribution = "bernoulli",
             n.trees = n.trees, interaction.depth = interaction.depth,
             n.minobsinnode = n.minobsinnodes)
  


  y_probabilities <- predict(fit, test, type = "response", n.trees = n.trees)
  
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  

  
  return(AUC(y_true = y_true, y_pred = y_probabilities))
}


f <- function(n.trees, interaction.depth, n.minobsinnodes) {


  
  auc <- vector(mode = "numeric", length = number_of_folds)

  
  for(i in 1:number_of_folds) {
    auc[i] <-  calculate_auc(i, n.trees, interaction.depth, n.minobsinnodes)
  }



  return(mean(auc))
  
}


result <- tibble(auc = vector(mode = "numeric"),
                 n.trees = vector(mode = "numeric"),
                 interaction.depth = vector(mode = "numeric"),
                 n.minobsinnode = vector(mode = "numeric"))


n.treess = c(10, 50, 100, 150, 200)
interaction.depths = c(1, 2, 4, 6, 8, 10)
n.minobsinnodes = c(1, 5, 10, 15, 25)

for(n.trees in n.treess) {
  for(interaction.depth in interaction.depths){
    for(n.minobsinnode in n.minobsinnodes) {

      
        
         auc <- f(n.trees, interaction.depth, n.minobsinnodes)
        result %<>%
          add_row(auc = auc,
                  n.trees = n.trees,
                  interaction.depth = interaction.depth,
                  n.minobsinnode = n.minobsinnode)

        print(auc)
      }
    }
  }




print(result %>%
        arrange(desc(auc)))



