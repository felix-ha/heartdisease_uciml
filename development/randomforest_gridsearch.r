library(caret)
library(MLmetrics)
library(randomForest)
library(tidyverse)
library(magrittr)
source("helpers.r")


df <- get_training_df_clean()


set.seed(25)
number_of_folds <- 10
folds <- createFolds(df$target, k = number_of_folds)


f <- function(mtry, maxnodes, nodesize, ntree) {
  
  auc <- vector(mode = "numeric", length = number_of_folds)
  
  for(fold_index in c(1:number_of_folds)){
    training <- df[-folds[[fold_index]],]
    test <- df[folds[[fold_index]],]
    
    fit <- randomForest(target ~ ., training,  ntree = ntree, mtry = mtry,
                        nodesize = nodesize, maxnodes = maxnodes)
    # using ratio of poisitive labels as probability
    y_probabilities <- predict(fit, test, type = "vote")[,2]
    
    
    y_true <- ifelse(test$target == "no_disease", 0, 1)
    
    auc[fold_index] <- AUC(y_true = y_true, y_pred = y_probabilities)
    
    
  }
  
  return(mean(auc))
  
}


result <- tibble(auc = vector(mode = "numeric"),
                 maxnodes = vector(mode = "numeric"),
                 nodesize = vector(mode = "numeric"),
                 mtry = vector(mode = "numeric"),
                 ntree = vector(mode = "numeric"))


# Maximum number of terminal nodes trees in the forest can have. 
# If not given, trees are grown to the maximum possible 
# (subject to limits by nodesize).
# If set larger than maximum possible, a warning is issued.
maxnodess <- c(2, 4, 8, 16, 30)

# Minimum size of terminal nodes. 
# Setting this number larger causes smaller trees to be grown 
# (and thus take less time). 
# Note that the default values are different 
# for classification (1) and regression (5).
nodesizes <- c(1, 5, 10)
mtrys <-  c(1, 2, 3, 4, 5)
ntrees <- c(10, 100, 500)

for(mtry in mtrys) {
  for(ntree in ntrees){
    for(maxnodes in maxnodess) {
      for(nodesize in nodesizes){
        
        auc <- f(mtry, maxnodes, nodesize,  ntree)
        result %<>% 
          add_row(auc = auc, 
                  mtry = mtry,
                  maxnodes = maxnodes,
                  nodesize = nodesize,
                  ntree = ntree)
        
        print(auc)
      }
    }
  }
}



result %>%
  arrange(desc(auc))



