library(parallel)
library(caret)
library(MLmetrics)
library(rpart)
library(ipred)
library(tidyverse)
library(magrittr)
source("helpers.r")


df <- get_training_df_clean()


set.seed(25)
number_of_folds <- 10
folds <- createFolds(df$target, k = number_of_folds)


ncores <- detectCores(logical = TRUE)

cl <- makeCluster(ncores)

clusterEvalQ(cl, {
  library(rpart)
  library(MLmetrics)
  library(ipred)
})

clusterExport(cl, c("folds", "df"))





calculate_auc <- function(fold_index) {
  
  training <- df[-folds[[fold_index]],]
  test <- df[folds[[fold_index]],]
  
  fit <- bagging(target ~ ., training,
                 nbagg = 100,
                 coob = FALSE,
                 control = rpart.control(maxdepth = maxdepth, minbucket = minbucket,
                                         minsplit = minsplit), cp = cp)
  
  y_probabilities <- predict(fit, test, type = "prob")[,2]
  
  
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  return(AUC(y_true = y_true, y_pred = y_probabilities))
}


f <- function(maxdepth, minsplit, minbucket, cp) {
  
  auc <- vector(mode = "numeric", length = number_of_folds)
  
  auc <- unlist(clusterApply(cl, x = 1:number_of_folds, fun = calculate_auc))
  
  return(mean(auc))
  
}


result <- tibble(auc = vector(mode = "numeric"),
                 minsplit = vector(mode = "numeric"),
                 minbucket = vector(mode = "numeric"),
                 maxdepth = vector(mode = "numeric"),
                 cp = vector(mode = "numeric"))


# the minimum number of observations that must exist in a node in order for a
# split to be attempted.
minsplits <- c(10, 20, 30)

# the minimum number of observations in any terminal <leaf> node. If only
# one of minbucket or minsplit is specified, the code either sets minsplit to
# minbucket*3 or minbucket to minsplit/3, as appropriate.
minbuckets <-  c(3, 7, 10)

# Set the maximum depth of any node of the final tree, with the root node counted
# as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit
# machines  r
maxdepths <-  c(5, 10, 20)

cps <- c(0.1, 0.01, 0.001)

for(maxdepth in maxdepths) {
  for(cp in cps){
    for(minsplit in minsplits) {
      for(minbucket in minbuckets){
        
        clusterExport(cl, c("maxdepth", "minsplit", "minbucket", "cp"))
        
        auc <- f(maxdepth, minsplit, minbucket,  cp)
        result %<>% 
          add_row(auc = auc, 
                  maxdepth = maxdepth,
                  minsplit = minsplit,
                  minbucket = minbucket,
                  cp = cp)
        
        print(auc)
      }
    }
  }
}



print(result %>%
  arrange(desc(auc)))



stopCluster(cl)

# # A tibble: 81 x 5
# auc minsplit minbucket maxdepth    cp
# <dbl>    <dbl>     <dbl>    <dbl> <dbl>
#   1 0.916       20        10       10 0.1  


