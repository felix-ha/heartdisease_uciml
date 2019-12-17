library(parallel)
library(rpart)
library(caret)
library(microbenchmark)
source("helpers.r")



df <- get_training_df_clean()


set.seed(25)
number_of_folds <- 120
folds <- createFolds(df$target, k = number_of_folds)



calculate_auc <- function(fold_index) {

training <- df[-folds[[fold_index]],]
test <- df[folds[[fold_index]],]

fit <- rpart(target ~., data=training, method = "class")
# using ratio of poisitive labels as probability
y_probabilities <- predict(fit, test)[,2]


y_true <- ifelse(test$target == "no_disease", 0, 1)

return(AUC(y_true = y_true, y_pred = y_probabilities))
}

auc_loop <- function(){
  auc <- vector(mode = "numeric", length = number_of_folds)
  for(i in 1:number_of_folds) {
    auc[i] <- calculate_auc(i)
  }
  # Sys.sleep(1)
  return(auc)
}

auc_functional <- function(){
  map_dbl(1:number_of_folds, calculate_auc)
}

ncores <- detectCores(logical = TRUE)

cl <- makeCluster(ncores)

clusterEvalQ(cl, {
  library(rpart)
  library(MLmetrics)
})

clusterExport(cl, c("folds", "df"))

#result <- clusterApply(cl, x = 1:10, fun = calculate_auc)

# print(system.time(clusterApply(cl, x = 1:number_of_folds, fun = calculate_auc)))
# print(system.time(auc_loop()))
# print(system.time(auc_functional()))


microbenchmark(auc_loop(),
               auc_functional(),
               clusterApply(cl, x = 1:number_of_folds, fun = calculate_auc), 
                times = 10L)


stopCluster(cl)
