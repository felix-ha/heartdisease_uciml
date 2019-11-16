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

f <- function(x) {

auc <- vector(mode = "numeric", length = number_of_folds)

for(fold_index in c(1:number_of_folds)){
  training <- df[-folds[[fold_index]],]
  test <- df[folds[[fold_index]],]
  
  
  model <- model.matrix(target ~ ., training)[,-1]
  y <- ifelse(training$target == "no_disease", 0, 1)
  fit <- glmnet(model, y, alpha = x[1], family = "binomial", lambda = x[2], standardize = TRUE)
  
  x_test  <- model.matrix(target ~ ., test)[,-1]
  y_probabilities <- predict(fit, x_test,  type="response")
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  auc[fold_index] <- AUC(y_true = y_true, y_pred = y_probabilities)

  
}

return(-mean(auc))

}

x_0 <- c(alpha = 0.5, lambda = 0.5)




h <- 0.1
steplength <- 0.05
eps <- 0.01
iter_max <- 5



x_current <- x_0
iter = 0

result <- tibble(auc = vector(mode = "numeric"),
                 gradient = vector(mode = "numeric"),
                 alpha = vector(mode = "numeric"),
                 lambda = vector(mode = "numeric"),
                k = vector(mode = "integer"))

repeat ({

dx_1 <- (f(x_current + c(h,0)) - f(x_current - c(h,0))) / (2 * h)
dx_2 <- (f(x_current + c(0, h)) - f(x_current - c(0, h))) / (2 * h)
gradient <- c(dx_1, dx_2)

x_current <- x_current - steplength * gradient
norm_grad <- norm(gradient, type = "2")

iter <- iter + 1

if(norm_grad < eps) break

if(iter > iter_max) {
  print("max iter reached")
  break
}


result %<>% 
  add_row(auc = -f(x_current), 
          gradient = norm_grad, 
          alpha = x_current[1],
          lambda = x_current[2],
          k = iter)

print(top_n(result, n = 1)$auc)

})


print(tail(result))
