rm(list = ls())
source("helpers.r")
library(MLmetrics)
library(magrittr)


learner_A <- function(training, test) {
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  fit <- glm(target ~ oldpeak, data=training, family =binomial(link = "logit"))
  y_probabilities <- predict(fit, test,  type="response")
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
}

learner_B <- function(training, test) {
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  fit <- glm(target ~ chol, data=training, family =binomial(link = "logit"))
  y_probabilities <- predict(fit, test,  type="response")
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
}

# use p = 0.81 to get equally sized sets S_1 and S_2
df <- get_training_df(p = 0.81)

p_1_t <- 0
s = vector(mode = "numeric", length = 5)

for(i in c(1:5)) {
  set.seed(25 + i)
  number_of_folds <- 2
  folds <- createFolds(df$target, k = number_of_folds)
  
  for(fold_index in c(1:number_of_folds)){
    S_1 <- df[-folds[[fold_index]],]
    S_2 <- df[folds[[fold_index]],]
    
    A_p_1 <- learner_A(training = S_1, test = S_2)
    B_p_1 <- learner_B(training = S_1, test = S_2)
    
    A_p_2 <- learner_A(training = S_2, test = S_1)
    B_p_2 <- learner_B(training = S_2, test = S_1)
    
    p_1 <-  A_p_1 - B_p_1
    p_2 <-  A_p_2 - B_p_2
    p_mean <-  (p_1 + p_2) / 2
    
    s[i] <- (p_1 - p_mean) *  (p_1 - p_mean) + (p_2 - p_mean) *  (p_2 - p_mean) 
    
    if(i == i) p_1_t <- p_1
  }
}

t <- p_1_t / sqrt(mean(s))
alpha <-  0.05
p <- 1 - alpha / 2
t_5 <- qt(p, df = 5)
p_value <- (1 - pt(t, df = 5)) * 2

reject <- abs(t) > t_5

print(paste0("5x2cv t statistic: ", round(t, 3)))
print(paste0("p_value: ", round(p_value, 3), " alpha: ", alpha))
print(paste0("learners are significant different: ", reject))


