rm(list = ls())
source("helpers.r")
library(MLmetrics)
library(magrittr)


learner_A <- function(training, test) {
  name <- "log reg oldpeak"
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  fit <- glm(target ~ oldpeak, data=training, family =binomial(link = "logit"))
  y_probabilities <- predict(fit, test,  type="response")
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  return(list(name = name, auc = auc))
}

learner_B <- function(training, test) {
  name <- "log reg chol"
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  fit <- glm(target ~ chol, data=training, family =binomial(link = "logit"))
  y_probabilities <- predict(fit, test,  type="response")
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  return(list(name = name, auc = auc))
}

learner_C <- function(training, test) {
  name <- "log reg age"
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  fit <- glm(target ~ age, data=training, family =binomial(link = "logit"))
  y_probabilities <- predict(fit, test,  type="response")
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  return(list(name = name, auc = auc))
}


cross_validation_52 <- function(learner_A, learner_B, df){
  p_1_t <- 0
  s = vector(mode = "numeric", length = 5)
  
  for(i in c(1:5)) {
    set.seed(25 + i)
    number_of_folds <- 2
    folds <- createFolds(df$target, k = number_of_folds)
    
    for(fold_index in c(1:number_of_folds)){
      S_1 <- df[-folds[[fold_index]],]
      S_2 <- df[folds[[fold_index]],]
      
      A_p_1 <- learner_A(training = S_1, test = S_2)[["auc"]]
      B_p_1 <- learner_B(training = S_1, test = S_2)[["auc"]]
      
      A_p_2 <- learner_A(training = S_2, test = S_1)[["auc"]]
      B_p_2 <- learner_B(training = S_2, test = S_1)[["auc"]]
      
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
  return(t)
}

cross_validation <- function(learner_A, learner_B, df){
  set.seed(25)
  number_of_folds <- 10
  folds <- createFolds(df$target, k = number_of_folds)
  model_result <- tibble(model = vector("character"),
                         auc = vector("numeric"))
  
  for(fold_index in c(1:number_of_folds)){
      training <- df[-folds[[fold_index]],]
      test <- df[folds[[fold_index]],]
      
      result_A<- learner_A(training, test)
      result_B <- learner_B(training, test)
      
      model_result %<>%
        add_row(model = result_A[[1]], auc = result_A[[2]]) %>%
        add_row(model = result_B[[1]], auc = result_B[[2]])
      
    }
  
  return(model_result)
  
}

cross_validation_selection <- function(learner_A, learner_B){
  # use p = 0.81 to get equally sized sets S_1 and S_2
  df <- get_training_df(p = 0.81)
  t <- cross_validation_52(learner_A, learner_B, df)
  
  df <- get_training_df(p = 0.8)
  model_result<- cross_validation(learner_A, learner_B, df)
  return(list(model_result = model_result, t = t))
}

plot_result <- function(model_result){
  model_result <- result[["model_result"]]
  t <- result[["t"]]
  
  models <- (model_result %>% distinct(model))[["model"]]
  model_result %>%
    mutate(model = factor(model, levels = models)) %>%
    ggplot(aes(x = model, y = auc))+
    geom_boxplot() +
    labs(
      x = "Modells",
      y = "AUC",
      title = "Cross validation comparison of modells",
      subtitle = paste0("5 x 2 cross validation  t statistic : ",round(t,3))
    ) + 
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0)
          )
}


result <- cross_validation_selection(learner_C, learner_B)
plot <- plot_result(result)

model_summary <- result[["model_result"]] %>%
  group_by(model) %>%
  summarize(mean = mean(auc),
            median = median(auc),
           sd = sd(auc))

print(model_summary)
print(plot)



