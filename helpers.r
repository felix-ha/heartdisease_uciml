library(tidyverse)
library(caret)
library(MLmetrics)


get_df <- function(df){

  result <- df %>%
    transmute(age,
              thalach,
              trestbps,
              oldpeak,
              ca,
              chol,
              restecg = factor(df$restecg, levels = c(0,1,2), labels = c("normal", "ST-T_abnormalty", "hypertrophy")),
              fbs = factor(df$fbs, levels = c(0,1), labels = c("no", "yes")),
              sex = factor(df$sex, levels = c(0,1), labels = c("Female", "Male")),
              exang = factor(df$exang, levels = c(0,1), labels = c("no", "yes")),
              cp = factor(df$cp, levels = c(0,1,2,3), labels = c("typical_angina", "atypical_angina","non-anginal_pain", "asymptomatic")),
              slope = factor(df$slope, levels = c(0,1,2), labels = c("upsloping", "flat", "downsloping")),
              target = factor(df$target, levels = c(0,1), labels = c("no_disease", "disease")),
              thal = factor(df$thal,levels = c(0,1,2,3), labels = c("normal", "fixed_defect", "reversable_defect", "?"))
    )
}


get_clean_df <- function(df) {
  
  df <- df %>%
    filter(restecg != 2) %>%
    filter(thal != 0)


  result <- df %>%
    transmute(age,
              thalach,
              trestbps,
              oldpeak,
              ca,
              chol,
              restecg = factor(df$restecg, levels = c(0,1), labels = c("normal", "ST-T_abnormalty")),
              fbs = factor(df$fbs, levels = c(0,1), labels = c("no", "yes")),
              sex = factor(df$sex, levels = c(0,1), labels = c("Female", "Male")),
              exang = factor(df$exang, levels = c(0,1), labels = c("no", "yes")),
              cp = factor(df$cp, levels = c(0,1,2,3), labels = c("typical_angina", "atypical_angina","non-anginal_pain", "asymptomatic")),
              slope = factor(df$slope, levels = c(0,1,2), labels = c("upsloping", "flat", "downsloping")),
              target = factor(df$target, levels = c(0,1), labels = c("no_disease", "disease")),
              thal = factor(df$thal,levels = c(1,2,3), labels = c("fixed_defect", "reversable_defect", "?"))
    )
}

get_training_df_clean <- function(p = 0.8) {
  set.seed(25)
  suppressMessages({
  df_raw <- read_csv("data.csv")})
  df <- get_clean_df(df_raw)
  inTraining <- createDataPartition(df$target, p = p, list = FALSE)
  training <- df[inTraining,]
  # testing  <- df[-inTraining,]
  return(training)
}

get_training_df <- function(p = 0.8) {
  set.seed(25)
  df_raw <- read_csv("data.csv")
  df <- get_df(df_raw)
  inTraining <- createDataPartition(df$target, p = p, list = FALSE)
  training <- df[inTraining,]
  # testing  <- df[-inTraining,]
  return(training)
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
  
  return(list(t = t, p_value = p_value))
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
  df <- get_training_df_clean(p = 0.81)
  cv_52_result <- cross_validation_52(learner_A, learner_B, df)

  df <- get_training_df_clean(p = 0.8)
  model_result<- cross_validation(learner_A, learner_B, df)

    return(list(model_result = model_result, t = cv_52_result$t, p_value = cv_52_result$p_value))
}

plot_result <- function(model_result){
  model_result <- result$model_result
  t <- result$t
  p_value <- result$p_value
  
  models <- (model_result %>% distinct(model))[["model"]]
  model_result %>%
    mutate(model = factor(model, levels = models)) %>%
    ggplot(aes(x = model, y = auc))+
    geom_boxplot() +
    labs(
      x = "Models",
      y = "AUC",
      title = "Cross validation comparison of models",
      subtitle = paste0("5 x 2 cross validation  t-statistic: ",round(t,3), " p value: ", round(p_value,3))
    ) + 
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0)
    )
}


cv_compare_learner <- function(learners, df){
  set.seed(25)
  number_of_folds <- 10
  folds <- createFolds(df$target, k = number_of_folds)
  
  model_result <- tibble(model = vector("character"),
                         auc = vector("numeric"))
  
  for(learner in learners){
    for(fold_index in c(1:number_of_folds)){
      training <- df[-folds[[fold_index]],]
      test <- df[folds[[fold_index]],]
      
      result <- learner(training, test)
      
      
      model_result %<>%
        add_row(model = result[[1]], auc = result[[2]])
      
    }
  }
  
  return(model_result)
}


plot_best_learners <- function(model_result){
  
  
  models <- (model_result %>% distinct(model))[["model"]]
  model_result %>%
    mutate(model = factor(model, levels = models)) %>%
    ggplot(aes(x = model, y = auc))+
    geom_boxplot() +
    geom_jitter(width = 0.05, alpha = 0.25) +
    labs(
      x = "Models",
      y = "AUC",
      title = "Cross validation comparison of models"
    ) + 
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0)
    )
}
