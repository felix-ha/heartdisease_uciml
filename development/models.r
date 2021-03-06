library(tidyverse)
library(precrec)
library(MLmetrics)
library(caret)
library(rpart)
library(magrittr)
theme_update(plot.title = element_text(hjust = 0.5))
source("utils.r")
source("pre_processing.r")
library(recipes)
library(class)


heart <- get_opt_df()
heart_dummies <- get_df_dummies(heart)

# do not use test set while model optimization!
# df_raw <- read_csv("data.csv")
# heart <- get_df(df_raw)
# heart_trainig <- get_training_df(heart)
# heart_test <- get_test_df(heart)

set.seed(25)
number_of_folds <- 10
folds <- createFolds(heart$target, k = number_of_folds)

models <- c("log_reg", "log_best_subset", "tree", "knn", "knn_preprocessed") 
number_of_models <- length(models)


result <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(result) <- c("model", "auc")
result <- as_tibble(result)

for(model in models){
  for(fold_index in c(1:number_of_folds)){
    training <- heart[-folds[[fold_index]],]
    test <- heart[folds[[fold_index]],]
    
    y_true <- ifelse(test$target == "no_disease", 0, 1)
    
    if (model == "log_reg") {
      fit <- glm(target ~., data=training, family =binomial(link = "logit"))
      y_probabilities <- unname(predict(fit, test,  type="response"))
    }
    if (model == "tree"){
      fit <- rpart(target ~., data=training, method = "class")
      # using ratio of poisitive labels as probability
      y_probabilities <- unname(predict(fit, test)[,2])
    }
    if (model == "log_best_subset"){
      fit <- glm(target ~ oldpeak + cp + ca + thal + thalach + slope, data=training, family =binomial(link = "logit"))
      y_probabilities <- unname(predict(fit, test,  type="response"))
    }
    if(model == "knn"){
      training <- heart_dummies[-folds[[fold_index]],]
      test <- heart_dummies[folds[[fold_index]],]

      train_points <- training %>%
        select(-target_disease)
      
      test_points <- test %>%
        select(-target_disease)
      
      train_labels <-  unlist(training %>%
                                select(target_disease))
      
      test_labels <-  unlist(test %>%
                               select(target_disease))
      
      fit <- knn(train_points, test_points, train_labels, k = 189, prob = TRUE);
      y_probabilities <- (attributes(fit)$prob)
    }
    if(model == "knn_preprocessed"){
      training <- heart_dummies[-folds[[fold_index]],]
      test <- heart_dummies[folds[[fold_index]],]
      
      processed_df <- process_unit(training, test)
      training <- processed_df[[1]]
      test <- processed_df[[2]]
      
      train_points <- training %>%
        select(-target_disease)
      
      test_points <- test %>%
        select(-target_disease)
      
      train_labels <-  unlist(training %>%
                                select(target_disease))
      
      test_labels <-  unlist(test %>%
                               select(target_disease))
      
      fit <- knn(train_points, test_points, train_labels, k = 208, prob = TRUE);
      y_probabilities <- (attributes(fit)$prob)
    }
  
    auc <- AUC(y_true = y_true, y_pred = y_probabilities)
    result %<>%
      add_row(model = model, auc = auc)
  }
}


ggplot(data = result, mapping = aes(y = auc, x = model)) +
  geom_boxplot() + 
  expand_limits(y = 0)

result %<>%
  group_by(model) %>%
  summarize(Median = median(auc),
            AVG = mean(auc)) %>%
  arrange(desc(Median))
print(result)
