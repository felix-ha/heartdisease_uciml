library(R6)
library(caret)
library(MLmetrics)
library(tidyverse)
library(magrittr)
source('helpers.r')


logistic_regression <- R6Class(
  "Logistic Regression Model",
  private = list(
    ..formula = "",
    ..model_name = "",
    ..df = NULL,
    ..number_of_folds = NULL,
    ..folds = NULL
  ),
  
  public = list(
    
    fit = function() {
     
      result <- tibble(model = vector(mode = "character"),
                       auc = vector(mode = "numeric"))
      
      for(fold_index in c(1:private$..number_of_folds)){
        training <- private$..df[-private$..folds[[fold_index]],]
        test <- private$..df[private$..folds[[fold_index]],]
        y_true <- ifelse(test$target == "no_disease", 0, 1)
        
        fit <- glm(private$..formula, data=training, family =binomial(link = "logit"))
        y_probabilities <- predict(fit, test,  type="response")
        
        auc <- AUC(y_true = y_true, y_pred = y_probabilities)
        
        result %<>%
          add_row(model = private$..model_name, auc = auc)
      }
      
      return(result)
      
      },
    
    initialize = function(df, model_name, formula) {
      private$..model_name <- model_name
      private$..df <- df 
      private$..formula <- formula
      
      private$..number_of_folds <- 10
      private$..folds <- createFolds(private$..df$target, k = private$..number_of_folds )
    }
  )
  

)

df <- get_training_df()

log_lm <- logistic_regression$new(df, "glm", "target ~ age + chol + restecg + thalach + sex")
result <- log_lm$fit()

result %<>%
  group_by(model) %>%
  summarize(Median = median(auc),
            AVG = mean(auc)) %>%
  arrange(desc(Median))
print(result)
