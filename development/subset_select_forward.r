library(tidyverse)
library(caret)
library(MLmetrics)
library(magrittr)
source("utils.r")

# forward selection: selects "number_of_variables_to_select" and chooses the best model with
# the avg auc (does not stop if the next model has a smaller auc)

df_raw <- read_csv("./data.csv")
heart <- get_df(df_raw)
heart <- get_training_df(heart)


set.seed(25)
number_of_folds <- 10
folds <- createFolds(heart$target, k = number_of_folds)

number_of_variables_to_select <- 13
variables_left <- names(heart)
variables_left <- variables_left[variables_left != "target"]
number_of_variables_initial <- length(variables_left)


result <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(result) <- c("model", "auc")
result <- as_tibble(result)

formula_previous <- paste("target ~")
for(i in c(1:number_of_variables_to_select)) {
  auc_avg <- vector("numeric", length = length(number_of_variables_to_select))
  auc_avg <- 0
  
 
  for(var in variables_left){
    aucs <- vector("numeric", length = number_of_folds)
    
    if(length(variables_left) == number_of_variables_initial){
      formula_current <-  paste(formula_previous, var)
}
    else
      formula_current <-  paste(formula_previous, "+", var)


    for(fold_index in c(1:number_of_folds)){
      training <- heart[-folds[[fold_index]],]
      test <- heart[folds[[fold_index]],]
      
      y_true <- ifelse(test$target == "no_disease", 0, 1)
      
      fit <- glm(formula_current, data=training, family =binomial(link = "logit"))
      y_probabilities <- unname(predict(fit, test,  type="response"))
      aucs[fold_index] <-  AUC(y_true = y_true, y_pred = y_probabilities)
  
    }
    
    auc_avg_current <- mean(aucs)

    if(auc_avg < auc_avg_current){
      aucs_selected <- aucs
      formula_selected <- formula_current
      variable_selected <- var
      
      auc_avg <- auc_avg_current
    }

  }

  result %<>%
    add_row(model = formula_selected, auc = aucs_selected)
  
  formula_previous <- formula_selected
  variables_left <- variables_left[variables_left != variable_selected]
}

ggplot(data = result, mapping = aes(y = auc, x = model)) +
  geom_boxplot() + 
  expand_limits(y = 0) +
   scale_x_discrete(labels = c(1:number_of_variables_to_select))

summary <- result %>%
  group_by(model) %>%
  summarize(AVG = mean(auc),
            Median = median(auc))

best_model <-  as.character(summary[summary$AVG == max(summary$AVG),1])
