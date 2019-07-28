source("utils.r")
library(caret)
library(recipes)
library(class)
library(MLmetrics)
library(magrittr)

# finding best k for kNN with forward selection

max_k <- 215
step <- 1

df_raw <- read_csv("data.csv")
heart <- get_df(df_raw)
heart <- get_training_df(heart)
heart_dummies <- get_df_dummies(heart)


set.seed(25)
number_of_folds <- 10
folds <- createFolds(heart$target, k = number_of_folds)

result <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(result) <- c("k", "auc")
result <- as_tibble(result)

for(k in seq(1, max_k, by = step)){
  aucs <- vector("numeric", length = number_of_folds)
for(fold_index in c(1:number_of_folds)){
  training <- heart_dummies[-folds[[fold_index]],]
  test <- heart_dummies[folds[[fold_index]],]

  train_points <- training %>%
            select(-target_disease)
  
  test_points <- test %>%
    select(-target_disease)
  
  train_labels <-  unlist(training %>%
    select(target_disease))
  
  y_true <-  unlist(test %>%
                            select(target_disease))

  fit <- knn(train_points, test_points, train_labels, k = k, prob = TRUE);
  y_probabilities <- (attributes(fit)$prob)
  aucs[fold_index] <-  AUC(y_true = y_true, y_pred = y_probabilities)

}
  
  result %<>%
    add_row(k = as.character(k), auc = aucs)
}

ggplot(data = result, mapping = aes(y = auc, x = k)) +
  geom_boxplot(group=k) + 
  expand_limits(y = 0)

summary <- result %>%
  group_by(k) %>%
  summarize(AVG = mean(auc),
            Median = median(auc))

best_model <-  as.character(summary[summary$Median == max(summary$Median),1])
print(paste("best k:", best_model))
