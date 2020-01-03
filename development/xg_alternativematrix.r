source("helpers.r")
library(xgboost)


df <-  get_training_df_clean()

dtrain <- df %>%
  select(-target) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric) %>%
  select_if(is.numeric) %>%
  as.matrix()

dtrain <- xgb.DMatrix(label = ifelse(df$target == "no_disease", 0, 1),
                      data = dtrain)

param <- list(max_depth = 2, eta = 0.3, verbose = 0, nthread = 2,
              objective = "binary:logistic")

bst <- xgb.train(param, dtrain, nrounds = 5)


y_probabilities <- predict(bst, dtrain)

AUC(y_true = ifelse(df$target == "no_disease", 0, 1),
    y_pred = y_probabilities)


