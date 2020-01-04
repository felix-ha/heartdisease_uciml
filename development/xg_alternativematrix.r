source("helpers.r")
library(xgboost)


# variable f is character
df <- tibble(x = c(1, 2, 3.4, 5, 100),
             f = c("a", "a", "b", "b", "C"),
             y = c(-1, -1.1, 40, 44, -12))

# convert f to character
df <- df %>%
  mutate(f = factor(f, levels = c("a", "b", "C")))

# create a model matrix (-1 to drop intercept) -> one hot encoding
df_model_matrix <- model.matrix(y ~ .-1, df)


dtrain <- xgb.DMatrix(df_model_matrix, label = df$y)

param <- list(max_depth = 2, eta = 0.3, verbose = 0, nthread = 2)

bst <- xgb.train(param, dtrain, nrounds = 50)

y_pred <- predict(bst, dtrain)




# #false endocing from kaggle
# 
# df <-  get_training_df_clean()
# 
# dtrain <- df %>%
#   select(-target) %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate_if(is.factor, as.numeric) %>%
#   select_if(is.numeric) %>%
#   as.matrix()
# 
# dtrain <- xgb.DMatrix(label = ifelse(df$target == "no_disease", 0, 1),
#                       data = dtrain)
# 
# param <- list(max_depth = 2, eta = 0.3, verbose = 0, nthread = 2,
#               objective = "binary:logistic")
# 
# bst <- xgb.train(param, dtrain, nrounds = 5)
# 
# 
# y_probabilities <- predict(bst, dtrain)
# 
# AUC(y_true = ifelse(df$target == "no_disease", 0, 1),
#     y_pred = y_probabilities)


