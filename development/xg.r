source("helpers.r")
library(xgboost)
library(Matrix)

df <-  get_training_df_clean()

df_matrix <- sparse.model.matrix(target ~ .-1, data = df)


dtrain <- xgb.DMatrix(df_matrix, label = ifelse(df$target == "no_disease", 0, 1))

param <- list(max_depth = 2, eta = 0.3, verbose = 0, nthread = 2,
              objective = "binary:logistic")

bst <- xgb.train(param, dtrain, nrounds = 5)


y_probabilities <- predict(bst, dtrain)

AUC(y_true = ifelse(df$target == "no_disease", 0, 1),
    y_pred = y_probabilities)

