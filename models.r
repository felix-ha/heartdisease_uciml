library(tidyverse)
library(precrec)
library(MLmetrics)
source("utils.r")

df_raw <- read_csv("data.csv")
heart <- get_df(df_raw)

fit=glm(target ∼.,
             data=heart ,family =binomial(link = "logit"))
summary(fit)


fit$fitted.values

pred <- ifelse(fit$fitted.values < 0.5, "no_disease", "disease")

accuracy <- Accuracy(y_true = heart$target, y_pred = pred)
precision <- Precision(y_true = heart$target, y_pred = pred, positive = "disease")
recall <- Recall(y_true = heart$target, y_pred = pred, positive = "disease")
f_1 <- F1_Score(y_true = heart$target, y_pred = pred, positive = "disease")
auc <- AUC(y_true = ifelse(heart$target == "no_disease", 0, 1)
           , y_pred = fit$fitted.values)
confusion_matrix <- ConfusionMatrix(y_true = ifelse(heart$target == "no_disease", 0, 1)
                                    , y_pred = fit$fitted.values)



print(paste("Accuracy  : ", round(accuracy, 3)))
print(paste("Precision : ", round(precision, 3)))
print(paste("Recall    : ", round(recall, 3)))
print(paste("F1        : ", round(f_1, 3)))
print(paste("AUC       : ", round(auc, 3)))

precrec_obj <- evalmod(scores = fit$fitted.values, labels = ifelse(heart$target == "no_disease", 0, 1))
autoplot(precrec_obj)

# print("Confusion Matrix    : ")
# print(confusion_matrix)

      