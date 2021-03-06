library(tidyverse)
library(recipes)
library(caret)

# create clean data set: e. g. sex is a numeric vector, needs to be a factor
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

get_df_dummies <- function(df){
  result <- df %>% 
    recipe(target ~ .) %>% 
    step_dummy(restecg, fbs, sex, exang, cp, slope, thal, target) %>% 
    prep(training = df) %>% 
    bake(new_data = df)
}

get_training_df <- function(df) {
  set.seed(25)
  inTraining <- createDataPartition(df$target, p = .80, list = FALSE)
  training <- df[ inTraining,]
  # testing  <- df[-inTraining,]
  return(training)
}

get_test_df <- function(df) {
  set.seed(25)
  inTraining <- createDataPartition(df$target, p = .80, list = FALSE)
  # training <- df[ inTraining,]
  testing  <- df[-inTraining,]
  return(testing)
}

get_varibale_description <- function(variable) {
  if (variable == "cp") {
    return("chest pain type
-- Value 1: typical angina
-- Value 2: atypical angina
-- Value 3: non-anginal pain
-- Value 4: asymptomatic ")
  }
  if (variable == "trestbps") {
    return(" resting blood pressure (in mm Hg on admission to the hospital) ")
  }
  if (variable == "fbs") {
    return("(fasting blood sugar > 120 mg/dl) (1 = true; 0 = false) ")
  }
  if (variable == "restecg") {
    return("resting electrocardiographic results
-- Value 0: normal
-- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
-- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria ")
  }
  if (variable == "thalach") {
    return("maximum heart rate achieved")
  }
  if (variable == "exang") {
    return("exercise induced angina (1 = yes; 0 = no)")
  }
  if (variable == "oldpeak") {
    return(" ST depression induced by exercise relative to rest")
  }
  if (variable == "slope") {
    return("the slope of the peak exercise ST segment
-- Value 1: upsloping
-- Value 2: flat
-- Value 3: downsloping ")
  }
  if (variable == "ca") {
    return("number of major vessels (0-3) colored by flourosopy ")
  }
  if (variable == "thal") {
    return("3 = normal; 6 = fixed defect; 7 = reversable defect ")
  }
  if (variable == "target") {
    return("1 = disease; 0 = no disease")
  }
  if (variable == "age") {
    return("age in years ")
  }
  if (variable == "sex") {
    return("sex (1 = male; 0 = female) ")
  }
  if (variable == "chol") {
    return("serum cholestoral in mg/dl")
  } else {
    return("not set")
  }
  
}

visualize_metrics <- function(df, fit, threshold) {
  y_probabilities <- unname(predict(fit, df,  type="response"))
  y_predicted <- ifelse(y_probabilities > threshold, 1, 0)
  y_true <- ifelse(df$target == "no_disease", 0, 1)
  
  accuracy <- Accuracy(y_true = y_true, y_pred = y_predicted)
  precision <- Precision(y_true = y_true, y_pred = y_predicted, positive = "1")
  recall <- Recall(y_true = y_true, y_pred = y_predicted, positive = "1")
  f_1 <- F1_Score(y_true = y_true, y_pred = y_predicted, positive = "1")
  confusion_matrix <- ConfusionMatrix(y_true = y_true, y_pred = y_predicted)
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  
  print(paste("Metrics for fit with threshold", threshold))
  print("Confusion Matrix    : ")
  print(confusion_matrix)
  print(paste("Accuracy  : ", round(accuracy, 3)))
  print(paste("Precision : ", round(precision, 3)))
  print(paste("Recall    : ", round(recall, 3)))
  print(paste("F1        : ", round(f_1, 3)))
  print(paste("AUC       : ", round(auc, 3)))
  
  precrec_obj <- evalmod(scores = y_probabilities, labels = y_true)
  autoplot(precrec_obj)
}

get_opt_df <- function() {
  df_raw <- read_csv("data.csv")
  heart <- get_df(df_raw)
  heart <- get_training_df(heart)
}
