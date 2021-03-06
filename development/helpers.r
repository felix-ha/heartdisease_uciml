library(tidyverse)
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

get_training_df <- function(p = 0.8) {
  set.seed(25)
  df_raw <- read_csv("data.csv")
  heart <- get_df(df_raw)
  inTraining <- createDataPartition(heart$target, p = p, list = FALSE)
  training <- heart[ inTraining,]
  # testing  <- df[-inTraining,]
  return(training)
}

odds_ratio  <- function(df, variable) {
    cross_class_table  <- table(df$target, df[[variable]])
    or  <- (cross_class_table[2,2] / cross_class_table[1,2]) / (cross_class_table[2,1] / cross_class_table[1,1])
}
