source("helpers.r")

result <- tibble(predictors = vector(mode = "character"),
                 mean_interaction = vector(mode = "numeric"),
                 mean_no_interaction = vector(mode = "numeric"))

df <- get_training_df_clean()


categorial <- c("cp")
continuous <- c("oldpeak", "ca")

for(cat in categorial) {
  for(con in continuous) {
    
    no_interaction <- paste0(cat, " + ", con)
    interaction <- paste0(cat, " * ", con)
    
    formula_no_interaction <- paste0("target ~ ", no_interaction)
    formula_interaction <- paste0("target ~ ", interaction)
    



learner_A <- function(training, test) {
  name <- no_interaction
  
  fit <- glm(formula_no_interaction, data=training, family =binomial(link = "logit"))
  y_probabilities <- predict(fit, test,  type="response")
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  return(list(name = name, auc = auc))
}

learner_B <- function(training, test) {
  name <- interaction
  
  fit <- glm(formula_interaction, data=training, family =binomial(link = "logit"))
  y_probabilities <- predict(fit, test,  type="response")
  y_true <- ifelse(test$target == "no_disease", 0, 1)
  
  auc <- AUC(y_true = y_true, y_pred = y_probabilities)
  return(list(name = name, auc = auc))
}



cv_result <- cross_validation(learner_A, learner_B, df)

cv_result %>% group_by(model) %>%
  summarise(mean = mean(auc))

mean_interaction <- mean(cv_result %>% 
                          filter(model == interaction) %>%
                          pull(auc))

mean_no_interaction <- mean(cv_result %>% 
                           filter(model == no_interaction) %>%
                           pull(auc))

if(mean_interaction > mean_no_interaction) {

  result <- result %>% add_row(predictors = interaction, 
                               mean_interaction = mean_interaction,
                               mean_no_interaction = mean_no_interaction)
}

  }
}

print(result %>% mutate(diff = mean_interaction - mean_no_interaction))
  