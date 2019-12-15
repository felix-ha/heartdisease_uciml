source("helpers.r")
library(magrittr)
library(rpart)
library(rpart.plot)

df <- get_training_df_clean()

fit <- rpart(target ~., data=df, method = "class",
             control = rpart.control(maxdepth = 3))
# using ratio of poisitive labels as probability
y_probabilities <- predict(fit, df)[,2]



fit <- lm(thalach ~ chol, df)


fit_tree <- rpart(thalach ~ chol, df, 
                  control = rpart.control(maxdepth = 3))
summary(fit_tree)

rpart.plot(fit_tree)

df %<>% mutate(tree = predict(fit_tree, df))

ggplot() +
  geom_point(aes(x = chol, y = thalach), df) + 
  geom_abline(slope = fit$coefficients[2],
              intercept = fit$coefficients[1],
              col = "red") + 
  geom_line(aes(x = chol, y = tree), df)

