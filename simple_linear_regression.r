source("helpers.r")
library(magrittr)

df <- get_training_df()


fit <- lm(thalach ~ age, df)
summary(fit)

beta <- fit$coefficients


ggplot(df) + 
  geom_point(aes(x = age, y = thalach)) +
  geom_abline(intercept = beta[1], slope = beta[2]) 


df %<>% add_column(residuals = fit$residuals)

ggplot(df) +
  geom_point(aes(x = age, y = residuals)) +
  geom_abline(slope = 0) 

ggplot(df) +
  geom_density(aes(x = residuals)) 
