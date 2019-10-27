rm(list = ls())
library(tidyverse)
source("helpers.r")

df <- get_training_df()

df_continuous <- df %>% select(
thalach,
trestbps,
oldpeak,
chol, age)

pca <- prcomp(df_continuous, center = TRUE, scale = TRUE)

principal_components <- pca$x

variance <- pca$sdev^2
df_pca_var <- tibble(prop_variance = round(variance / sum(variance) * 100, 1),
                     pc = c(1:ncol(df_continuous)))

ggplot(df_pca_var, aes(y = prop_variance, x = pc)) +
  geom_point(size = 5) +
  geom_line()

df_pca <- tibble(PC1 = principal_components[,1], 
                 PC2 = principal_components[,2],
                 target = df$target)

ggplot(df_pca, aes(x = PC1, y = PC2, color = target)) + 
  geom_point() +
  theme_bw()

loading_scores <- pca$rotation

df_scale <- df_continuous %>% transmute(thalach = (thalach - mean(thalach)) / sd(thalach),
                                        trestbps = (trestbps - mean(trestbps)) / sd(trestbps),
                                        oldpeak = (oldpeak - mean(oldpeak)) / sd(oldpeak),
                                        chol = (chol - mean(chol)) / sd(chol),
                                        age = (age - mean(age)) / sd(age))

print(sum(loading_scores[,1]* df_scale[1,]))
print(principal_components[1,1])

pred_matrix <- predict(pca, newdata=df_continuous)
print(pred_matrix[1,1])

fit <- glm(target ~ ., data=df_pca, family =binomial(link = "logit"))
summary(fit)
