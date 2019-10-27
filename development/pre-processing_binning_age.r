rm(list = ls())
source("helpers.r")
library(tidyverse)

df <- get_training_df()

age_groups <- c("29-40", "41-45", "46-50", "51-60", "61-70", "71-77")

df %<>%
  mutate(
    disease = ifelse(target == "no_disease", 0, 1),
    age_group = case_when(
      age <= 40 ~ age_groups[1],
      (age > 40) & (age <= 45) ~ age_groups[2],
      (age > 45) & (age <= 50) ~ age_groups[3],
      (age > 50) & (age <= 60) ~ age_groups[4],
      (age > 60) & (age <= 70) ~ age_groups[5],
      age > 70 ~ age_groups[6]
    )
  ) %>% 
  mutate(age_group= factor(age_group, levels = age_groups))


df_age_groups <- df %>%
  group_by(age_group) %>%
  summarize(count = n(),
            absent = count - sum(disease),
            present = sum(disease),
            proportion = present / count)

print(df_age_groups)

ggplot(df, aes(x = age, y = disease)) +
  geom_point(position = position_jitter(w = 0.5, h = 0), shape = 1, alpha = 0.5) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p <- ggplot(df_age_groups, aes(x = age_group, y = proportion)) +
  geom_point(size = 5) +
  theme_bw() +
  labs(title = "Attempt to bin the continuous predictor age") 

print(p)

#binning with a regression tree
library(rpart)
library(rpart.plot)

fit <- rpart(target ~ age, 
             data=df, 
             method = "class",
             control = rpart.control(minbucket = 10))
rpart.plot(fit, type = 4)


splits <- (fit$splits)[,4]
splits



age_groups <- c("29-54", "55-63", "64-77")

df %<>%
  mutate(
    disease = ifelse(target == "no_disease", 0, 1),
    age_group = case_when(
      age <= 54 ~ age_groups[1],
      (age > 54) & (age <= 63) ~ age_groups[2],
      age > 63 ~ age_groups[3]
    )
  ) %>% 
  mutate(age_group= factor(age_group, levels = age_groups))


df_age_groups <- df %>%
  group_by(age_group) %>%
  summarize(count = n(),
            absent = count - sum(disease),
            present = sum(disease),
            proportion = present / count)

print(df_age_groups)

p <- ggplot(df_age_groups, aes(x = age_group, y = proportion)) +
  geom_point(size = 5) +
  theme_bw() +
  labs(title = "Attempt to bin again, with the help of an regression tree",
       subtitle = "tree does not have monotic segments...") 

print(p)
