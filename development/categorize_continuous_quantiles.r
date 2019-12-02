source("helpers.r")
library(magrittr)


df <- get_training_df_clean()


df %<>% mutate(oldpeak_q = case_when( oldpeak <= quantile(oldpeak, 0.25) ~ "q1",
                                      oldpeak <= quantile(oldpeak, 0.5) ~  "q2",
                                      oldpeak <= quantile(oldpeak, 0.75) ~ "q3",
                                 TRUE ~ "q4"))

df %<>% mutate(oldpeak_q = factor(oldpeak_q, levels = c("q1", "q2", "q3", "q4"),
                              labels = c("q1", "q2", "q3", "q4")))



fit <- glm(target ~  cp + oldpeak_q + ca, 
           df, family = binomial(link = "logit"))
summary(fit)


plot(df$oldpeak, predict(loess(ifelse(df$target == "no_disease", 0, 1) ~ df$oldpeak)))

plot(df$ca, predict(loess(ifelse(df$target == "no_disease", 0, 1) ~ df$ca)))
