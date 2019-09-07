rm(list = ls())
source("helpers.r")
library(tidyverse)
library(magrittr)

df <- get_training_df()
df %<>% mutate(target = ifelse(df$target == "no_disease", 1, 2))

df_all <- df %>% 
  select(age, target) %>%
  arrange(age) %>%
  mutate(Csum = cumsum(age),
         CDF = Csum / sum(age))


df_disease <- df %>% 
  filter(target == 2) %>%
  select(age, target) %>%
  arrange(age) %>%
  mutate(Csum = cumsum(age),
         CDF = Csum / sum(age))



df_all_perfect <- df %>% 
  select(target) %>%
  arrange(target) %>%
  mutate(Csum = cumsum(target),
         CDF = Csum / sum(target))


df_disease_perfect <- df %>% 
  filter(target == 2) %>%
  select(target) %>%
  arrange(target) %>%
  mutate(Csum = cumsum(target),
         CDF = Csum / sum(target))




F_D <- function(x){
temp <- df_disease %>%
  filter(Csum <= x)
i <- nrow(temp)
return(df_disease$CDF[i])
}

F_inverse <- function(u){
  temp <- df_all %>%
    filter(CDF <= u)
  i <- nrow(temp)
  return(df_all$Csum[i])
}

CAP <- function(u){
  return(F_D(F_inverse(u)))
}






F_D_perfect <- function(x){
  temp <- df_disease_perfect %>%
    filter(Csum <= x)
  i <- nrow(temp)
  return(df_disease$CDF[i])
}

F_inverse_perfect <- function(u){
  temp <- df_all_perfect %>%
    filter(CDF <= u)
  i <- nrow(temp)
  return(df_all$Csum[i])
}

CAP_perfect <- function(u){
  return(F_D_perfect(F_inverse_perfect(u)))
}


df_cap <-  tibble(u = seq(0.01, 1, by=0.01),
                  cap = map_dbl(u, CAP),
                  cap_perfect = map_dbl(u, CAP_perfect))

ggplot(df_cap) +
  geom_line(aes(u, cap)) + 
  geom_line(aes(u, u), linetype = 2) + 
  geom_line(aes(u, cap_perfect), linetype = 2) + 
  labs(x = "F", y = "F_disease")
  