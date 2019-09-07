rm(list = ls())
source("helpers.r")
library(tidyverse)
library(magrittr)

df <- get_training_df()

df_all <- df %>% 
  select(age, target) %>%
  arrange(age) %>%
  mutate(Csum = cumsum(age),
         CDF = Csum / sum(age))


df_disease <- df %>% 
  filter(target == "disease") %>%
  select(age, target) %>%
  arrange(age) %>%
  mutate(Csum = cumsum(age),
         CDF = Csum / sum(age))

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

df_cap <-  tibble(u = seq(0.01, 1, by=0.01),
                  cap = map_dbl(u, CAP))

ggplot(df_cap, aes(u, cap)) +
  geom_line() + 
  labs(x = "F", y = "F_disease")
  