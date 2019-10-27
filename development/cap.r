rm(list = ls())
source("helpers.r")
library(tidyverse)
library(magrittr)


predictor <- "age"

df <- get_training_df()
df %<>% mutate(target = ifelse(df$target == "no_disease", 0, 1)) 

values_all <- sort(df[[predictor]])
values_disease <- sort((df %>% filter(target == 1))[[predictor]])

p <- length(values_disease) / length(values_all)

cdf_all <- cumsum(values_all) / sum(values_all)
cdf_disease <- cumsum(values_disease) / sum(values_disease)


df_cap <- tibble(u = vector("numeric"),
                 cap = vector("numeric"),
                 cap_perfect = vector("numeric"))

for(u in seq(0, 1, by=(1-0.1) / 10)){
  tmp <- cdf_all[cdf_all <= u]
  s <- tmp[length(tmp)]
  s <- values_all[which(cdf_all == s)]
  
  #F_D(F^-1(u)) = F_D(s)
  cap <- values_disease[values_disease <= s]
  cap <- cdf_disease[length(cap)]
  
  if(length(cap) == 0) cap <- 0
  if(u > p) cap_perfect <- 1
  else cap_perfect <- u * (1/p)
  df_cap %<>% add_row(u = u, cap = cap, cap_perfect = cap_perfect)
}

ggplot(df_cap) + 
  geom_line(aes(x = u, y = cap)) + 
  geom_line(aes(x = u, y = cap_perfect), color = "red") +
  geom_segment(aes(x = 0, xend = 1 , y = 0, yend = 1), linetype = 2) +
  labs(
    x = "u",
    y = "CAP(u)",
    title = paste("Cumulative Accuracy Profile of", predictor)
  ) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0)
  )
