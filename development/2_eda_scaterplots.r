library(tidyverse)
theme_update(plot.title = element_text(hjust = 0.5))
source("utils.r")

df_raw <- read_csv("data.csv")
heart <- get_df(df_raw)

ggplot(data = heart) +
  geom_point(mapping = aes(x = age, y = chol, color = target), size = 2) + 
  scale_color_manual(values=c("blue", "red"))
