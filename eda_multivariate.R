library(tidyverse)
theme_update(plot.title = element_text(hjust = 0.5))
source("utils.r")

df_raw <- read_csv("data.csv")
heart <- get_df(df_raw)


# box plots: categorial and continuos variable ----------------------------------------------------------------
# ggplot(data = heart, mapping = aes(x = chol, y = ..density..)) +
#   geom_freqpoly(mapping = aes(color = sex), binwidth = 10)

ggplot(data = heart, mapping = aes(x = sex, y = chol)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = fbs, y = chol)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = restecg, y = chol)) +
  geom_boxplot()

#thalach - maximum heart rate achieved
ggplot(data = heart, mapping = aes(x = fbs, y = thalach)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = sex, y = thalach)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = restecg, y = thalach)) +
  geom_boxplot()

#resting blood pressure (in mm Hg on admission to the hospital)
ggplot(data = heart, mapping = aes(x = fbs, y = trestbps)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = sex, y = trestbps)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = restecg, y = trestbps)) +
  geom_boxplot()

# target
ggplot(data = heart, mapping = aes(x = target, y = age)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = target, y = chol)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = target, y = thalach)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = target, y = trestbps)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = target, y = oldpeak)) +
  geom_boxplot()

ggplot(data = heart, mapping = aes(x = target, y = ca)) +
  geom_boxplot()

# two categorial ----------------------------------------------------------

ggplot(data = heart) +
  geom_count(mapping = aes(x = target, y = sex))
table(heart$sex, heart$target)

ggplot(data = heart) +
  geom_count(mapping = aes(x = target, y = restecg))
table(heart$restecg, heart$target)

ggplot(data = heart) +
  geom_count(mapping = aes(x = target, y = fbs))
table(heart$fbs, heart$target)

ggplot(data = heart) +
  geom_count(mapping = aes(x = target, y = exang))
table(heart$exang, heart$target)

ggplot(data = heart) +
  geom_count(mapping = aes(x = target, y = cp))
table(heart$cp, heart$target)

ggplot(data = heart) +
  geom_count(mapping = aes(x = target, y = slope))
table(heart$slope, heart$target)

ggplot(data = heart) +
  geom_count(mapping = aes(x = target, y = thal))
table(heart$thal, heart$target)
