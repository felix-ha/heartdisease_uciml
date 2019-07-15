library(tidyverse)
theme_update(plot.title = element_text(hjust = 0.5))
source("utils.r")

df_raw <- read_csv("data.csv")
heart <- get_df(df_raw)

# target ------------------------------------------------------------------

heart %>%
  count(target)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = target)) +
  ggtitle("histogram of target")

# sex ---------------------------------------------------------------------

heart %>%
  count(sex)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = sex)) +
  ggtitle("histogram of sex")

# exang - exercise induced angina -------------------------------------------------------------------

heart %>%
  count(exang)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = exang)) +
  ggtitle("histogram of exercise induced angina")

# cp - chest pain type ----------------------------------------------------------------------

heart %>%
  count(cp)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = cp)) +
  ggtitle("histogram of chest pain type")

# slope - the slope of the peak exercise ST segment -------------------------------------------------------------------

heart %>%
  count(slope)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = slope)) +
  ggtitle("histogram of the slope of the peak exercise ST segment")

# thal --------------------------------------------------------------------

heart %>%
  count(thal)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = thal)) +
  ggtitle("histogram of thal")


# fbs - fasting blood sugar > 120 mg/dl ---------------------------------------------------------------------

heart %>%
  count(fbs)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = fbs)) +
  ggtitle("histogram of fasting blood sugar > 120 mg/dl")


# restecg - resting electrocardiographic results --------------------------

heart %>%
  count(restecg)

ggplot(data = heart) +
  geom_bar(mapping = aes(x = restecg)) +
  ggtitle("histogram of resting electrocardiographic results")


# chol - serum cholestoral in mg/dl --------------------------------------------------------------------

heart %>%
  count(cut_width(chol, 30))

m <- round(mean(heart$chol),1)
sd <- round(sd(heart$chol),1)

ggplot(data = heart) +
  geom_histogram(mapping = aes(x = chol), bins = 12) +
  ggtitle(paste("histogram of serum cholestoral in mg/dl, m=", m, "sd=", sd))

# thalach - maximum heart rate achieved-----------------------------------------------------------------

heart %>%
  count(cut_width(thalach, 15))

m <- round(mean(heart$thalach),1)
sd <- round(sd(heart$thalach),1)

ggplot(data = heart) +
  geom_histogram(mapping = aes(x = thalach), bins = 9) +
  ggtitle(paste("histogram of maximum heart rate achieved, m=", m, "sd=", sd))

# trestbps - resting blood pressure (in mm Hg on admission to the hospital) ----------------------------------------------------------------

heart %>%
  count(cut_width(trestbps, 15))

m <- round(mean(heart$trestbps),1)
sd <- round(sd(heart$trestbps),1)

ggplot(data = heart) +
  geom_histogram(mapping = aes(x = trestbps), bins = 8) +
  ggtitle(paste("histogram of resting blood pressurein mm/Hg, m=", m, "sd=", sd))


# oldpeak - ST depression induced by exercise relative to rest -----------------------------------------------------------------

heart %>%
  count(cut_width(oldpeak, .5))

m <- round(mean(heart$trestbps),1)
sd <- round(sd(heart$trestbps),1)

ggplot(data = heart) +
  geom_histogram(mapping = aes(x = trestbps), bins = 12) +
  ggtitle(paste("histogram of resting blood pressurein mm/Hg, m=", m, "sd=", sd))

# ca - number of major vessels (0-3) colored by flourosopy ----------------

heart %>%
  count(ca)

m <- round(mean(heart$ca),1)
sd <- round(sd(heart$ca),1)

ggplot(data = heart) +
  geom_histogram(mapping = aes(x = ca), bins = 5) +
  ggtitle(paste("histogram of number of major vessels (0-3) colored by flourosopy\nm=", m, "sd=", sd))
