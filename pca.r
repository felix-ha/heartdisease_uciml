rm(list = ls())
library(tidyverse)

data(iris)
head(iris, 3)

iris <- iris[sample(nrow(iris)),]

log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 

print(ir.pca)

pred_matrix <- predict(ir.pca, 
        newdata=log.ir)

df <- tibble(pc1 = pred_matrix[,1],
             pc2 = pred_matrix[,2],
             target = iris[, 5])

ggplot(df, aes(x = pc1, y = pc2, color = target))+
  geom_point()
