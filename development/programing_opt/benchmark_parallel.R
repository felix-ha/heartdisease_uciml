rm(list = ls())

library(parallel)
library(microbenchmark)

N <- 510


f <- function(x) {
  for (i in 1:100) {exp(i) * sin(i)}
}

parallel <- function() {
  n_cores <- detectCores(logical = TRUE)
  cluster <- makeCluster(n_cores)
  clusterApply(cluster, x = 1:N, fun = f)
  stopCluster(cluster)
}

loop <- function(){
  for(x in 1:N) f(x)
}



result <- microbenchmark(loop(), parallel(), times = 10)

print(result)