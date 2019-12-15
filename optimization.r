
# Optimization for a function f: R^N -> R ---------------------------------

library(tidyverse)

f <- function(x) x[1]^2 + x[2]^2 


h_eps <- sqrt(.Machine$double.eps)

get_unit_vector <- function(N,i) {
  result <- rep(0, N)
  result[i] <- 1
  return(result)
}

gradient <- function(f, x, h = h_eps) {
  N <- length(x)
  
  result <- map_dbl(1:N, function(i) {
    x_delta <- get_unit_vector(N,i) * h
    dx <- (f(x + x_delta) - f(x - x_delta)) / (2 * h)
    return(dx)
  })
}





x_0 <- c(4, -10)
alpha <- 0.5

max_iter <- 1e3
eps <- 1e-3


x_current <- x_0
iteration <- 0

grad <- gradient(f, x_current)
gradient_norm <- norm(grad, "2")

while(gradient_norm > eps ){

  x_next <- x_current - alpha * grad
  grad <- gradient(f, x_next)
  gradient_norm <- norm(grad, "2")
  
  x_current <- x_next
  
  if(iteration > max_iter)
    break
  
  iteration <- iteration + 1
}





print(iteration)
print(x_current)
print(f(x_current))

