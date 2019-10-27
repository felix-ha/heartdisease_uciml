rm(list = ls())

unit_vector <- function(dim, index) {
  result <- vector(mode = "numeric", length = dim)
  result[index] = 1
  return(result)
}

gradient <- function(f, x) {
  h <- 0.00001
  dim <- length(x)
  result <- vector(mode = "numeric", length = dim)
  
  for(i in c(1:dim)){
    result[i] = (f(x + unit_vector(dim, i) * h) -  f(x - unit_vector(dim, i) * h)) / (2 * h) 
  }

  return(result)
}

const <- 2
f <- function(x) x[1]^2 + x[2]^2 + const

x_0 <- c(5,1)
epsilon_stop <- 1e-2

x_current  <- x_0
alpha <- 0.1
epsilon <- norm(gradient(f, x_current), type = "2")

while(epsilon > epsilon_stop) {
  x_next <- x_current - alpha * gradient(f, x_current)
  
  x_current <- x_next
  epsilon <- norm(gradient(f, x_current), type = "2") 
}

print(epsilon)
print((x_current))
print(f(x_current))


      