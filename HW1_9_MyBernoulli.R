# Define a custom function that returns draws from a Bernoulli rv

my_rbernoulli <- function(n, p) { 
  x <- c()
  y <- runif(n, min = 0, max = 1)
  for (i in 1:n){
    if (y[i] <= p){
      #Use the knowledge in 9(a), use 1{U<=p} to simulate X
      x <- c(x,1)
    } else if (y[i] > p){
      x <- c(x,0)
    }
  }
  
  # Return draws
  return(x)
}#MY_RBERNOULLI

# Test the custom Bernoulli generator function
x <- my_rbernoulli(10000, 0.5)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 0.5

