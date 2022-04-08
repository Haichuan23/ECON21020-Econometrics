# Define a custom function that returns draws from a Bernoulli rv

my_rbernoulli <- function(n, p) { 
  x <- runif(n, min = 0, max = 1)
  # Return draws
  return(x)
}#MY_RBERNOULLI

# Test the custom Bernoulli generator function
x <- my_rbernoulli(10000, 0.5)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 0.5
