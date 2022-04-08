# Define a custom function that returns draws from a Binomial rv

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


my_rbinomial <- function(n, p, m) {
  x <- c()
  for (i in 1:n){
    d <- 0
    for (j in 1:m){
      y <- my_rbernoulli(m,p)
      if(y[j] == 1){
        d = d+1
      }
    }
    x <- c(x, d)
  }
  return (x)
}#MY_RBINOMIAL

# Test the custom Binomial generator function
x <- my_rbinomial(10000, 0.5, 10)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 5