# Load dependencies (every time)
library(ggplot2)

#Econometrics Pset1
#Question 8a Generate n = 10000 draws from a standard normal random variable 
# N(0,1) and plot the simulated data in a histogram using ggplot2.

# Generate a vector of n draws from a standard normal rv
n <- 10000
mu <- 0
sigma <- 1
x <- rnorm(n, mu, sigma)
ggplot()+aes(x)+geom_histogram(bins = 50)




