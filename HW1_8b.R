# Load dependencies (every time)
library(ggplot2)

#Econometrics Pset1
#Generate n = 10000 draws from a uniform random variable U (−1, 1) and plot the 
#simulated data in a histogram using ggplot2.


n <- 10000
min_y <- -1
max_y <- 1
y <- runif(n, min_y, max_y)
# Plot a histogram of the draws using ggplot2 # [INSERT YOUR CODE HERE]
hist(y, freq = FALSE, xlab = 'x')
