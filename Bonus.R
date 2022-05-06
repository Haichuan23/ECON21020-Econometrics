#Bonus
df <- read.csv("/Users/haichuan/ECON21020:Econometrics/ak91.csv")
n <- nrow(df)
# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE
n <- 329509

# Part a: Give OLS estimators for the BLP coefficient
beta_hat <- cov(yrs_educ, wkly_wage)/ var(yrs_educ)
beta_hat
alpha_hat <- mean(wkly_wage)-mean(yrs_educ) * beta
alpha_hat


#Part a:
# Define a custom function to compute the ols estimators
my_simplecoef <- function(y,x){
  #Compute and return estimates for alpha and beta
  beta <- cov(x, y)/ var(x)
  alpha <- mean(y) - mean(x)*beta
  res <- c(alpha, beta)
  return(res)
}#My_SIMPLECOEF

# Test the function using your solutions to Problem 6
coef <- my_simplecoef(wkly_wage, yrs_educ)
coef 

#Part b) Define a custom function to calculate BLP estimates
my_simpleblp <- function(coef, x){
  alpha <- coef[1]
  beta <- coef[2]
  y <- alpha + beta * x
  return(y)
}
mean(wkly_wage) - mean(my_simpleblp(coef, yrs_educ))

#Part c
# define a custom function to compute the standard error
my_simplese <- function(coef, y, x){
  alpha <- coef[1]
  beta <- coef[2]
  blp <- alpha + beta * x
  epsilon <- y - blp
  se_numer <- sqrt(mean(epsilon^2 * (x-mean(x))^2))
  n <- length(x)
  se <- (se_numer / var(x))/sqrt(n)
  return(se)
}



