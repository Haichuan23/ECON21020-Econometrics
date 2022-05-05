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

# Part c: 
x <- 16
blp_est <- alpha_hat + beta_hat * x
blp_est

# Part d:
epsilon <- wkly_wage - blp_est
se_numer <- sqrt(mean(epsilon^2 * (yrs_educ-mean(yrs_educ))^2))
se <- (se_numer / var(yrs_educ))/sqrt(n)
se

# Part e:
# test statistics
beta_null <- 31
test <- (beta-beta_null)/se
test

# Part f:
p_value <- 2*pnorm(-abs(test))
p_value


