#Code from Problem 5 to test the new problem
df <- read.csv("/Users/haichuan/ECON21020:Econometrics/ak91.csv")
# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE


has_college_degree <- yrs_educ == 16
count <- sum(has_college_degree)


count <- 0
for (val in yrs_educ){
  if (val == 16) count = count + 1
}

n <- length(yrs_educ)
p_x = count/n # probability of being a college grad

mu_college <- mean(wkly_wage[has_college_degree])

sum <- 0 
var <- 0
count_college <- 0
for (i in 1:length(yrs_educ)){
  if (yrs_educ[i] == 16){
    sum= sum+wkly_wage[i]
    count_college = count_college+1
  }
}
mucollege <- sum / count_college # mean wage of college graduates


for (i in 1:length(yrs_educ)){
  if(yrs_educ[i]==16){
    var = var + (wkly_wage[i]-mucollege)^2 
  }
}

var = var/(count_college) # calculate the variance
se_college = sqrt(var/p_x)/sqrt(n)


# Problem 6, part a, return a confidence interval

# Define a custom function that returns a two-sided confidence interval
my_confint <- function(mu_hat, se, alpha){
  # Compute and return the confidence interval
  wing <- qnorm(1-alpha/2) * se
  left <- mu_hat - wing
  right <- mu_hat + wing
  confint <- c(left, right)
  return (confint)
}

# Problem 6, part b 

# Define a custom function that returns True If mu_0 is not in confint
my_testrejects <- function(confint, mu_0){
  left_end <- confint[1]
  right_end <- confint[2]
  
  #Check whether mu_0 is in confint
  is_in_confint <- (mu_0 >= left_end) && (mu_0 <= right_end)
  
  #If mu_0 is in confit, dont reject. Else, reject.
  is_rejected <- !is_in_confint
  return (is_rejected)
}

#Check whether the test rejects on 1% significance level
confint_01 <- my_confint(mu_college, se_college, 0.01)
my_testrejects(confint_01, 600)

# Check whether the test rejects on 10% significance level
confint_10 <- my_confint(mu_college, se_college, 0.1)
my_testrejects(confint_10, 600)


# Problem c from extra credit
# Define a custom function for a two-sided test
my_twosidedtest <- function(mu_hat, se, alpha, mu_0){
  # Compute the confidence interval with siginificance level alpha
  confint <- my_confint(mu_hat, se, alpha)
  is_rejected <- my_testrejects(confint, mu_0)
  
  # Construct test message
  if (is_rejected){
    message <-paste("The test of H0: mu =", mu_0, "against H1: mu !=", mu_0,
                    "rejects at a", alpha, "significance level")
  }else{
    message <-paste("The test of H0: mu =", mu_0, "against H1: mu !=", mu_0,
                    "fails to rejects at a", alpha, "significance level")
  }
  #Print the message
  print(message)
}#MY_TWOSIDEDTEST

#Check whether the test rejects on 1\% significance level
my_twosidedtest(mu_college, se_college, 0.01, 600) # Should not reject

#Check whether the test rejects on 10\% siginificance level
my_twosidedtest(mu_college, se_college, 0.1, 600) # Should not reject





