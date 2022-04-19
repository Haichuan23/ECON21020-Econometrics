df <- read.csv("/Users/haichuan/ECON21020:Econometrics/ak91.csv")
# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

# An alternative way of writing the for loop#
#has_college_degree <- yrs_educ == 16
#count <- sum(has_college_degree)


count <- 0
for (val in yrs_educ){
  if (val == 16) count = count + 1
}

n <- length(yrs_educ)
p_x = count/n # probability of being a college grad

# AN alternative way of counting mean wages
#mu_college <- mean(wkly_wage[has_college_degree])

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

null_mean = 595
z <- (mucollege-null_mean)/se_college
p_value <- 2*pnorm(-abs(z))
p_value
