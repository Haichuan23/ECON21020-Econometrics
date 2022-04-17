df <- read.csv("/Users/haichuan/ECON21020:Econometrics/ak91.csv")
# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE


count <- 0
for (val in yrs_educ){
  if (val == 16) count = count + 1
}

n <- 329509
p_x = count/n # probability of being a college grad

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

var = var/(count_college-1) #calculate the variance
se_college = sqrt(var/p_x)/sqrt(count_college)


error <- qnorm(0.975) * se_college
mucollege - error
mucollege + error