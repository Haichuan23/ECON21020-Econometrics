df <- read.csv("/Users/haichuan/ECON21020:Econometrics/ak91.csv")
# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

sum <- 0 
se_sum <- 0
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
    se_sum = se_sum+ (wkly_wage[i]-mucollege)^2 
  }
}

se_college = se_sum/(count_college-1) #calculate the variance
se_college
