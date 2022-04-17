df <- read.csv("/Users/haichuan/ECON21020:Econometrics/ak91.csv")

# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

sum <- 0
count_college <- 0
for (i in 1:length(yrs_educ)){
  if (yrs_educ[i] == 16){
    sum= sum+wkly_wage[i]
    count_college = count_college+1
  }
}
mucollege <- sum / count_college
mucollege

