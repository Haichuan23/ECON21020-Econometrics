df <- read.csv("/Users/haichuan/ECON21020:Econometrics/ak91.csv")

# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE


count <- 0
for (val in yrs_educ){
  if (val == 16) count = count + 1
}

n <- 329509
result = count/n
