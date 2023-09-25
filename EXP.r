# session
Sys.setlocale (, "ru_Ru")
mydata = read.csv ("A:\\School\\Альма-матер\\Курсовая\\in_anketa_5\\Test.csv")
library (gridExtra)

#filtering
mydata$cash_cost = as.numeric(mydata$cash_cost)
mydata$dep_cost = as.numeric(mydata$dep_cost)
mydata$cash_sec = as.numeric(mydata$cash_sec)
mydata$dep_sec = as.numeric(mydata$dep_sec)
mydata$dep_ease = as.numeric(mydata$dep_ease)
mydata$cash_ease = as.numeric(mydata$cash_ease)
mydata$interest_dep = as.numeric (mydata$interest_dep)

# model
res = glm ((choice) ~ interest_dep + bundling + I(dep_cost - cash_cost) + I(dep_ease-cash_ease) + I(dep_sec-cash_sec) +
+ I(-anon_cash) + I(budgeting_dep - budgeting_cash) + online_dep + I(fast_dep - fast_cash) + I(-accept_cash) +  age_1 + educ_1 + sett_typ_1 + gender_1 + inet_1 + sett + I(income_y/365), data = mydata, family = "binomial")


util_cbdc = coefficients[[1]] + coefficients[[3]] * mydata$bundling + coefficients[[4]] * mydata$cash_cost + coefficients[[5]] * mydata$dep_ease + coefficients[[6]] * mydata$dep_sec +
+ 0.5 * coefficients[[7]] * mydata$anon_cash + coefficients[[8]] * mydata$budgeting_dep + coefficients[[9]] * mydata$online_dep + coefficients[[10]] * mydata$fast_dep +
+ coefficients[[11]] * mydata$accept_cash + mydata$age_coef_1 + mydata$educ_coef_1 + mydata$sett_typ_coef_1 + mydata$gender_coef_1 + mydata$inet_coef_1 + mydata$sett_coef_1 + coefficients[[52]] * mydata$income_y

util_dep = coefficients[[1]] + coefficients[[2]] * mydata$interest_dep + coefficients[[3]] * mydata$bundling + coefficients[[4]] * mydata$dep_cost + coefficients[[5]] * mydata$dep_ease + coefficients[[6]] * mydata$dep_sec +
+ 0 * coefficients[[7]] * mydata$anon_cash + coefficients[[8]] * mydata$budgeting_dep + coefficients[[9]] * mydata$online_dep + coefficients[[10]] * mydata$fast_dep +
+ 0 * coefficients[[11]] * mydata$accept_cash + mydata$age_coef_1 + mydata$educ_coef_1 + mydata$sett_typ_coef_1 + mydata$gender_coef_1 + mydata$inet_coef_1 + mydata$sett_coef_1 + coefficients[[52]] * mydata$income_y

util_cash = 0 * coefficients[[1]] + 0 * coefficients[[2]] * mydata$interest_dep + 0 * coefficients[[3]] * mydata$bundling + coefficients[[4]] * mydata$cash_cost + coefficients[[5]] * mydata$cash_ease + coefficients[[6]] * mydata$cash_sec +
+ coefficients[[7]] * mydata$anon_cash + coefficients[[8]] * mydata$budgeting_cash + coefficients[[9]] * 0 * mydata$online_dep + coefficients[[10]] * mydata$fast_cash +
+ coefficients[[11]] * mydata$accept_cash + mydata$age_coef_1 + mydata$educ_coef_1 + mydata$sett_typ_coef_1 + mydata$gender_coef_1 + mydata$inet_coef_1 + mydata$sett_coef_1 + coefficients[[52]] * mydata$income_y

prob_cbdc = exp(util_cbdc)/(exp(util_dep)+exp(util_cash)+exp(util_cbdc))

prob_cbdc = data.frame (prob_cbdc)

mydata$probab = probab

mydata$probab_day = mydata$summ * mydata$probab 

write.csv (mydata, file = "A:\\School\\Альма-матер\\Курсовая\\in_anketa_5\\FINALE.csv", row.names = FALSE )
