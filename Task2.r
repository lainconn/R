# session
Sys.setlocale (, "ru_Ru")
mydata = read.csv ("A:\\School\\Альма-матер\\Курсовая\\in_anketa_5\\Test.csv")
library (tidyverse)
library (nnet)

#filtering
mydata$cash_cost = as.numeric(mydata$cash_cost)
mydata$dep_cost = as.numeric(mydata$dep_cost)
mydata$cash_sec = as.numeric(mydata$cash_sec)
mydata$dep_sec = as.numeric(mydata$dep_sec)
mydata$dep_ease = as.numeric(mydata$dep_ease)
mydata$cash_ease = as.numeric(mydata$cash_ease)
mydata$interest_dep = as.numeric (mydata$interest_dep)

# model
res_cash = lm ( cash_day ~ cash_cost + cash_ease + cash_sec + anon_cash + budgeting_cash + fast_cash + accept_cash + age_1 + educ_1 + sett_typ_1 + gender_1 + inet_1 + sett + income_y, data = mydata)
res_dep = lm ( dep_day ~ interest_dep + bundling + online_dep + dep_cost + dep_ease + dep_sec + budgeting_dep + fast_dep + age_1 + educ_1 + sett_typ_1 + gender_1 + inet_1 + sett + income_y, data = mydata)

util_cash = res_cash$coefficients[1] + (res_cash$coefficients[2] * mydata$cash_cost) + (res_cash$coefficients[3] * mydata$cash_ease) +
(res_cash$coefficients[4] * mydata$cash_sec) + (res_cash$coefficients[5] * mydata$anon_cash) + (res_cash$coefficients[6] * mydata$budgeting_cash) +
(res_cash$coefficients[7] * mydata$fast_cash) + (res_cash$coefficients[8] * mydata$accept_cash) + (mydata$sett_coef * mydata$sett_coef_fact) + 
+ (res_cash$coefficients[49] * mydata$income_y) + (mydata$age_coef * mydata$age_fact) + (mydata$educ_coef * mydata$educ_fact) + (mydata$sett_typ_fact * mydata$sett_typ_coef) +
+ (mydata$gender_coef * mydata$gender_fact) + (mydata$inet_coef * mydata$inet_fact)

util_dep = res_dep$coefficients[1] + (res_dep$coefficients[2] * mydata$interest_dep) +  (res_dep$coefficients[3] * mydata$bundling) + (res_dep$coefficients[4] * mydata$online_dep) +
(res_dep$coefficients[5] * mydata$dep_cost) + (res_dep$coefficients[6] * mydata$dep_ease) + (res_dep$coefficients[7] * mydata$dep_sec) +
(res_dep$coefficients[8] * mydata$budgeting_dep) + (res_dep$coefficients[9] * mydata$fast_dep) + (mydata$sett_coef_1 * mydata$sett_coef_fact) + 
+ (res_dep$coefficients[50] * mydata$income_y) + (mydata$age_coef_1 * mydata$age_fact) + (mydata$educ_coef_1 * mydata$educ_fact) + (mydata$sett_typ_fact * mydata$sett_typ_coef_1) +
+ (mydata$gender_coef_1 * mydata$gender_fact) + (mydata$inet_coef_1 * mydata$inet_fact)


util_cash = as.vector(util_cash)
util_dep = as.vector (util_dep)
util_margin = vector (length = length(util_cash))

for (i in seq_along(util_cash)) {
    if(is.na (util_cash[i]) || is.na (util_dep[i])) {
        util_margin[i] = NA
  } else if (util_cash[i] > util_dep[i]) {
    util_margin[i] = "cash"
  } else {
    util_margin[i] = "dep"
  }
}

mydata$util_margin = util_margin
mydata$util_margin_sorted = relevel (factor(mydata$util_margin), ref = "cash" )

res = multinom (util_margin_sorted ~ interest_dep + bundling + I(dep_cost - cash_cost) + I(dep_ease-cash_ease) + I(dep_sec-cash_sec) + 
+ anon_cash + I(budgeting_dep - budgeting_cash) + online_dep + I(fast_dep - fast_cash) + accept_cash +  age_1 + educ_1 + sett_typ_1 + gender_1 + inet_1 + sett + income_y, data = mydata)


plot = (ggplot(data = mydata) +
 geom_point (mapping = aes (x = bundling, y = util_margin_sorted)))

prob_dep = (exp(util_dep))/(exp(util_dep) + exp (util_cash))
