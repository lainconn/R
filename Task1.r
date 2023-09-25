# session
Sys.setlocale (, "ru_Ru")
mydata = read.csv ("A:\\School\\Альма-матер\\Курсовая\\in_anketa_5\\Test.csv")
library (ggplot2)

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
#print(summary (res))

predprob = round(fitted(res),2)

interest = setNames (mydata$interest_dep, paste0("", seq_along(mydata$interest_dep)))

print (mean (predprob, na.rm = TRUE))

# utility cbdc_c = cashlike
coefficients = coef (res)

util_cash = coefficients['I(dep_cost - cash_cost)'] * mydata$cash_cost + coefficients['I(dep_ease - cash_ease)'] * mydata$cash_ease + 
+ coefficients['I(dep_sec - cash_sec)'] * mydata$cash_sec

util_cbdc_c = util_cash

util_dep = coefficients['(Intercept)'] + (coefficients['interest_dep'] * mydata$interest_dep) + 
+ (coefficients['bundling'] * mydata$bundling) + (coefficients['I(dep_cost - cash_cost)'] * mydata$dep_cost) + 
+ (coefficients['I(dep_ease - cash_ease)'] * mydata$dep_ease) + (coefficients['I(dep_sec - cash_sec)'] * mydata$dep_sec)


# graph

print(ggplot (mydata, aes (x= bundling, y = choice)) + geom_smooth  ()) #+
#stat_smooth (method = "glm", method.args = list (family = binomial)))

