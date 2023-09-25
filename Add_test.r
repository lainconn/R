# session
Sys.setlocale (, "ru_Ru")
mydata = read.csv ("A:\\School\\Альма-матер\\Курсовая\\in_anketa_5\\Test.csv")
library(nnet, help, pos = 2, lib.loc = NULL)
library(tidyverse)

#model
mydata$ind_assess_fact = relevel (factor(mydata$ind_assess), ref = "ЗНАНИЙ И НАВЫКОВ НЕТ")
mydata$assess_1 = relevel (factor(mydata$assess_1), ref = "ПРАВИЛЬНО")
mydata$assess_2 = relevel (factor(mydata$assess_2), ref = "ПРАВИЛЬНО")
mydata$assess_3 = relevel (factor(mydata$assess_3), ref = "ПРАВИЛЬНО")
mydata$assess_4 = relevel (factor(mydata$assess_4), ref = "ПРАВИЛЬНО")
mydata$assess_5 = relevel (factor(mydata$assess_5), ref = "ПРАВИЛЬНО")
mydata$assess_6 = relevel (factor(mydata$assess_6), ref = "ПРАВИЛЬНО")
mydata$assess_7 = relevel (factor(mydata$assess_7), ref = "ПРАВИЛЬНО")
mydata$assess_8 = relevel (factor(mydata$assess_8), ref = "ПРАВИЛЬНО")
interest_fit = data.frame(mydata$interest_dep, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = FALSE)

fin_res = multinom (ind_assess_fact ~ assess_1 + assess_2 + assess_3 + assess_4 + assess_5 + assess_6 + assess_7 + assess_8, data = mydata)
fin_res_fit = round(fitted(fin_res), 4)

finka = fin_res_fit [,1]
finka_2 = fin_res_fit [,2]

fin_res_2 = data.frame(fin_res_fit, row.names =  , check.rows = TRUE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())
#predprob = data.frame(predprob, row.names = , check.rows = TRUE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())


plot = ggplot () +
geom_point (mapping = aes (x = fin_res_fit[1], y = mydata$interest_dep))
