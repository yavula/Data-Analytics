getwd()
setwd()
data=read.csv('loan_final313.csv')
str(data)
summary(data)
View(data)
library(stringr)
str_pad(data$final_d, 8, pad = "0")

data$finalDate <- as.Date(str_pad(data$final_d, 8, pad = "0"),"%m%d%Y")
data$issueDate <- as.Date(data$issue_d,"%m/%d/%Y")

data$loanPeriod <- data$finalDate - data$issueDate
data=data[,-c(1,2,19,20)]
data$loanPeriod=round((data$loanPeriod/365),2)
View(data)
str(data)








