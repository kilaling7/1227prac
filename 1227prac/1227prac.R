setwd('E:/1206')
PSA_A <- read.csv('PSA_A_2022.csv',header=T)
PSA_A$Svi <- as.factor(PSA_A$Svi)

set.seed(99)
N<-nrow(PSA_A)
Sindex=sample(N,67)
Train=PSA_A[Sindex,]
Test=PSA_A[-Sindex,]
M1 <- lm(PSA~.,data=Train)
summary(M1)

library(car)
library(lmtest)
library(nortest)
library(randtests)

###Function Form 結果是假設未通過
es=rstandard(M1)
residualPlot(M1,type="rstandard",quadratic=F)
resettest(M1,power=2,type='regressor')
#Homogeneity 假設未通過
ncvTest(M1)#This test is often called the Breusch-Pagan test; 

