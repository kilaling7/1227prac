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

#Normality 未通過 拒絕H0 不常態
qqPlot(M1)
lillie.test(es)#KS test for normality
shapiro.test(es)#Shapiro-Wilk Normality Test
plot(es,type = "l",col='2')
acf(es, ci=0.99)
#dwtest(M1)#Durbin-Watson test 檢定獨立性 因P-VALUE=0.8041>0.05 不拒絕
runs.test(es)

#Phase V:
library(MASS)
boxcox(M1)

M2=lm(log(PSA)~.,data=Train)
summary(M2)
