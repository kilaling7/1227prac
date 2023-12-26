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


e2s=rstandard(M2)
residualPlot(M2,type="rstandard",quadratic=F)

resettest(M2,power=2,type='regressor')
ncvTest(M2)#This test is often called the Breusch-Pagan test; 

qqPlot(M2)
lillie.test(e2s)#KS test for normality
shapiro.test(e2s)#Shapiro-Wilk Normality Test

plot(e2s,type = "l",col='2')
acf(e2s,ci=0.99)
#dwtest(M2)#Durbin-Watson test
runs.test(es)

s1=step(M2)#寫AIC=
summary(s1)
M2a=lm(log(PSA)~Age+Bph+Svi+Cap+Gscore,data=Train)
summary(M2a)
M2b=lm(log(PSA)~+Bph+Svi+Cap+Gscore,data=Train)
summary(M2b)

vif(M1)
vif(M2a)
vif(M2b)

M1p=predict(M1, newdata=Test)
r1=M1p-Test$PSA
MSE1=mean(r1^2)
RMSE1 = sqrt(MSE1)
MAE1 = mean(abs(r1))
MAPE1=mean(abs(r1/Test$PSA))

M2ap=predict(M2a, newdata=Test)
r2a=exp(M2ap)-Test$PSA
MSE2a=mean(r2a^2)
RMSE2a = sqrt(MSE2a)
MAE2a = mean(abs(r2a))
MAPE2a=mean(abs(r2a/Test$PSA))

M2bp=predict(M2b, newdata=Test)
r2b=exp(M2bp)-Test$PSA
MSE2b=mean(r2b^2)
RMSE2b = sqrt(MSE2b)
MAE2b = mean(abs(r2b))
MAPE2b=mean(abs(r2b/Test$PSA))

summary(M2a)
confint(M2a)

##
influencePlot(M2a)
n=dim(Train)[1]
p=length(M2a$coef)
a=cooks.distance(M2a)
plot(a)
abline(h=4/(n-p), lty=2)
identify(1:n, a, row.names(Train))
