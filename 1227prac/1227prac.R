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