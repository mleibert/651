rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

gpa<-read.table("GradePointAverage.txt")
#gpa$X3<-scan("GradePointAverageX2.txt")
head(gpa)
colnames(gpa)<-c("Y","X1"  )
gpa.n<-nrow(gpa)


anova(lm(gpa$Y~gpa$X))

gpa.lm<-function(x){    2.11405   +   0.03883  *x }

gpa.MSE<-sum((gpa$Y-gpa.lm(gpa$X))^2)/(gpa.n-2)
gpa.MSR<-sum((gpa.lm(gpa$X)-mean(gpa$Y))^2)

gpa.F<-gpa.MSR/gpa.MSE

gpa.MSR/(gpa.MSE*(gpa.n-2)+gpa.MSR)

gpa.b1<- 0.03883 
gpa.b1/0.01277 

gpa.b1/0.01277 <2*qt(1-(.05/2),120)

(gpa.b1/0.01277) ^2

gpa.MSR/(gpa.MSE*(gpa.n-2) + gpa.MSR)

gpa.F<qf(1-(.01/2),1,gpa.n)

1-pf(gpa.F,1,gpa.n)












gpa$X1X2<-gpa$X2*gpa$X1

lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 ) 

summary(lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 ))

gpa.t<-as.numeric(lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 )[[1]][4] )  /
		as.numeric(summary(lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 ))[[4]][ 4,2])
gpa.t

gpa.MSE

gpa.alpha<-(1-.95)/2

qt(1-gpa.alpha,gpa.n-3)

abs(gpa.t) < qt(1-gpa.alpha,gpa.n-3)
#reject H0


