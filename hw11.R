 rm(list = ls())

setwd("G:\\math\\651")

ad<-read.table("AnnualDues.txt",header=F)
head(ad);names(ad)<-c("Y","X")

#MLE
glm(ad$Y~ad$X,family="binomial")

B0<-as.numeric(glm(ad$Y~ad$X,family="binomial")$coefficients[1])
B1<-as.numeric(glm(ad$Y~ad$X,family="binomial")$coefficients[2])

##
#(1+exp(-B0-B1*X))^-1

exp(B1)	#this is the estimated odds ratio

(1+exp(-B0-B1*x))^-1

-log(3)-4.8075
-5.906112/-  0.1251  


f <- function(x)  ( (1+exp(-B0-B1*x))^(-1) -.75 ) 
uniroot(f, lower=0.1, upper=100000000)$root








####### 14.13

cp<-read.table("CarPurchase.txt",header=F)
head(cp)
names(cp)<-c("Y","X1","X2")

cp.glm<-glm(cp$Y~cp$X1+cp$X2,family="binomial");
cp.glm$coefficients*-c(1,50,3)

as.numeric(exp(cp.glm$coefficients))[-1]
#With a unit increase in the 1st or 2nd covariate (income or age) we expect 
#the odds of success to increase exp(b1)= 1.070079 or exp(b2)= 1.819627
#times. This means that the odds of buying a new car increase by 7% for every 
#additional $1,000 of income and by 82% for every additional year of age.

1/( 1+exp( sum(cp.glm$coefficients*-c(1,50,3) )  ) )


#########


ad<-read.table("AnnualDues.txt",header=F)
names(ad)<-c("Y","X")
head(ad)

ad.lm<-glm(ad$Y~ad$X,family="binomial")
summary(ad.lm)
summary(ad.lm)$coefficients[2,2]

 c(exp(0.1251-qnorm(1-.1/2)*summary(ad.lm)$coefficients[2,2]),
 	exp(0.1251+qnorm(1-.1/2)*summary(ad.lm)$coefficients[2,2]))

#

 abs(0.1251  /summary(ad.lm)$coefficients[2,2])

if (abs(0.1251  /summary(ad.lm)$coefficients[2,2]) > qnorm(1-.1/2) ){
	print("reject H0")} else {print("fail to reject H0")}

2*(1-pnorm( 1.87393))


##
names(summary(ad.lm))
ad.G2<-summary(ad.lm)$null.deviance-summary(ad.lm)$deviance;ad.G2

if (ad.G2 < qchisq(1-.1,1) ) {print("fail to reject H0")} else 
	{print("reject H0")}

1-pchisq(ad.G2 ,1)

