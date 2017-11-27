 


rm(list=ls())

setwd("g://math//651")

cash<-read.table("CashOffers.txt")
names(cash)<-c("Y","X1","X2","X3")
tail(cash)

cash$X1<-as.factor(cash$X1)

plot(aov(Y~(X1),data=cash)$fitted,aov(Y~X1,data=cash)$residuals)
qqnorm(aov(Y~X1,data=cash)$residuals); 

#


unique( round(as.vector (aov(Y~(X1),data=cash)$fitted ),4))

dat<-data.frame(((as.vector(aov(Y~(X1),data=cash)$residuals)^2)),cash$X1)
cash.ss<-rep(NA,max(as.vector(dat[,2]) ))


for (i in 1:max(as.vector(dat[,2]) ) ){
	cash.ss[i]<-(sum(dat[which(dat[,2] == i),1]))/
		(length(dat[which(dat[,2] == i),1])-1)}

require("SuppDists")

cash.alpha<-.01
qmaxFratio(1-cash.alpha,	max(as.vector(cash$X2))-1	,
	as.numeric(max(as.vector(cash$X1)))		)

qmaxFratio(1-.01,11,3)



max(cash.ss)/min(cash.ss)

#since H* = 10.44493 > H(.95,5,7) = 9.697305; we reject H0 and conclude
#the five treatment variances are not equal


pmaxFratio(max(cash.ss)/min(cash.ss),11,3)

