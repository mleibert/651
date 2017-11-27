 


rm(list=ls())

setwd("g://math//651")

abt<-read.table("abt.txt")
names(abt)<-c("Y","X1","X2" )
tail(abt)
abt.n<-nrow(abt)

abt$X1<-as.factor(abt$X1)

unique( round(as.vector (aov(Y~(X1),data=abt)$fitted ),4))

dat<-data.frame(((as.vector(aov(Y~(X1),data=abt)$residuals)^2)),abt$X1)
abt.ss<-rep(NA,max(as.vector(dat[,2]) ))


for (i in 1:max(as.vector(dat[,2]) ) ){
	abt.ss[i]<-(sum(dat[which(dat[,2] == i),1]))/
		(length(dat[which(dat[,2] == i),1])-1)}

require("SuppDists")

abt.alpha<-.05
qmaxFratio(1-abt.alpha,max(as.vector(abt$X2))-1,
	as.numeric(max(as.vector(abt$X1))))

max(abt.ss)/min(abt.ss)

#since H* = 10.44493 > H(.95,5,7) = 9.697305; we reject H0 and conclude
#the five treatment variances are not equal

