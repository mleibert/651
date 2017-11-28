
getwd()
setwd("G://math//651")
hay<-read.table("HayFeverRelief.txt")

hay.fv<-(((aov(hay[,1]~ as.factor(hay[,2])*as.factor(hay[,3]))$fitted)))
unique(round(as.vector(hay.fv),4))

hay.e<-round(aov(hay[,1]~ as.factor(hay[,2]) * as.factor(hay[,3]))$residuals,4)
as.vector(hay.e)


head(hay)

mean(hay[1:4,1])

plot(hay.fv,as.vector(hay.e))
qqnorm(as.vector(hay.e))


par(mar=c(2,2,2,2),mfrow=c(1,3) )
plot(1:3,	unique(round(as.vector((((aov(hay[,1]~ 
	as.factor(hay[,2]) )$fitted)))),4)),col="blue",pch=16)
lines(1:3, unique(round(as.vector((((aov(hay[,1]~ 
	as.factor(hay[,2]) )$fitted)))),4))  , col='red')

aov(hay[,1]~ as.factor(hay[,2]) * as.factor(hay[,3]))
anova(lm(hay[,1]~ as.factor(hay[,2]) * as.factor(hay[,3])))