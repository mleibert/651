
getwd()
setwd("G://math//651")
hay<-read.table("HayFeverRelief.txt")

hay.fv<-(((aov(hay[,1]~ as.factor(hay[,2])*as.factor(hay[,3]))$fitted)))
unique(round(as.vector(hay.fv),4))

hay.e<-round(aov(hay[,1]~ as.factor(hay[,2]) * as.factor(hay[,3]))$residuals,4)
as.vector(hay.e)

matrix(as.vector(hay.e),4,9)

head(hay)

mean(hay[1:4,1])

plot(hay.fv,as.vector(hay.e))
qqnorm(as.vector(hay.e))


plot(1:3,	unique(round(as.vector((((aov(hay[,1]~ 
	as.factor(hay[,2]) )$fitted)))),4)),col="blue",pch=16)
lines(1:3, unique(round(as.vector((((aov(hay[,1]~ 
	as.factor(hay[,2]) )$fitted)))),4))  , col='red')

aov(hay[,1]~ as.factor(hay[,2]) * as.factor(hay[,3]))
anova(lm(hay[,1]~ as.factor(hay[,2]) * as.factor(hay[,3])))


29.425 / ((3-1)*(3-1))
qf(1-.05,4,27)



110.010/0.060
qf(1-.05,2,27)
1-pf(110.010/0.060,2,27)


61.830/0.060
qf(1-.05,2,27)
1-pf(61.830/0.060,2,27)

