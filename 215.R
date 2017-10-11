# Confidence Interval
b1=4;b0=10.2
ablm<-function(X){ X*b1+b0 }

Xh=4

Yh=ablm(Xh);Yh
n<-nrow(ab)

ab$ei<-ab$Y-ablm(ab$X)


MSE<-sum(ab$ei^2)/(n-2);MSE
Xbar<-mean(ab$X);Xbar

ss<-MSE*((1/n) + ( (Xh-Xbar)^2 )/(sum((ab$X-Xbar)^2)));ss
s<-sqrt(ss);s

#confidence level?
CL<-.99
alpha<-(1-CL)/2;alpha
cl<-qt(1-alpha,n-2);cl

Yh-cl*s;Yh+cl*s


################### Prediction Interval

Xh=2

Yh=ablm(Xh);Yh
n<-nrow(ab);n

ab$ei<-ab$Y-ablm(ab$X)


MSE<-sum(ab$ei^2)/(n-2);MSE
Xbar<-mean(ab$X);Xbar

ss<-MSE*(1+(1/n) + ( (Xh-Xbar)^2 )/(sum((ab$X-Xbar)^2)));ss
s<-sqrt(ss);s

#confidence level?
CL<-.99
alpha<-(1-CL)/2;alpha
cl<-qt(1-alpha,n-2);cl

Yh-cl*s;Yh+cl*s




################### Multiple Predictions Interval

Xh=2
m=3

Yh=ablm(Xh);Yh
n<-nrow(ab);n

ab$ei<-ab$Y-ablm(ab$X)


MSE<-sum(ab$ei^2)/(n-2);MSE
Xbar<-mean(ab$X);Xbar

ss<-MSE*((1/m)+(1/n) + ( (Xh-Xbar)^2 )/(sum((ab$X-Xbar)^2)));ss
s<-sqrt(ss);s

#confidence level?
CL<-.99
alpha<-(1-CL)/2;alpha
cl<-qt(1-alpha,n-2);cl

Yh-cl*s;Yh+cl*s

###################   Regression Line Band

Xh=2
Yh=ablm(Xh);Yh
n<-nrow(ab);n

ss<-MSE*((1/n) + ( (Xh-Xbar)^2 )/(sum((ab$X-Xbar)^2)));ss
s<-sqrt(ss);s

#confidence level?
CL<-.99
alpha<-(1-CL)/2;alpha
WW<-2*qf(CL,2,n-2);WW

Yh
Yh-sqrt(WW)*s;Yh+sqrt(WW)*s


Xh=4
Yh=ablm(Xh);Yh
n<-nrow(ab);n

ss<-MSE*((1/n) + ( (Xh-Xbar)^2 )/(sum((ab$X-Xbar)^2)));ss
s<-sqrt(ss);s

#confidence level?
CL<-.99
alpha<-(1-CL)/2;alpha
WW<-2*qf(CL,2,n-2);WW

Yh
Yh-sqrt(WW)*s;Yh+sqrt(WW)*s