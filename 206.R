ab<-read.table("AirfreightBreakage.txt")
colnames(ab)<-c("Y","X")
lm(ab$Y~ab$X)
b1<-4;b0<-10.2
ablm<-function(X){ X*b1+b0 }
ab$ei<-ab$Y-ablm(ab$X)

n=nrow(ab);n
MSE<-sum(ab$ei^2)/(n-2);MSE

b1<-sum( ( ab$X - mean(ab$X) )*( ( ab$Y - mean(ab$Y) ) ) ) /
	sum(	( ab$X - mean(ab$X) )^2 )
lm(ab$Y~ab$X)


ss<-MSE/sum( ( ab$X - mean(ab$X) )^2 );ss
s<-sqrt(ss);s

CL<-.95
alpha<-(1-CL)/2;alpha
cl<-qt(1-alpha,n-2);cl


b1-cl*s;b1+cl*s
 

## t test

ttest<-b1/s
ttest

CL<-.95
alpha<-(1-CL)/2;alpha
cl<-qt(1-alpha,n-2);cl

if( abs(ttest) > cl ) {print("conclude Ha")} else {print("conclude H0")}

dt( 1.81,n-1)



summary(lm(ab$Y~ab$X))










































































