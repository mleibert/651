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
















































































