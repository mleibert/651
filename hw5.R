rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

cp<-read.table("CommercialProperties.txt")
head(cp)
colnames(cp)<-c("Y","X1","X2","X3","X4")

pairs(cp)
cor(cp[,-1])

lm(cp[,1]~cp[,2]+cp[,3]+cp[,4]+cp[,5])

#resids
cp.n<-nrow(cp)
cp.Y<-as.matrix(cp[,1])
cp.X<-as.matrix( data.frame(1,cp[,2:5]) )
cp.hat<-cp.X %*% solve(t(cp.X)%*%cp.X) %*% t(cp.X)
cp.ee<-(diag(cp.n)-cp.hat) %*% cp.Y
as.vector(cp.ee)

boxplot(cp.ee)






#6.27
X1=c(7,4,16,3,21,8)
X2=c(33,41,7,49,5,31)
Y=c(42,33,75,28,91,55)
n=length(X1)
X<-rep(1,n)

X<-as.matrix(data.frame(X,X1,X2));colnames(X)<-NULL
Y<-as.matrix(data.frame(Y));colnames(Y)<-NULL
p<-dim(X)[2]
 
b<-solve(t(X)%*%X)%*%t(X)%*%Y;b
H<-X%*%solve(t(X)%*%X) %*% t(X)
e<-(diag(n)-H)%*%Y;e
J<-matrix(1,n,n)
SSR<-t(b)%*%t(X)%*%Y - (1/n)*t(Y) %*%J  %*% Y
MSE<- as.numeric((t(e) %*% e) / (n-p))
MSE*solve(t(X)%*%X)
