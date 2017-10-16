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
cp.H<-cp.X %*% solve(t(cp.X)%*%cp.X) %*% t(cp.X)
cp.ee<-(diag(cp.n)-cp.hat) %*% cp.Y
as.vector(cp.ee)
cp.p<-5

boxplot(cp.ee)

 (anova(  lm(cp[,1]~cp[,2]+cp[,3]+cp[,4]+cp[,5]) ))

cp.J<-matrix(1,cp.n,cp.n)
cp.MSR<-(t(cp.Y)%*%(cp.H-(1/cp.n)*cp.J)%*%cp.Y)/(cp.p-1)

t(cp.b) %*% t(cp.X) %*%cp.Y - (1/cp.n)*t(cp.Y)%*%cp.J%*%cp.Y


cp.b<-matrix(as.numeric(lm(cp[,1]~cp[,2]+cp[,3]+cp[,4]+cp[,5])[[1]][1:5]),5,1)

cp.MSE<-t(cp.ee)%*%(cp.ee)/(cp.n-cp.p)

cp.MSR/cp.MSE
cp.alpha<-.05
qf(1-cp.alpha,cp.p-1,cp.n-cp.p)


