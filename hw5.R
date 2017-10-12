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