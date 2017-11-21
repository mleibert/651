rm(list=ls())

setwd("g://math//651")

prod<-read.table("ProductivityImprovement.txt")
names(prod)<-c("Y","X1","X2")
tail(prod)

par(mfrow=c(1,3))
boxplot(prod[which(prod[2] == 1),1])
boxplot(prod[which(prod[2] == 2),1])
boxplot(prod[which(prod[2] == 3),1])

prod.r<-max(as.numeric(prod[,2]))
prod.n<-nrow(prod)

Y<-as.matrix(prod[,1])
X<-matrix(0,nrow(prod),3)
B<-matrix(NA,3,1)
E<-matrix(NA,nrow(prod),1)
Ybar<-rep(NA,nrow(prod))

for (i in 1:3){
	Ybar[which(prod[,2] == i)]<-mean(prod[which(prod[,2] == i),1 ])
	X[which(prod[,2] == i),i]<-	prod[which(prod[,2] == i),2]/
		prod[which(prod[,2] == i),2]
	B[i,1]<-mean(prod[which(prod[,2] == i),1])
	E[which(prod[,2] == i),1]<-prod[which(prod[,2] == i),1]-B[i,1]
}

options(scipen=999)
(X%*%B+E) 
sum(E)

prod$X1<-as.factor(prod$X1)
anova(lm(prod$Y~prod$X1))

## Check anova
n<-mu<-rep(NA,3)
for (i in 1:3){
	mu[i]<-mean(prod[ which(prod$X1 == i) ,1])
	n[i]<-max(prod[ which(prod$X1 == i) ,3])
}
MEAN<-sum(mu*n/nrow(prod))
sum(n*(mu-MEAN )^2)


qf(1-.05,prod.r-1,prod.n-prod.r)
summary(aov(prod$Y~prod$X1))[[1]][1,4]

#if F*=15.72053 <= F() =  3.402826, fail to reject H0. 
#Since 15.72053 > 3.402826, reject H0

1-pf(summary(aov(prod$Y~prod$X1))[[1]][1,4],prod.r-1,prod.n-prod.r)

####################

rm(list=ls())


prod<-read.table("ProductivityImprovement.txt")
names(prod)<-c("Y","X1","X2")
tail(prod)

prod.r<-max(as.numeric(prod[,2]))
prod.n<-nrow(prod)

Y<-as.matrix(prod[,1])
X<-matrix(0,nrow(prod),3)
B<-matrix(NA,3,1)
E<-matrix(NA,nrow(prod),1)
Ybar<-rep(NA,nrow(prod))

for (i in 1:3){
	Ybar[which(prod[,2] == i)]<-mean(prod[which(prod[,2] == i),1 ])
	X[which(prod[,2] == i),i]<-	prod[which(prod[,2] == i),2]/
		prod[which(prod[,2] == i),2]
	B[i,1]<-mean(prod[which(prod[,2] == i),1])
	E[which(prod[,2] == i),1]<-prod[which(prod[,2] == i),1]-B[i,1]
}


X[which(prod$X1 == max(as.numeric(prod$X1 )) ), ]<- (-1)
Y;X;B

pmatrix(X,digits=0)

" \mu + \tau_1"




lm(Y ~ X[,1]+ X[,2]   )

(sum(anova(lm(Y ~ X[,1]+ X[,2]   ))[1:(prod.r-1),3])/(prod.r-1))/
sum(anova(lm(Y ~ X[,1]+ X[,2]   ))[ (prod.r ),3]) 

qf(1-.05,prod.r-1,prod.n-prod.r)


