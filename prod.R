rm(list=ls())

setwd("g://math//651")

prod<-read.table("ProductivityImprovement.txt")
names(prod)<-c("Y","X1","X2")
tail(prod)

par(mfrow=c(1,3))
boxplot(prod[which(prod[2] == 1),1],ylim=c(5,10))
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

aov(lm())

options(scipen=999)
(X%*%B+E) 
sum(E)

prod$X1<-as.factor(prod$X1)
anova(lm(prod$Y~prod$X1)) 

aov(prod$Y~prod$X1)$fitted

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

#####
par(pin=c(1,1))
plot(1:prod.r,unique(Ybar),col="blue",pch=16)
lines(1:prod.r, unique(Ybar)  , col='red')


prod.MSE<-anova(lm(Y ~ X[,1]+ X[,2]   ))[3,2]
prod.n<-rep(NA,max(as.numeric(prod$X1)))
for(i in 1:max(as.numeric(prod$X1))){prod.n[i]<-
	max(prod[which(prod$X1==i),3])}

unique(Ybar)

prod.T<-qtukey(1-.1,prod.r,nrow(prod)-prod.r)*(1/sqrt(2))
prod.list<-list()

for ( i in 1:ncol(combn(1:prod.r, 2))){
	mu<-c(unique(Ybar)[combn(1:prod.r, 2)[,i][1]],
		unique(Ybar)[combn(1:prod.r, 2)[,i][2]])
	Dhat<-max(mu)-min(mu)
	
	ssD<-prod.MSE*(1/(prod.n[combn(1:prod.r, 2)[,i][1]]) +
		(1/ (prod.n[combn(1:prod.r, 2)[,i][2]])  ))
	sqrt(ssD)
	prod.list[[i]]<-data.frame(round(Dhat-prod.T*sqrt(ssD),3),
	paste0("(",round(max(mu),2),"-",round(min(mu),2),")-",
		round(prod.T,2),"(",round(sqrt(ssD),2),")"),
	paste0("mu_", combn(1:prod.r, 2)[,i][ which(mu == max(mu))], "-mu_",
	combn(1:prod.r, 2)[,i][ which(mu == min(mu))] ),
	paste0("(",round(max(mu),2),"-",round(min(mu),2),")-",
		round(prod.T,2),"(",	round(sqrt(ssD),2),")"),
	round(Dhat+prod.T*sqrt(ssD),3)	)
	colnames(prod.list[[i]])<-c("","","","","")
}

TukeyHSD(	aov(prod$Y ~ prod$X1 ),	conf.level = 0.9) 


#####

plot(aov(Y~X1,data=prod)$fitted,aov(Y~X1,data=prod)$residuals)
qqnorm(aov(Y~X1,data=prod)$residuals); 

