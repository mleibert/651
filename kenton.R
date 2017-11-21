kenton<-read.table("kenton.txt")
names(kenton)<-c("Y","X1","X2")
tail(kenton)

kenton$X1<-as.factor(kenton$X1)

anova(lm(kenton$Y~kenton$X1))

n<-mu<-rep(NA,4)

for (i in 1:4){
	mu[i]<-mean(kenton[ which(kenton$X1 == i) ,1])
	n[i]<-max(kenton[ which(kenton$X1 == i) ,3])
}

MEAN<-sum(mu*n/nrow(kenton))

sum(n*(mu-MEAN )^2)

kenton.r<-max(as.numeric(kenton$X1))
kenton.n<-nrow(kenton)
qf(1-.05,4-1,19-4)

1-pf(summary(aov(kenton$Y~kenton$X1))[[1]][1,4],kenton.r-1,kenton.n-kenton.r)


####
rm(list=ls())

kenton<-read.table("kenton.txt")
names(kenton)<-c("Y","X1","X2")
tail(kenton)

kenton.r<-max(as.numeric(kenton$X1))
kenton.n<-nrow(kenton)


Y<-as.matrix(kenton[,1])
X<-matrix(0,nrow(kenton),kenton.r)
B<-matrix(NA,kenton.r,1)
E<-matrix(NA,nrow(kenton),1)
Ybar<-rep(NA,nrow(kenton))

for (i in 1:kenton.r){
	Ybar[which(kenton[,2] == i)]<-mean(kenton[which(kenton[,2] == i),1 ])
	X[which(kenton[,2] == i),i]<-	kenton[which(kenton[,2] == i),2]/
		kenton[which(kenton[,2] == i),2]
	B[i,1]<-mean(kenton[which(kenton[,2] == i),1])
	E[which(kenton[,2] == i),1]<-kenton[which(kenton[,2] == i),1]-B[i,1]
}


X[which(kenton$X1 == max(as.numeric(kenton$X1 )) ), ]<- (-1)

lm(Y ~ X[,1]+ X[,2]+ X[,3]  )

anova(lm(Y ~ X[,1]+ X[,2]+ X[,3]  ))

sum(anova(lm(Y ~ X[,1]+ X[,2]+ X[,3]  ))[1:3,3])/3

