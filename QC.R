
setwd("g://math//651")

QC<-read.table("QuestionnaireColor.txt")
names(QC)<-c("Y","X1","X2")
tail(QC)

par(mfrow=c(1,3))
boxplot(QC[which(QC[2] == 1),1])
boxplot(QC[which(QC[2] == 2),1])
boxplot(QC[which(QC[2] == 3),1])

Y<-as.matrix(QC[,1])
X<-matrix(0,nrow(QC),3)
B<-matrix(NA,3,1)
E<-matrix(NA,nrow(QC),1)
Ybar<-rep(NA,nrow(QC))

for (i in 1:3){
	Ybar[which(QC[,2] == i)]<-mean(QC[which(QC[,2] == i),1 ])
	X[which(QC[,2] == i),i]<-	QC[which(QC[,2] == i),2]/
		QC[which(QC[,2] == i),2]
	B[i,1]<-mean(QC[which(QC[,2] == i),1])
	E[which(QC[,2] == i),1]<-QC[which(QC[,2] == i),1]-B[i,1]
}

options(scipen=999)
E
(X%*%B+E) 
sum(E)

QC$X1<-as.factor(QC$X1)
anova(lm(QC$Y~QC$X1))

## Check anova
n<-mu<-rep(NA,3)
for (i in 1:3){
	mu[i]<-mean(QC[ which(QC$X1 == i) ,1])
	n[i]<-max(QC[ which(QC$X1 == i) ,3])
}
MEAN<-sum(mu*n/nrow(QC))
sum(n*(mu-MEAN )^2)

QC.r<-max(as.numeric(QC[,2]))
QC.n<-nrow(QC)

qf(1-.1,QC.r-1,QC.n-QC.r)
summary(aov(QC$Y~QC$X1))[[1]][1,4]

#if F*=15.72053 <= F() =  3.402826, fail to reject H0. 
#Since 15.72053 > 3.402826, reject H0

1-pf(summary(aov(QC$Y~QC$X1))[[1]][1,4],QC.r-1,QC.n-QC.r)


####







Y<-as.matrix(QC[,1])

X<-matrix(0,nrow(QC),3)
X

B<-matrix(NA,3,1)
E<-matrix(NA,nrow(QC),1)
Ybar<-rep(NA,nrow(QC))

for (i in 1:3){
	Ybar[which(QC[,2] == i)]<-mean(QC[which(QC[,2] == i),1 ])
	X[which(QC[,2] == i),i]<-	QC[which(QC[,2] == i),2]/
		QC[which(QC[,2] == i),2]
	B[i,1]<-mean(QC[which(QC[,2] == i),1])
	E[which(QC[,2] == i),1]<-QC[which(QC[,2] == i),1]-B[i,1]
}

#############################


rm(list=ls())

QC<-read.table("QuestionnaireColor.txt")
names(QC)<-c("Y","X1","X2")
tail(QC)

QC.r<-max(as.numeric(QC[,2]))
QC.n<-nrow(QC)

Y<-as.matrix(QC[,1])
X<-matrix(0,nrow(QC),3)
B<-matrix(NA,3,1)
E<-matrix(NA,nrow(QC),1)
Ybar<-rep(NA,nrow(QC))

for (i in 1:QC.r){
	Ybar[which(QC[,2] == i)]<-mean(QC[which(QC[,2] == i),1 ])
	X[which(QC[,2] == i),i]<-	QC[which(QC[,2] == i),2]/
		QC[which(QC[,2] == i),2]
	B[i,1]<-mean(QC[which(QC[,2] == i),1])
	E[which(QC[,2] == i),1]<-QC[which(QC[,2] == i),1]-B[i,1]
}


X[which(QC$X1 == max(as.numeric(QC$X1 )) ), ]<- (-1)
Y;X;B

pmatrix(X,digits=0)

X[which(QC$X1 == max(as.numeric(QC$X1 )) ), ]<- (-1)
lm(Y ~ X[,1]+ X[,2]   )

(sum(anova(lm(Y ~ X[,1]+ X[,2]   ))[1:(QC.r-1),3])/(QC.r-1))/
sum(anova(lm(Y ~ X[,1]+ X[,2]   ))[ (QC.r ),3]) 

qf(1-.05,QC.r-1,QC.n-QC.r)








