rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

#
gpa<-read.table("gradepointaverage.txt")
head(gpa)
colnames(gpa)<-c("Y","X")

b0<-as.numeric(lm(gpa$Y~gpa$X)[[1]][1]) ;b0
b1<-as.numeric(lm(gpa$Y~gpa$X)[[1]][2]) ;b1
gpalm<-function(X){X*b1+b0 }


n.gpa=nrow(gpa)
gpa$ei<-(gpa$Y-gpalm(gpa$X))
sum(gpa$ei)
gpa$eiei<-gpa$ei^2
MSE<-sum(gpa$eiei)/(n.gpa-2);MSE
gpa$SR<-(gpalm(gpa$X)-mean(gpa$Y))^2
SSR<-sum(gpa$SR);SSR
MSR<-SSR

SSTO<-sum( (gpa$Y-mean(gpa$Y))^2 ); SSTO
SSR
SSE<-

#a
anova(lm(gpa$Y~gpa$X))

#c
Ftest<-MSR/MSE;Ftest

#Alpha?
alpha=.01
dr<-qf(1-alpha,1,n.gpa-2)


if( Ftest > dr ) {print("conclude Ha")} else {print("conclude H0")}

#e
cor(gpa$Y,gpa$X)

#f
SSR/sum(as.data.frame(anova(lm(gpa$Y~gpa$X)))[,2])
cor(gpa$Y,gpa$X)^2

###########

ab<-read.table("AirfreightBreakage.txt")
head(ab)
colnames(ab)<-c("Y","X")


b0<-as.numeric(lm(ab$Y~ab$X)[[1]][1]) ;b0
b1<-as.numeric(lm(ab$Y~ab$X)[[1]][2]) ;b1
ablm<-function(X){X*b1+b0 }


n.ab=nrow(ab)
ab$ei<-(ab$Y-ablm(ab$X))
sum(ab$ei)
ab$eiei<-ab$ei^2
MSE<-sum(ab$eiei)/(n.ab-2);MSE
ab$SR<-(ablm(ab$X)-mean(ab$Y))^2
SSR<-sum(ab$SR);SSR
MSR<-SSR

#2.23 a
anova(lm(ab$Y~ab$X))
SSTO<-sum( (ab$Y-mean(ab$Y))^2 ); SSTO

paste("(",ablm(ab$X),"-", mean(ab$Y),")^2",sep="")
paste("(",ab$Y,"-", ablm(ab$X),")^2",sep="")

Ftest<-MSR/MSE
Ftest

#Alpha?
alpha=.05
dr<-qf(1-alpha,1,n.ab-2);dr

if( Ftest > dr ) {print("conclude Ha")} else {print("conclude H0")}


summary(lm(ab$Y~ab$X))
anova(lm(ab$Y~ab$X))

(4/(sqrt(
MSE/
sum((ab$X-mean(ab$X))^2)
))) 


###### 2.26


ph<-read.table("PlasticHardness.txt")
head(ph)
colnames(ph)<-c("Y","X")

ph.b0<-as.numeric(lm(ph$Y~ph$X)[[1]][1]) ;ph.b0
ph.b1<-as.numeric(lm(ph$Y~ph$X)[[1]][2]) ;ph.b1
ph.lm<-function(X){ ph.b0+ph.b1*X}


n.ph=nrow(ph);n.ph
ph$ei<-(ph$Y-ph.lm(ph$X) )
sum(ph$ei)
ph$eiei<-ph$ei^2
ph.MSE<-sum(ph$eiei)/(n.ph-2);ph.MSE
ph$SR<-(ph.lm(ph$X)-mean(ph$Y))^2
ph.SSR<-sum(ph$SR);ph.MSE
ph.MSR<-ph.SSR

ph.SSTO<-sum( (ph$Y-mean(ph$Y))^2 ); ph.SSTO

#a
boxplot(ph$ei)

###### 2.26

#a
anova(lm(ph$Y~ph$X))
#c
Ftest<-ph.MSR/ph.MSE;Ftest

#Alpha?
alpha=.01
dr<-qf(1-alpha,1,n.ph-2)

if( Ftest > dr ) {print("conclude Ha")} else {print("conclude H0")}

plot(ph$X,ph$Y)
cor(ph$X,ph$Y)^2


meanz<-rep(0,4)
for(i in 1:length(unique(ph$X))) { 
	meanz[i]<- mean(ph[which(ph$X == unique(ph$X)[i]),1])	}
meanz

SPE<-data.frame(ph$Y,ph$X,sort(rep(meanz,4)))
spe<-(ph$Y-SPE[,3])^2
ph.SSPE<-sum(spe)
ph.SSLF <- sum(ph$eiei) - ph.SSPE

ph.Ftest<-(ph.SSLF/(4-2) )/( ph.SSPE /12)

#Alpha?
ph.alpha=.01
dr<-qf(1-ph.alpha,2,12);dr

if( ph.Ftest> dr ) {print("conclude Ha")} else {print("conclude H0")}












