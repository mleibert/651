rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

#1.19
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
anova(lm(ph$Y~ph$X))

#b
ph.Ftest<-ph.MSR/ph.MSE;ph.Ftest

#Alpha?
ph.alpha=.01
dr<-qf(1-ph.alpha,1,n.ph-2);dr

if( ph.Ftest> dr ) {print("conclude Ha")} else {print("conclude H0")}

#c
par(mfrow=c(1,2))
plot(ph[,3],ph[,2],xlim=c(-30,30))
plot(ph.lm(ph$X)-mean(ph[,1]),ph[,2])

par(mfrow=c(1,2))
plot(ph[,2],ph[,1],ylim=c(min(ph$Y),max(ph$Y)),
	xlim=c(min(ph$X)-1,max(ph$X)+1) );abline( lm(ph$Y~ph$X),col="red" )

par(c(1,2))
plot(ph$X,ph$Y)
plot(ph$X,ph$Y)

points(ph[,2],ph.lm(ph$X)-mean(ph[,1]) )


#d
ph.SSR/(ph.SSR+ph.MSE*(n.ph-2))
cor(ph$Y,ph$X)

#f
SSR/sum(as.data.frame(anova(lm(gpa$Y~gpa$X)))[,2])
cor(gpa$Y,gpa$X)^2




#3.3

#1.19
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


boxplot(gpa$X)
plot(gpa$ei,gpalm(gpa$X))
qqnorm(gpa$ei)

#d
gpa$eiRank<-rank(gpa$ei)
gpa$Eoei<-MSE*(qnorm(	(  (gpa$eiRank-.375) / (n.gpa+.25) )	))
cor(gpa$ei,gpa$Eoei)

gpa$ei[order(gpa$ei)]
rank(gpa$ei)
sort(gpa$ei)
cor(gpa$ei,gpa$Eoei)


###
toluca<-read.table("Toluca.txt")
colnames(toluca)<-c("X","Y")
toluca$Yhat<-as.numeric(lm(toluca$Y~toluca$X)[[1]][1]) +
	as.numeric(lm(toluca$Y~toluca$X)[[1]][2]) * toluca$X
toluca$ei<-toluca$Y-toluca$Yhat
toluca$eiRank<-rank(toluca$ei)

toluca.MSE<-sum(toluca$ei^2)/(nrow(toluca)-2)
toluca$Eoei<-sqrt(toluca.MSE)*(
	qnorm((  (toluca$eiRank-.375) / (nrow(toluca)+.25) )	))
cor(toluca$ei,toluca$Eoei)
qnorm(toluca$ei)

dotchart(toluca$X)

ggplot(gpa, aes(x = X)) + geom_dotplot( ) + theme_bw() 

stem(ab$ei) 

paste("e_",1:10,"=(" ,
	ab$Y,"-",round(ablm(ab$X),4), ")", " = " ,round(ab$ei,4),sep="")

plot(ab$X,ab$ei)



########### 3.14


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
anova(lm(ph$Y~ph$X))
ph$ei<-(ph$Y-ph.lm(ph$X) )
paste("(",ph$Y,"-",ph.lm(ph$X),")+",sep="")

meanYj<-rep(0,length(unique(ab$X)))

for (i in 1:length(unique(ph$X))){
	meanYj[i]<-mean(ph[which(ph[,2] == unique(ph$X)[i]),1]) }



(ph[which(ph[,2] == unique(ph$X)[i]),1]) 


SSPE<-data.frame(ph[,1],ph[,2],sort(rep(meanYj,4)))
paste("(",SSPE[,1],"-",SSPE[,3],")^2+",sep="")

SSPE$SPE<-(SSPE[,1]-SSPE[,3])^2
sum(SSPE$SPE)
