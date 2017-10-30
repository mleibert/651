rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

gpa<-read.table("GradePointAverage.txt")
gpa$X3<-scan("GradePointAverageX2.txt")
head(gpa)
colnames(gpa)<-c("Y","X1","X2"  )
gpa.n<-nrow(gpa)


gpa$X1X2<-gpa$X2*gpa$X1

lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 ) 

summary(lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 ))

gpa.t<-as.numeric(lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 )[[1]][4] )  /
		as.numeric(summary(lm(gpa$Y~gpa$X1+gpa$X2+gpa$X1X2 ))[[4]][ 4,2])
gpa.t

gpa.alpha<-(1-.95)/2

qt(1-gpa.alpha,gpa.n-3)

abs(gpa.t) < qt(1-gpa.alpha,gpa.n-3)
#reject H0
