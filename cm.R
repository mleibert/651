rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

cm<-read.table("CopierMaintenance.txt")
names(cm)<-c("Y","X")
tail(cm)

lm(cm$Y~cm$X)
cm.lm<-function(x){ -0.5802  +  15.0352    * x }

cm$Yhat<-cm.lm(cm$X)
cm$e<-cm$Y-cm$Yhat
cm.n<-nrow(cm)

cm.mse<-sum((cm$e^2))/(cm.n-2)

cm.lm(6)-sqrt(cm.mse*((1/cm.n) + ((6-mean(cm$X))^2 /
	 sum( (cm$X-mean(cm$X))^2 ) ) )) *	qt(1-.1/2,cm.n - 2)

cm.lm(6)+sqrt(cm.mse*((1/cm.n) + ((6-mean(cm$X))^2 /
	 sum( (cm$X-mean(cm$X))^2 ) ) )) *	qt(1-.1/2,cm.n - 2)



cm.c<-length(unique(cm$X))


SPE<-rep(NA,cm.c)

for ( i in 1:cm.c){
	ybar<-mean(cm[ which( cm$X == unique(cm$X)[i] ) ,]$Y )
	SPE[i]<-sum( (cm[ which( cm$X == unique(cm$X)[i] ) ,]$Y - ybar)^2 )}

cm.sse<-cm.mse*(cm.n-2)

(cm.sse-sum(SPE))/((cm.n-2)-(cm.n-cm.c) ) / (  sum(SPE) / (cm.n-cm.c) ) <

qf(1-.05,cm.c-2,cm.n-cm.c)



cm.B<-qt(1-(.05/4),cm.n-2);cm.B

cm.b0<-  -0.5802   
cm.b1<-  15.0352 


sqrt(cm.mse*((1/cm.n) + ((mean(cm$X)^2)/sum((cm$X-mean(cm$X))^2) ) ))
sqrt(cm.mse/ ( sum((cm$X-mean(cm$X))^2) ) )

cm.b0-cm.B*sqrt(cm.mse*((1/cm.n) +
	 ((mean(cm$X)^2)/sum((cm$X-mean(cm$X))^2) ) ))
cm.b0+cm.B*sqrt(cm.mse*((1/cm.n) +
	 ((mean(cm$X)^2)/sum((cm$X-mean(cm$X))^2) ) ))

cm.b1-cm.B*sqrt(cm.mse/ ( sum((cm$X-mean(cm$X))^2) ) )
cm.b1+cm.B*sqrt(cm.mse/ ( sum((cm$X-mean(cm$X))^2) ) )


