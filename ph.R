rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

ph<-read.table("plastichardness.txt")
names(ph)<-c("Y","X")
tail(ph)

ph.n<-nrow(ph)
lm(ph$Y~ph$X)
ph.lm<-function(x) {    168.600    +    2.034  *x}
ph.msr<-sum( ( ph.lm(ph$X) -  mean(ph$Y))^2 )
ph.mse<-sum( ( ph$Y -ph.lm(ph$X)   )^2 )/(ph.n-2)
ph.sse<-ph.mse*(ph.n-2)
ph.ssto<-ph.sse+ph.msr

ph.F<- ph.msr/ph.mse
ph.F

anova(lm(ph$Y~ph$X))

qf(1-.01,1,ph.n-2)
ph.F<qf(1-.01,1,ph.n)

1-pf(ph.F,1,ph.n-2)

ph.sse/ph.ssto 
ph.msr/ph.ssto

ph.msr-ph.sse
SPE <-rep(NA,length(unique(ph$X)))

for(i in 1:length(unique(ph$X)) ){
	TY<-mean(ph[which( ph$X == unique(ph$X)[i] ),]$Y)
	SPE[i]<-sum(((ph[which( ph$X == unique(ph$X)[i] ),]$Y)-TY)^2)
}

ph.c<-length(unique(ph$X))


((ph.sse - sum(SPE) ) / ((ph.n-2)-(ph.n-ph.c)) ) / (sum(SPE) / (ph.n-ph.c))<

qf(1-.01,ph.c-2,ph.n-ph.c) 


ph.B<-qt(1-(.1/4),ph.n-2)
lm(ph$Y~ph$X)

 168.600 - sqrt(ph.mse*( (1/ph.n) + (mean(ph$X)^2) / 
	(sum((ph$X-mean(ph$X))^2)) ))*ph.B
 168.600 +sqrt(ph.mse*( (1/ph.n) + (mean(ph$X)^2) / 
	(sum((ph$X-mean(ph$X))^2)) ))*ph.B


 2.034  - sqrt(ph.mse / sum((ph$X-mean(ph$X))^2) ) * ph.B
 2.034  + sqrt(ph.mse / sum((ph$X-mean(ph$X))^2) ) * ph.B

