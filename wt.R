rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

wt<-read.table("CH07TA01.txt")
head(wt)
colnames(wt)<-c("X1","X2","X3","Y" )

anova(lm(wt$Y ~ wt$X2+wt$X1+wt$X3 ))	
anova(lm(wt$Y ~ wt$X1 ))
33.17/143.12

anova(lm(wt$Y ~ wt$X3+wt$X1 +wt$X2))	



anova(lm(wt$Y ~ wt$X2+wt$X1 ))	
anova(lm(wt$Y ~ wt$X2 ))
 
(33.17+352.27)-352.27
 

	permz<-permutations(n=3,r=3,v=c(1,2,3),repeats.allowed=F)
	sumz<-rep(NA,6)
for( i in 1:6){
	a<-paste("wt$Y ~ ", "wt$X",permz[i,1] ,"+wt$X",permz[i,2] ,
		"+wt$X",permz[i,3],	sep = "")
	sumz[i]<-sum(anova(lm(a ))[[2]][1:3])
}







