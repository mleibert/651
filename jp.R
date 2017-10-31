rm(list=ls())
jp<-read.table("JobProficiency.txt")
colnames(jp)<-c("Y","X1","X2","X3","X4" )
tail(jp)

stem(jp$X1)
stem(jp$X2)
stem(jp$X3)
stem(jp$X4)

pairs( jp[] )
cor(jp[,2:ncol(jp)])
#The third and fourth test appear to be very linearly correlated.

lm(jp$Y~jp$X1+jp$X2+jp$X3+jp$X4)


#9.11

jp.R2ap<-vector(mode="numeric", length=0)
jp.counts<-vector(mode="numeric", length=0)
jp.aic<-vector(mode="numeric", length=0)
jp.cp<-vector(mode="numeric", length=0)
jp.press<-vector(mode="numeric", length=0)
jp.variables<-vector(mode="character", length=0)
jp.sses<-vector(mode="numeric", length=0)
jp.mses<-vector(mode="numeric", length=0)


jp.p<-ncol(jp)
jp.n<-nrow(jp)

for(i in 1:(jp.p-1)  ){
	VIM<-combn( names(jp)[-1],i) 
		for(j in 1:ncol(VIM) ){
			level<-VIM[,j]
			jp.variables<-c(jp.variables,paste(level, collapse = ','))
			level<-paste("jp$",level,sep="")
			level<-paste(level, collapse = '+')
			level<-paste0("jp$Y~",level)
			
			jp.R2ap<-c(jp.R2ap,summary( lm(  level ) )$adj.r.squared)
			 
			jp.counts<-c(jp.counts,i+1)	
	
	jp.cp<-c(jp.cp, (-(jp.n-2*(i+1)))+anova(lm(level))[i+1,2]	/
	( anova( lm(Y~.,jp)	)[jp.p,2] / (jp.n-jp.p) )  )
	
	jp.aic<-c(jp.aic,jp.n*log(anova(lm(level))[i+1,2])-jp.n*log(jp.n)+
		2*(i+1))
	
	jp.press<-c(jp.press,PRESS( lm(level) ))
	jp.sses<-c(jp.sses,	anova(lm(level))[i+1,2]	)
	jp.mses<-c(jp.mses,anova(lm(level))[i+1,3]	)	}}


jp.crit<-data.frame(jp.counts,jp.variables,jp.R2ap, jp.aic, jp.cp,jp.press)
 

null= lm(Y ~ 1, data=jp)
full <- (	lm(Y~.,jp)	)
step(null, scope=list(lower=null, upper=full), direction="forward")



