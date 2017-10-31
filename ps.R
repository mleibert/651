rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999) 

 
setwd("G:/math/651")

ps<-read.table("patientsatisfaction.txt")
head(ps)
colnames(ps)<-c("Y","X1","X2","X3" )

pairs(ps)
cor(ps[,-1])

lm(ps[,1]~ps[,2]+ps[,3]+ps[,4]+ps[,5])

#resids
ps.n<-nrow(ps)
ps.Y<-as.matrix(ps[,1])
ps.X<-as.matrix( data.frame(1,ps[,2:ncol(ps)]) )
ps.H<-ps.X %*% solve(t(ps.X)%*%ps.X) %*% t(ps.X)
ps.ee<-(diag(ps.n)-ps.H) %*% ps.Y
as.vector(ps.ee)
ps.p<-dim(ps.X)[2]


boxplot(ps.ee)

 (anova(  lm(ps[,1]~ps[,2]+ps[,3]+ps[,4] ) ))

ps.J<-matrix(1,ps.n,ps.n)
ps.MSR<-(t(ps.Y)%*%(ps.H-(1/ps.n)*ps.J)%*%ps.Y)/(ps.p-1)
ps.MSE<-t(ps.ee)%*%(ps.ee)/(ps.n-ps.p)

#t(ps.b) %*% t(ps.X) %*%ps.Y - (1/ps.n)*t(ps.Y)%*%ps.J%*%ps.Y


#ps.b<-matrix(as.numeric(lm(ps[,1]~ps[,2]+ps[,3]+ps[,4]+ps[,5]
#	)[[1]][1:5]),5,1)


ps.MSR/ps.MSE
ps.alpha<-.05
qf(1-ps.alpha,ps.p-1,ps.n-ps.p)

1-pf(  1-ps.alpha,3,ps.n-ps.p)

if( ps.MSR/ps.MSE > qf(1-ps.alpha,ps.p-1,ps.n-ps.p) ){print("conclude Ha")
	} else {print("conclude H0")}
#0.4878


#c
ps.b<- solve(t(ps.X)%*%ps.X)%*%(t(ps.X)%*%ps.Y)
ps.b
ps.sb<-as.numeric(diag(sqrt((as.numeric(ps.MSE)*solve(t(ps.X)%*%ps.X)))))
ps.sb
ps.g<-4

ps.B<-qt(  1-((ps.alpha)/(2*ps.g)) ,ps.n-ps.p)

ps.b[-1]-ps.sb[-1]*ps.B
ps.b[-1]+ps.sb[-1]*ps.B

summary ( (  lm(ps[,1]~ps[,2]+ps[,3]+ps[,4] ) ) )


(t(ps.Y)%*%(ps.H-(1/ps.n)*ps.J)%*%ps.Y)/
t(ps.Y)%*%(diag(ps.n)-(1/ps.n)*ps.J )%*%ps.Y

anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))

summary(lm(ps$Y~ps$X1+ps$X2+ps$X3))


#7.5b
#SSR(X3|X1,X2)
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))
#F*
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][3]/(
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][4]/(ps.n-4))




qf(1-.025,1,(ps.n-4))

anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][3]/(
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][4]/(ps.n-4))<qf(1-.025,1,(ps.n-4))
#Conclude H0

anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[5]][3]
#Conclude H0






#SSR(X2,X3|X1)
sum(anova(lm(ps$Y~ps$X1+ps$X2+ps$X3) )[[2]][2:3])

(sum(anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][2:3])/2)/(
	anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][4]/(ps.n-4))


#7.14
 summary(lm(ps$Y ~ ps$X1+ps$X2+ps$X3 )	)
 summary(lm(ps$Y ~ ps$X1  )	)

 anova(lm(ps$Y ~ ps$X2 +ps$X1 )	)[[2]][2]/
 anova(lm(ps$Y ~ ps$X2 )	)[[2]][2]
 

(sum( anova(lm(ps$Y ~ ps$X3 +ps$X1+ps$X2 ) ) [[2]][1:3]) -
sum( anova(lm(ps$Y ~  ps$X3+ps$X2 )	) [[2]][1:2])  )/
 ( anova(lm(ps$Y ~  ps$X3+ps$X2 )	) [[2]][3])  


 summary(lm(ps$Y ~ ps$X3 +ps$X1+ps$X2 ) ) 



influence.measures(lm(ps$Y ~ ps$X3 +ps$X1+ps$X2 ))


ps.R2ap<-vector(mode="numeric", length=0)
ps.counts<-vector(mode="numeric", length=0)
ps.aic<-vector(mode="numeric", length=0)
ps.cp<-vector(mode="numeric", length=0)
ps.press<-vector(mode="numeric", length=0)
ps.variables<-vector(mode="character", length=0)

ps.p<-ncol(ps)
ps.n<-nrow(ps)

for(i in 1:(ps.p-1)  ){
	VIM<-combn( names(ps)[-1],i) 
		for(j in 1:ncol(VIM) ){
			level<-VIM[,j]
			ps.variables<-c(ps.variables,paste(level, collapse = ','))
			level<-paste("ps$",level,sep="")
			level<-paste(level, collapse = '+')
			level<-paste0("ps$Y~",level)
			
			ps.R2ap<-c(ps.R2ap,summary( lm(  level ) )$adj.r.squared)
			ps.counts<-c(ps.counts,i+1)	
	
	ps.cp<-c(ps.cp, (-(ps.n-2*(i+1)))+anova(lm(level))[i+1,2]	/
	( anova( lm(ps$Y~ps$X1+ps$X2+ps$X3 ) )[ps.p,2] / (ps.n-ps.p))  )
	
	ps.aic<-c(ps.aic,ps.n*log(anova(lm(level))[i+1,2])-ps.n*log(ps.n)+
		2*(i+1))
	
	ps.press<-c(ps.press,PRESS( lm(level) ))
				}}

ps.crit<-data.frame(ps.counts,ps.variables,ps.R2ap, ps.aic, ps.cp,ps.press)
ps.crit[which(ps.crit$ps.R2ap > .6 ),]
ps.crit[which(	(ps.crit$ps.counts-2) < ps.crit$ps.cp &
			 ps.crit$ps.cp  < (ps.crit$ps.counts+2)	) , ]


plot(ps.crit[,1],ps.crit[,3])

   lines(unique(ps.crit[,1])[1:2], c(
	max( ps.crit[which(ps.crit[,1]== 2) , 3] ) ,
	max( ps.crit[which(ps.crit[,1]== 3) , 3] ) ))
    lines(unique(ps.crit[,1])[2:3], c(
	max( ps.crit[which(ps.crit[,1] == 3) , 3] ) ,
	max( ps.crit[which(ps.crit[,1] == 4) , 3] ) )	)



plot(ps.crit[,1],ps.crit[,4])
plot(ps.crit[,1],ps.crit[,5])
plot(ps.crit[,1],ps.crit[,6])


  null= lm(Y ~ 1, data=ps)
full <- (lm(Y~.,ps))
 step(null, scope=list(lower=null, upper=full), direction="forward")

#with only three X variables, I think the all-possible-regressions procedure
#would be computationally feasible, and I don't think forward stepwise
#regression has any advantages here


