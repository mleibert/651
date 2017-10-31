su<-read.table("surgical.txt")
tail(su)
colnames(su)<-c( paste0("X",1:8),"Y","Y`" )

su<-su[,c( (ncol(su)-1):ncol(su), 1:(ncol(su)-2))  ]
 tail(su)

su<-su[1:54,c(1:6)]

su<-su[2:ncol(su)]
 tail(su)
names(su)[1]<-"Y"

cor(su, use="complete.obs")

 
 tail(su)


R2ap<-vector(mode="numeric", length=0)
counts<-vector(mode="numeric", length=0)
su.aic<-vector(mode="numeric", length=0)
su.cp<-vector(mode="numeric", length=0)
su.press<-vector(mode="numeric", length=0)

su.p<-ncol(su)
su.n<-nrow(su)

for(i in 1:4){
	VIM<-combn( names(su)[-1],i) 
		for(j in 1:ncol(VIM) ){
			level<-VIM[,j]
			level<-paste("su$",level,sep="")
			level<-paste(level, collapse = '+')
			level<-paste0("su$Y~",level)
			
			R2ap<-c(R2ap,summary( lm(  level ) )$adj.r.squared)
			counts<-c(cp,i+1)	
	
	su.cp<-c(su.cp, (-(su.n-2*(i+1)))+anova(lm(level))[i+1,2]	/
	(anova(lm(su$Y~su$X1+su$X2+su$X3+su$X4))[su.p,2] / (su.n-su.p))  )
	
	su.aic<-c(su.aic,su.n*log(anova(lm(level))[i+1,2])-su.n*log(su.n)+
		2*(i+1))
	
	su.press<-c(su.press,PRESS( lm(level) ))
	
			}}


plot(cp,R2ap)
 
blerg<- leaps(y=su$Y,x=su[,ncol(su):2])[[1]] 
paste0("X",which(blerg[5,]==T),collapse = ',')


anova(lm(su$Y~su$X4))[2,2]/
(anova(lm(su$Y~su$X4+su$X3+su$X2+su$X1))[5,2] / (nrow(su)-5) )-
(nrow(su)-2*2)

su.lm<-lm(su$Y ~su$X1 )


step(null, scope=list(lower=null, upper=full), direction="forward")


su$Y<-su[,ncol(su)]
tail(su)
su<-su[,-ncol(su)]

  null= lm(Y ~ 1, data=su)
full <- (lm(Y~.,su))
 step(null, scope=list(lower=null, upper=full), direction="forward")

#w
