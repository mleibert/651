

#Visually we see that there is some strong correlation between the X variables
#which is confirmed by the Large coefficients of simple correlation between 
#pairs of predictor variables in the correlation matrix rxx. These are 
#signs that there is multicollinearity among the X variables.

rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999) 

 
setwd("G:/math/651")

ps<-read.table("patientsatisfaction.txt")
head(ps)
colnames(ps)<-c("Y","X1","X2","X3" )

pairs(ps[,-1])
cor(ps[,-1])

(1- summary(lm(ps$X1~ps$X2+ps$X3))$r.squared)^(-1)
(1- summary(lm(ps$X2~ps$X1+ps$X3))$r.squared)^(-1)
(1- summary(lm(ps$X3~ps$X1+ps$X2))$r.squared)^(-1)


#now that we look at the VIF values none of them are considerably larger
#than 1. This is indicative of low multicollinearity issues.



pairs(cp[,-1])
cor(cp[,-1])

#Looking at the correlation matrix and paris plot, it looks like there could
#be some multicollinearity issues, but even if there was my guess would be 
#that the multicollinearity is very weak.

(1- summary(lm(cp$X1~cp$X2+cp$X3+cp$X4))$r.squared)^(-1)
(1- summary(lm(cp$X2~cp$X1+cp$X3+cp$X4))$r.squared)^(-1)
(1- summary(lm(cp$X3~cp$X2+cp$X1+cp$X4))$r.squared)^(-1)
(1- summary(lm(cp$X4~cp$X2+cp$X3+cp$X1))$r.squared)^(-1)

#looking at the VIF we confirm the guess; all the VIF are barely over 1.


####


rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

ms<-read.table("MachineSpeed.txt")
colnames(ms)<-c("Y","X1" )

summary(lm(ms$Y~ms$X))$residuals
plot(ms$X,summary(lm(ms$Y~ms$X))$residuals)

(anova(lm(summary(lm(ms$Y~ms$X))$residuals^2~ ms$X))[[2]][1]/2)/
(anova(lm(ms$Y~ms$X))[[2]][2] / nrow(ms)	)^2

 qchisq(1-.1,1)

pchisq(1.660443,1)

plot(ms$X,(summary(lm(ms$Y~ms$X))$residuals)^2)

ms.ee<-summary(lm(ms$Y~ms$X))$residuals^2
 lm(ms.ee ~ms$X)

 summary(lm(ms.ee ~ms$X)) 

1/ (  -180.0833 +   1.2437 *ms$X )

ms.X<-matrix(ms$X,length(ms$X),2);ms.X[,1]<-1
ms.Y<-matrix(ms$Y,length(ms$Y),1)
ms.W<-matrix(0,length(ms$Y),length(ms$Y))
diag(ms.W)<-1/ (  -180.0833 +   1.2437 *ms$X )
solve(t(ms.X)%*%ms.W %*% ms.X)%*%t(ms.X)%*%ms.W%*%ms.Y


####

ps.rxx<-t(ps.X) %*% ps.X
ps.ryx<-t(ps.X) %*% ps.Y

C<-c(.000, .005, .01, .02, .03, .04, .05)

ps.bR<-list()
ps.R2<-list()

for ( i in 1:length(C)){
	ps.bR[[i]]<-solve(ps.rxx+C[i]*diag(dim( ps.rxx )[1] )) %*% ps.ryx

	SSR<-t(ps.Y )%*% ps.Y - t(ps.bR[[i]])  %*% t(ps.X ) %*% ps.Y 
	SSTO<-t(ps.Y )%*% ps.Y - (1/ps.n)*t(ps.Y )%*% ps.J%*% ps.Y
	ps.R2[[i]]<-1-(SSR/SSTO)
 }



