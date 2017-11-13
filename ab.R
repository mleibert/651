setwd("G:/math/651")

ab<-read.table("AirfreightBreakage.txt")
colnames(ab)<-c("Y","X")
lm(ab$Y ~ ab$X)

ab.b1<-  sum( 	(ab$X-mean(ab$X))*(ab$Y-mean(ab$Y))	  ) / 
		sum( 	(ab$X-mean(ab$X))*(ab$X-mean(ab$X))	  );ab.b1

ab.b0<-mean(ab$Y) - ab.b1*mean(ab$X);ab.b0

ab.lm<-function(x){ab.b0 + ab.b1 *x }

ab$Yhat<-ab.lm(ab$X)
ab$e<-ab$Y-ab$Yhat

ab.mse<-sum(ab$e^2)/8;ab.mse

ab.ss<-ab.mse/sum((ab$X-mean(ab$X))^2)

ab.b1-qt(1-(.05/2),8)*sqrt(ab.ss)
ab.b1+qt(1-(.05/2),8)*sqrt(ab.ss)

ab.b1/sqrt(ab.ss) < 2.3

2*(1-pt(ab.b1/sqrt(ab.ss),8))

ab.b0-qt(1-(.05/2),8)*(.6633)
ab.b0+qt(1-(.05/2),8)*(.6633)

qt(1-(.025/1),8)

(ab.b0-9)/.6633
 
(1-pt((ab.b0-9)/.6633,8))


ab.lm(2);ab.lm(4)
qt(1-(.01/2),8)

ab.n<-nrow(ab) 

ab.lm(2)-qt(1-(.01/2),8)*sqrt(ab.mse*(1/10   + 1/10))
ab.lm(2)+qt(1-(.01/2),8)*sqrt(ab.mse*(1/10  + 1/10))


ab.lm(4)-qt(1-(.01/2),8)*sqrt(ab.mse*(1/10   + 9/10))
ab.lm(4)+qt(1-(.01/2),8)*sqrt(ab.mse*(1/10  + 9/10) )


ab.lm(2)-qt(1-(.01/2),8)*sqrt(ab.mse*(1/10 +1  + 1/10))
ab.lm(2)+qt(1-(.01/2),8)*sqrt(ab.mse*(1/10 +1  + 1/10))


ab.lm(2)-qt(1-(.01/2),8)*sqrt(ab.mse*(1/10 + 1/3  + 1/10))
ab.lm(2)+qt(1-(.01/2),8)*sqrt(ab.mse*(1/10 +1/3   + 1/10))

ab.Xh<-
varYh.ab<-function(Xh){
	ab.mse*((1/ab.n)+((Xh-mean(ab$X))^2)/(sum((ab$X-mean(ab$X))^2)) )}

varYh.ab(2)

ab.lm(2)- sqrt(varYh.ab(2)) * sqrt( 2* qf(1-.01,2,ab.n-2) )  
ab.lm(2)+ sqrt(varYh.ab(2)) * sqrt( 2* qf(1-.01,2,ab.n-2) )  


ab.lm(4)- sqrt(varYh.ab(4)) * sqrt( 2* qf(1-.01,2,ab.n-2) )  
ab.lm(4)+ sqrt(varYh.ab(4)) * sqrt( 2* qf(1-.01,2,ab.n-2) )  

anova(lm(ab$Y~ab$X))

ab.n<-nrow(ab)
Fs<-(sum(( ab.lm(ab$X) - mean(ab$Y))^2 ))/(ab.mse)


Fs<qf(1-.05,1,ab.n-2)

1-pf(Fs,1,ab.n-2)

ab.b1/sqrt((ab.mse/(sum((ab$X-mean(ab$X))^2))))*
ab.b1/sqrt((ab.mse/(sum((ab$X-mean(ab$X))^2))))

ab.msr<-sum( (ab.lm(ab$X) - mean(ab$Y))^2 )
ab.msr/(ab.msr + ab.mse*(ab.n-2))
summary(lm(ab$Y~ab$X))




