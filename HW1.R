rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

#1.19
gpa<-read.table("gradepointaverage.txt")
head(gpa)
colnames(gpa)<-c("Y","X")

#a
b1<-sum((gpa$X-mean(gpa$X))*(gpa$Y-mean(gpa$Y)))/sum( (gpa$X-mean(gpa$X))^2 )
b1
b0<- mean(gpa$Y)-b1*mean(gpa$X)
b0 
as.numeric(lm( gpa$Y ~ gpa$X)[[1]][1])

#b
plot(gpa$X,gpa$Y)
abline(lm( gpa$Y ~ gpa$X), col="red")

#It is apparent that there is a relationship between ACT and GPA in the plot. 
#I do not think I would say the regression line fits the data well.

#c
b0+b1*30

#d
b1

#1.21
ab<-read.table("AirfreightBreakage.txt")
head(ab)
colnames(ab)<-c("Y","X")

#a
lm( ab$Y ~ ab$X)
#Y = 4x + 10.2
plot(ab$X,ab$Y)
abline(lm( ab$Y ~ ab$X), col="red")
#The linear regression function appears to be a good fit.

#b
4*1 + 10.2

#c
(4*2 + 10.2)-(4*1 + 10.2)

#d
4*mean(ab$X) + 10.2
mean(ab$Y)






#1.23
#a
gpa$ei<- (gpa$Y - (b1* gpa$X + b0 ) )
sum(gpa$ei)		#Yes to 1.17

#b
gpa$SE<-gpa$residuals^2
MSEgpa<-sum(gpa$SE)/(nrow(gpa)-2)
MSEgpa
sqrt(MSEgpa)	#GPA score




#1.25	4x + 10.2
#a
ab$residuals<- (ab$Y - (4* ab$X + 10.2 ) )
ab$residuals[1]	#the error (e) involves the vertical derivation of Y_i from
			#the unknown true regression line and hence is unknown.
			#The residual (e) is the vertical derivation of Y_i from
			#the fitted value Ybar, on the estimated regression line 
			#and is known.

#b
ablm <- function(X ){  4*X+10.2 }
ab$Ybar<-ablm(ab$X)
ab$ei<-ab$Y-ab$Ybar
ab$eiei<-(ab$Y-ab$Ybar)^2
sum(ab$eiei)/(nrow(ab)-2)

paste0(ab$Y,"-",ab$Ybar)

sum(ab$SSE)
anova(lm( ab$Y ~ ab$X))
sqrt(sum(ab$SSE))	#MSE is an unbiased estimator of s^2 

#2.15
n=nrow(ab)
ab$Yhat<-4*ab$X + 10.2
Xh=2
sqrt( ( sum(ab$residuals^2)/(n-2) )* (1/n + ( (Xh-mean(ab$X))^2 /
 ( sum( (ab$X-mean(ab$X))^2 ) ) ) ) )

4*2 + 10.2 -3.355* ( sqrt(.44))
4*2 + 10.2 +3.355* ( sqrt(.44))














