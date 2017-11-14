
rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999) 

 
setwd("G:/math/651")

bf<-read.table("bodyfat.txt",header=F)
head(bf)
 
colnames(bf)<-c("Y","X1","X2","X3"  )

pairs(bf[,-1])
cor(bf[,-1])

lm(bf[,1]~bf[,2]+bf[,3]+bf[,4]+bf[,5])

#resids
bf.n<-nrow(bf)
bf.Y<-as.matrix(bf[,1])
bf.X<-as.matrix( data.frame(1,bf[,2:ncol(bf)]) )
bf.H<-bf.X %*% solve(t(bf.X)%*%bf.X) %*% t(bf.X)
bf.ee<-(diag(bf.n)-bf.H) %*% bf.Y
as.vector(bf.ee)
bf.p<-dim(bf.X)[2]


boxplot(bf.ee)

 ( (  lm(bf[,1]~bf[,2]+bf[,3]+bf[,4] ) ))

bf.J<-matrix(1,bf.n,bf.n)



bf.rxx<-t(bf.X) %*% bf.X
bf.ryx<-t(bf.X) %*% bf.Y

bf.rxx
solve(t(bf.X) %*% bf.X)  %*% t(bf.X) %*% bf.Y



bf.bR<-list()
bf.R2<-list()

C<-seq(0,.01,.002)



for ( i in 1:length(C)){
	bf.bR[[i]]<-solve(bf.rxx+C[i]*diag(dim( bf.rxx )[1] )) %*% bf.ryx

	SSR<-t(bf.Y )%*% bf.Y - t(bf.bR[[i]])  %*% t(bf.X ) %*% bf.Y 
	SSTO<-t(bf.Y )%*% bf.Y - (1/bf.n)*t(bf.Y )%*% bf.J%*% bf.Y
	bf.R2[[i]]<-1-(SSR/SSTO)
 }



solve(bf.rxx+ C[2]*diag(dim(bf.rxx)[1] ))  %*% bf.rxx  %*% 
solve(bf.rxx+ C[2]*diag(dim(bf.rxx)[1] )) 