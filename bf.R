
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



head(bf)


sd(bf$X1)
sd(bf$X2)
sd(bf$X3)

bflist<-list()
for (i in 1:ncol(bf) ){
	bflist[[i]]<-(1/( sqrt(bf.n - 1)))*((bf[,i ] -mean(bf[,i ]) ) /
		 sd(bf[,i ]) )}

bft<-data.frame( bflist[[1]], bflist[[2]],bflist[[3]],bflist[[4]])

colnames(bft)<-c("Y","X1","X2","X3"  )

#resids
bft.n<-nrow(bft)
bft.Y<-as.matrix(bft[,1])
bft.X<-as.matrix( data.frame(bft[,2:ncol(bft)]) )
bft.H<-bft.X %*% solve(t(bft.X)%*%bft.X) %*% t(bft.X)
bft.ee<-(diag(bft.n)-bft.H) %*% bft.Y
bft.p<-dim(bft.X)[2]

bft.rxx<-t(bft.X) %*% bft.X
bft.ryx<-t(bft.X) %*% bft.Y


solve(bft.rxx +1*diag( dim(bft.rxx)[1] ) ) %*% bft.ryx

