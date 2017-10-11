A=matrix(0,4,2)
A[,1]<-c(2,3,5,4)
A[,2]<-c(1,5,7,8)
A

B=matrix(0,4,1)
B[,1]<-c(6,3,9,1)

C=matrix(0,4,2)
C[,1]<-c(3,8,5,2)
C[,2]<-c(8,6,1,4)
C


A-C

t(B)%*%  A

A%*%C
t(C)%*%  A

pmatrix(t(C)%*%  A,digits=0)


##  5.15

mat<-matrix(c(5,2,23,7),2,2,byrow=T);mat
solve(mat,c(8,28))


#####

x<-c(4, 1, 2, 3, 3, 4)
Y<-c(16, 5, 10, 15, 13, 22)
X<-matrix(0,length(x),2);X[,1]<-1;X[,2]<-x;X
Y<-as.matrix(Y)
n=6
solve(t(X)%*%X)%*%t(X)%*%Y
b<-solve(t(X)%*%X)%*%t(X)%*%Y;b
e<-Y-X%*%b;e


J<-matrix(rep(1,6^2),6,6)
t(b)%*%t(X)%*%Y - ((1/n)*t(Y))%*%J%*%Y
t(Y) %*% Y - t(b) %*% t(X) %*% Y

var(t(X)%*%X)
MSE<-sum(e^2)/(n-2)
denom<-sum((x-mean(x))^2);var(x)*(n-1)

matrix(c((MSE/n) + ( (mean(x)^2 * MSE) / denom),
rep((-mean(x)*MSE)/denom,2), (MSE)/denom),2,2,byrow=T)

 solve(t(X)%*%X

Xh<-matrix(c(1,4),2,1)
MSE*(1+t(Xh)%*% solve(t(X)%*%X) %*%Xh)


H<-X%*%solve(t(X)%*%X)%*%t(X);H
H%*%H

MSE*()

