

prog<-read.table("CH14TA01.txt",header=F)
prog<-prog[,-3]
names(prog)<-c("X","Y")
head(prog)

lm.prog<-glm(prog$Y~prog$X,fam="binomial")

summary(lm.prog)


disease<-read.table("CH14TA03.txt",header=F)
disease<-disease[,-1]
head(disease)

names(disease)<-c(paste0("X",1:4),"Y")
head(disease)
disease.lm<-glm(Y~.,data=disease,family="binomial")
summary(disease.lm)
