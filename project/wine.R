
WINE<-read.csv("wine.csv")

wine<-WINE
wine<-wine[complete.cases(wine), ]

wine<-wine[,-c(1,3)]
type<-names(tail(sort(table(wine$variety)),15))


wine<-wine[which(wine$variety %in% type),]
head(wine)
nrow(wine)


country<-names(tail(sort(table(wine$country)),11))
wine<-wine[which(wine$country %in% country ),]
nrow(wine)
table(wine$country)

for(i in names(Filter(is.factor, wine))[-length(names(Filter(is.factor,wine)))]	){
	wine[,which(names(wine) == i )]<-
		as.character(wine[,which(names(wine) == i )])
	wine[,which(names(wine) == i )]<-
		as.factor(wine[,which(names(wine) == i )])
}


head(wine)
wine$ID<-1:nrow(wine)

pps<-as.data.frame(table(wine$country))
pps$percent<-pps$Freq/sum(pps$Freq)
pps$n<-round(5000*pps$percent,0)
 
winelist<-list()

for( i in 1:nrow(pps)){
	winelist[[i]]<-wine[which(wine$country == pps$Var1[i]),]
	winelist[[i]]<-winelist[[i]][sample(nrow(winelist[[i]]), pps$n[i]), ]
}


  winelist<- do.call("rbind", winelist)


head(winelist)
table(winelist$country)
summary(lm(winelist$points~winelist$price + winelist$variety +
	 winelist$country ))

cor(	winelist[,4 ]	)

max(winelist$price)
winelist[which(winelist$price > 1000) ,]
unique(winelist$variety)
summary(lm(points~country+variety+price,data=winelist))

hist(wine[which(wine$variety == "Red Blend"),3])

ggplot(wine, aes(points, fill = variety)) +
  geom_histogram( bins= 50)


par(mfrow=c(3,5))
wines<-list()

wines[[16]]<-data.frame(variety=character(),      mean=numeric() )


for(i in 1:length(unique(wine$variety))	){
	hist(wine[which(wine$variety == unique(wine$variety)[i]  ),3],
	main = paste(i,".",unique(wine$variety)[i]),xlab="Points",ylab="Freq")
	wines[[i]]<-data.frame(	names(summary(wine[which(wine$variety == 
				unique(wine$variety)[i]  ),3])),
				as.vector(summary(wine[which(wine$variety == 
				unique(wine$variety)[i]  ),3])) )
	colnames(wines[[i]])<-c("summary","points") 
	wines[[16]]<-rbind(wines[[16]],
		data.frame(as.character(unique(wine$variety)[i]),
		wines[[i]][4,2]	))
}

	colnames(wines[[16]])<-c("variety","mean") 


newdata <- wines[[16]][order( wines[[16]]$mean),]

summary(aov(points~variety,data=wine))




