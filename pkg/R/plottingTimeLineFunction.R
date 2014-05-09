plottingTimeLineFunction <- function(AllData,finalMQQC,TargetVec = "ECstd"){

#AllData <- backup
uniSample <- AllData[grep(TargetVec, AllData$Name),]
UniMachine <- strsplit(as.character(uniSample$Name),"_")
UniMachine <- sapply(UniMachine,function(x){x[1]})

Vec.Test <- c("quan.msms.min","msmsMassCount.50%","msmsQuantile.50%","score.50%","mass.error.cal.50%")
Names <- c("Peptide ID / min","Fragment Counts / MSMS","log10 MSMS Median Intensity","Andromeda Score","Mass Error")
ColMQQCTLPLOTTING <<- c()
log2Fun <- c("msmsQuantile.50.","msmsQuantile.25.","msmsQuantile.75.","msmsQuantile.100.","msmsQuantile.0.")
for(i in log2Fun){
	if(any(make.names(colnames(AllData)) == make.names(i))){
		AllData[,make.names(colnames(AllData)) == i] <- log10(AllData[,make.names(colnames(AllData)) == i] )
	}
}

lapply(make.names(Vec.Test),function(x){
	tempVal <- as.character(tolower(colnames(uniSample)))
	tempVal[is.na(tempVal)] <- "NA"
	tempVal <-make.names(tempVal)	 == make.names(tolower(x))
	test<- aggregate(uniSample[,tempVal],list(UniMachine),max,na.rm = T)
	ColMQQCTLPLOTTING <<- 	cbind(ColMQQCTLPLOTTING,test[,2])
	rownames(ColMQQCTLPLOTTING) <<- test[,1]
	
	return(NULL)
})
ColMQQCTLPLOTTING[is.infinite(ColMQQCTLPLOTTING)] <- 0.1
ColMQQCTLPLOTTING <<- ColMQQCTLPLOTTING
MaxV<- apply(ColMQQCTLPLOTTING,2,max,na.rm = T)
names(MaxV) <- Vec.Test
colnames(ColMQQCTLPLOTTING) <- Vec.Test
for(i in unique(UniMachine)){
	dir.create(paste(finalMQQC ,"TimeLines",sep = "/"))
	pdf(pdfName <- paste(finalMQQC ,"TimeLines",paste(i,"TimeLine.pdf",sep = "-"),sep = "/"),height = 3.5,pointsize = 10,width = 17)
	par(mai = c(1.3,0.7,0.6,0.1),mfrow = c(1,length(Vec.Test)))
	
it.a <- 1	
for(a in Vec.Test){
	
	MacLine <- ColMQQCTLPLOTTING[rownames(ColMQQCTLPLOTTING) == i,make.names(colnames(ColMQQCTLPLOTTING)) == make.names(a)]
	tempI <- uniSample[UniMachine == i,]
	colnames(tempI)[is.na(colnames(tempI))]<- "NA"
	
	tempI <- tempI[order(tempI$System.Time.s),]
	TimeI <- tempI$System.Time	
	BestV <- MaxV[make.names(names(MaxV)) == make.names( a)] 	
	TimeI <- strsplit(as.character(TimeI),".",fixed =T)
	TimeI <- sapply(TimeI,function(x){
		x1 <- paste(x[1:3],collapse = "-")
		x2 <- paste(x[4:6],collapse = ":")
		x <- c(x1,x2)
		return(x)
	})
	if(length(grep(".50%",a))> 0){
	CountUp <- tempI[,make.names(colnames(tempI)) == make.names(gsub(".50%",".75%",a))]
	CountLo <- tempI[, make.names(colnames(tempI)) == make.names(gsub(".50%",".25%",a))]
	BordPlot <- T

	}else{BordPlot <- F}
	
	TimeI <- t(TimeI)
	TimeI <- gsub("^X","", TimeI)
	deleteTimeVec <- duplicated(substr(TimeI[,1],1,7))
	Count <- tempI[,make.names(colnames(tempI)) == make.names(a)]
	
	y <- as.numeric(as.character(Count))
	x <- as.numeric(as.character(tempI$System.Time.s))
 
 
  
  if(all(is.na(y))){y[is.na(y)] <- 0;Count[is.na(Count)] <- 0}
	if(length(x) == length(y) & length(y) > 0 & any(!is.na(Count))){
		
	if(BordPlot){
		rangeVal <- range(c(Count,CountUp,CountLo),na.rm = T)
		
	}else{
		rangeVal <- range(c(Count),na.rm = T)

	}
	rangeVal[1] <- 0
if(!is.na(BestV)){
	if(rangeVal[2] < BestV)
	rangeVal[2] <- BestV

}

xYear <- x > (31556926-x)	
x <- x[xYear]
y <- y[xYear]
	

plot(x, y,axes = F,type = "n",lwd = 3,xlab = "",ylab = Names[it.a],main = paste(i, Names[it.a],sep = "\n"),cex.lab = 2,ylim = rangeVal,xlim = c(min(x),as.numeric(Sys.time())))
grid()

if(it.a == 1){legend("bottomright",legend = c("All",i),fill = c("tomato","turquoise"),bty = "n",title = "Best value")}
it.a <- it.a+1 # Counter for Names

	if(BordPlot){

	#lines(x,CountUp,type = "l",lty = "dashed",lwd = 1,col = "red")
	#lines(x,CountLo,type = "l",lty = "dashed",lwd = 1,col = "red")
polygon(c(x, rev(x)), c(CountUp, rev(CountLo)),
     col = "grey80", border = NA)
     }
  
       lines(x,y,lwd =4,col = "grey30")
	time <- as.numeric(as.character(tempI$System.Time.s))[!deleteTimeVec]
  label <- substr(TimeI[! deleteTimeVec,1],1,10)
  if(as.numeric(Sys.time())-max(time) > 86400){
    time <- c(time,as.numeric(Sys.time()))
    label <- c(label,"today")  
  }
  
	axis(1,time,label = label,las = 2,cex.axis = 2)
	axis(2,cex.axis = 2)
	
	if(!is.na(BestV)){
		abline(h = BestV,col = "tomato",lty = "1414",lwd = 2)
	}
	if(length(MacLine) > 0){
		abline(h = MacLine,col = "turquoise",lty = "4949",lwd = 2)
	}
	}
 points(lowess(x,y),type = "l",col = "orange",lwd = 2)

}
	graphics.off()
#system(paste("open", pdfName))
#stop()
}
}

#plottingTimeLineFunction(AllData,finalMQQC)
