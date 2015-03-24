plottingTimeLineFunction <- function(AllData,finalMQQC,TargetVec = "ECstd",PDF = F,lwdfac = 1, RESettings = RESettings, TLname = ""){

#AllData <- backup
uniSample <- AllData[grep(TargetVec, AllData$Name),]
test <- sub(RESettings$REmac,"", as.character(uniSample$Name))


UniMachine  <- grepRE(as.character(uniSample$Name),RESettings$REmac)

test <- sub(RESettings$REmac,"", UniMachine)


Vec.Test <- c("quan.msms.min","Intensity.50%","msmsMassCount.50%","mass.error.uncal.50%")
Names <- c("Peptide ID / min","MS Median Intensity","Fragment Counts / MSMS","uncalibrated Mass Error [ppm]")
ColMQQCTLPLOTTING <<- c()
log2Fun <- c(F,T,F,F)
# for(i in log2Fun){
# 	if(any(make.names(colnames(AllData)) == make.names(i))){
# 		#AllData[,make.names(colnames(AllData)) == i] <- log10(AllData[,make.names(colnames(AllData)) == i] )
#     #hist(AllData[,make.names(colnames(AllData)) == i] )
# 	}
# }

lapply(make.names(Vec.Test),function(x){
	tempVal <- as.character(tolower(colnames(uniSample)))
	tempVal[is.na(tempVal)] <- "NA"
	tempVal <-make.names(tempVal)	 == make.names(tolower(x))
	if("mass.error.cal.50%" == x){
	test<- aggregate(uniSample[,tempVal],list(UniMachine),min,na.rm = T)
		
	}else{ 
	test<- aggregate(uniSample[,tempVal],list(UniMachine),max,na.rm = T)
	}
	ColMQQCTLPLOTTING <<- 	cbind(ColMQQCTLPLOTTING,test[,2])
	rownames(ColMQQCTLPLOTTING) <<- test[,1]
	
	return(NULL)
})


ColMQQCTLPLOTTING[is.infinite(ColMQQCTLPLOTTING)] <- 0.1
ColMQQCTLPLOTTING <<- ColMQQCTLPLOTTING
MaxV<- apply(abs(ColMQQCTLPLOTTING),2,max,na.rm = T)
MinV<- apply(abs(ColMQQCTLPLOTTING),2,min,na.rm = T)

names(MaxV) <- Vec.Test
names(MinV) <- Vec.Test

colnames(ColMQQCTLPLOTTING) <- Vec.Test
for(i in unique(UniMachine)){
	dir.create(paste(finalMQQC ,"TimeLines",sep = "/"))
if(PDF){
	inputName <- paste("TimeLine",TLname,".pdf",sep = "")
		pdf(pdfName <- paste(finalMQQC ,"TimeLines",paste(i,inputName,sep = "-"),sep = "/"),height = 3.5,pointsize = 12,width = 17)
	par(mai = c(1.2,0.5,0.5,0.1),mfrow = c(1,length(Vec.Test)),cex = 0.5,lwd =1)
	lwdfac = 1

}else{
	
		inputName <- paste("TimeLine",TLname,".jpg",sep = "")

		jpeg(pdfName <- paste(finalMQQC ,"TimeLines",paste(i,inputName,sep = "-"),sep = "/"),height = 350*1.1,pointsize = 10,width = 1700*1.1, quality = 90)
	par(mai = c(3.4,2,2,0.1)/2,mfrow = c(1,length(Vec.Test)),cex = 1,lwd =3)
	lwdfac = 2

}


	
it.a <- 1	
for(a in Vec.Test){
	Vec.Test <<- Vec.Test
	
	MacLine <- ColMQQCTLPLOTTING[rownames(ColMQQCTLPLOTTING) == i,make.names(colnames(ColMQQCTLPLOTTING)) == make.names(a)]
	#if(a == Vec.Test[5]){MacLine <- 0}
	tempI <- uniSample[UniMachine == i,]
  
	colnames(tempI)[is.na(colnames(tempI))]<- "NA"
	
	tempI <- tempI[order(tempI$System.Time.s),]
	TimeI <- tempI$System.Time	
	
	if(a == grep("mass.error",Vec.Test,value = T)){
	
	BestV <- MinV[make.names(names(MinV)) == make.names( a)] 

	}else{
	BestV <- MaxV[make.names(names(MaxV)) == make.names( a)] 
		
	}	
	TimeI <- strsplit(as.character(TimeI),".",fixed =T)
	TimeI <- sapply(TimeI,function(x){
		x1 <- paste(x[1:3],collapse = "-")
		x2 <- paste(x[4:6],collapse = ":")
		x <- c(x1,x2)
		return(x)
	})

	if(length(grep(".50.",make.names(a)))> 0){
		
	CountUp <- tempI[,make.names(colnames(tempI)) == gsub(".50.",".75.",make.names(a))]
	CountLo <- tempI[, make.names(colnames(tempI)) == gsub(".50.",".25.",make.names(a))]
	BordPlot <- T

	}else{BordPlot <- F}
	
	TimeI <- t(TimeI)
	TimeI <- gsub("^X","", TimeI)
	deleteTimeVec <- duplicated(substr(TimeI[,1],1,7))
	Count <- tempI[,make.names(colnames(tempI)) == make.names(a)]
	

	y <- as.numeric(as.character(Count))
	x <- as.numeric(as.character(tempI$System.Time.s))
	NameTemp <- Names[it.a]
  if(log2Fun[it.a]){
    y <- log10(y)
    CountUp <- log10(CountUp)
    CountLo <- log10(CountLo)
    Count <- log10(Count)
    
    BestV <- log10(BestV)
    NameTemp <- paste("log10",NameTemp)
  } 	
 	# if(a == Vec.Test[5]){

  # y <- log10(y)
	# Count <- log10(Count)
	# CountUp <- log10(CountUp)
	# CountLo <- log10(CountLo)
	# }	
 	
  
  if(all(is.na(y))){y[is.na(y)] <- 0;Count[is.na(Count)] <- 0}
	if(length(x) == length(y) & length(y) > 0 & any(!is.na(Count))){
		
	if(BordPlot){
		rangeVal <- range(c(Count,CountUp,CountLo),na.rm = T)
		
	}else{
		rangeVal <- range(c(Count),na.rm = T)

	}
	#if(rangeVal[1] > 0){rangeVal[1] <- 0}
if(!is.na(BestV) & a != grep("mass.error",(Vec.Test),value =T )){
	if(rangeVal[2] < BestV)
	rangeVal[2] <- BestV

}

xYear <- x > (max(x)-31536000)	
x <- x[xYear]
y <- y[xYear]

if(!any(c(all(is.na(x)),all(is.na(y))))){

plot(x, y,axes = F,type = "n",lwd = 3,xlab = "",ylab = NameTemp,main = paste(i, NameTemp,sep = "\n"),cex.lab = 2,ylim = rangeVal,xlim = c(min(x),as.numeric(Sys.time())))
grid()

it.a <- it.a+1 # Counter for Names

	if(BordPlot){

	#lines(x,CountUp,type = "l",lty = "dashed",lwd = 1,col = "red")
	#lines(x,CountLo,type = "l",lty = "dashed",lwd = 1,col = "red")
polygon(c(x, rev(x)), c(CountUp[xYear], rev(CountLo[xYear])),
     col = "grey80", border = NA)
     }
  
       lines(x,y,lwd =4*lwdfac,col = "grey30")
	time <- as.numeric(as.character(tempI$System.Time.s))[!deleteTimeVec]
  label <- substr(TimeI[! deleteTimeVec,1],1,10)
  if(as.numeric(Sys.time())-max(time) > 86400){
    time <- c(time,as.numeric(Sys.time()))
    label <- c(label,"today")  
  }
  
	axis(1,time,label = label,las = 2,cex.axis = 2)
	axis(2,cex.axis = 2)
	
	if(!is.na(BestV)){
		abline(h = BestV,col = "tomato",lty = "1414",lwd = 2*lwdfac)
	}
	if(length(MacLine) > 0){
		abline(h = MacLine,col = "cadetblue",lty = "4949",lwd = 2*lwdfac)
	}
	}
 points(lowess(x,y),type = "l",col = "orange",lwd = 2*lwdfac)

if(it.a == 2){legend("bottomleft",legend = c("All",i),col = c("tomato","cadetblue"),box.col = "transparent",title = "Best value",lty = "dashed",bg = "#FFFFFF70")}

}
}
	graphics.off()
#stop()
}
#system(paste("open", pdfName))

}
#try(plottingTimeLineFunction(AllData = collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = F, RESettings = RESettings),silent = F)
