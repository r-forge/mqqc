plottingTimeLineFunction <- function(AllData,folder,finalMQQC,TargetVec = "ECstd",PDF = F,lwdfac = 1, RESettings = RESettings, TLname = "",pdfShow = F){
  
  # Loading Distribution Cutoffs
  ECquan <- list.files(paste(folder,"_RmqqcFile_Quantiles",sep = "/"),pattern = TargetVec,full.names =  T)
  if(length(ECquan) == 1){
    try(rm("Quan"),silent = T)
    load(ECquan)
    names(Quan)[names(Quan) == "Isotope.patterns.min" ] <- "MSID.min"
  }
  
  AllData <<- AllData
  NameInfo <- stringSplitter(AllData$Name,RESettings = RESettings)
  NameInfo <- as.data.frame(NameInfo)
  NameInfo$TimeTag <- paste(substr(NameInfo$TimeTag,1,4),substr(NameInfo$TimeTag,5,6),substr(NameInfo$TimeTag,7,8),sep = "-")
  os <-   options("warn")
  AllData$System.Time.s <- as.numeric(as.POSIXct(NameInfo$TimeTag, format="%Y-%m-%d"))
  AllData$System.Time <- NameInfo$TimeTag#as.numeric(as.POSIXct(NameInfo$TimeTag, format="%Y-%m-%d"))
  
  options(warn = -1)
  #AllData <- backup
uniSample <- AllData[grep(TargetVec, AllData$Name),]
test <- sub(RESettings$REmac,"", as.character(uniSample$Name))


UniMachine  <- grepRE(as.character(uniSample$Name),RESettings$REmac)

test <- sub(RESettings$REmac,"", UniMachine)


#Vec.Test <- c("quan.msms.min","Intensity.50%","msmsMassCount.50%","mass.error.uncal.50%")
#Names <- c("Peptide ID / min","MS Median Intensity","Fragment Counts / MSMS","uncalibrated Mass Error [ppm]")
DensMatrixTemplate <- rbind(
  #MS
  c("MSID.min","Features/min",F,"MS"),
  c("Intensity.50.","MS Median Intensity",T,"MS"),
  c("precision.50.","Mass precision [ppm]",F,"MS"),
 # c("Coverage","Coverage",F,"MS"),
  #MSMS
  c("quan.msms.min","IDs/min",F,"MSMS"),
  c("msmsQuantile.50.","MSMS Median Intensity",T,"MSMS"),
  c("msmsMassCount.50.","MSMS Median Fragment Counts",F,"MSMS"),
  #nlc
  c("LCcombiScore","LC profile distribution",F,"LC"),
  c("ret.width.50.","log10 Retention Time [s]",F,"LC"),
  c("ret.peak.shape.abs.50.","Peak symmetry",T,"LC"))

Vec.Test <- DensMatrixTemplate[,1]
Names <- DensMatrixTemplate[,2]
TypeQC <- DensMatrixTemplate[,4]
# Names <- paste(TypeQC,Names,sep = ": ")

ColMQQCTLPLOTTING <<- c()
log2Fun <- c(F,T,F,F)
log2Fun <- c(F,T,F,F,T,F,F,F,T)
# for(i in log2Fun){
# 	if(any(make.names(colnames(AllData)) == make.names(i))){
# 		#AllData[,make.names(colnames(AllData)) == i] <- log10(AllData[,make.names(colnames(AllData)) == i] )
#     #hist(AllData[,make.names(colnames(AllData)) == i] )
# 	}
# }

nouse <-lapply(make.names(Vec.Test),function(x){
	tempVal <- as.character(tolower(colnames(uniSample)))
	tempVal[is.na(tempVal)] <- "NA"
	tempVal <-make.names(tempVal)	 == make.names(tolower(x))
	checkty <- uniSample[,tempVal]
	checktyS <- is.infinite(checkty)
	if("precision.50." == x){
	test<- aggregate(checkty[!checktyS],list(UniMachine[!checktyS]),min,na.rm = T)
		
	}else{ 
	test<- aggregate(checkty[!checktyS],list(UniMachine[!checktyS]),max,na.rm = T)
	}
	test[is.infinite(test[,2]),2] <- NA
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
unim <- unique(UniMachine)
# unim <- "Gladys"
for(i in unim){
	dir.create(paste(finalMQQC ,"TimeLines",sep = "/"),showWarnings = F)
if(PDF){
	inputName <- paste("TimeLine",TLname,".pdf",sep = "")
		pdf(pdfName <- paste(finalMQQC ,"TimeLines",paste(i,inputName,sep = "-"),sep = "/"),height = 10,pointsize = 12,width = 15)
	par(mai = c(1.2,0.5,0.5,0.1),mfrow = c(3,3),cex = 0.5,lwd =1)
	lwdfac = 1

}else{
	
		inputName <- paste("TimeLine",TLname,".jpg",sep = "")

		jpeg(pdfName <- paste(finalMQQC ,"TimeLines",paste(i,inputName,sep = "-"),sep = "/"),height = 1250,pointsize = 10,width = 1700*1.1, quality = 90)
	par(mai = c(3.4,2,2,0.1)/2,mfrow = c(3,3),cex = 1,lwd =3)
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
	
	if(a == grep("precision",Vec.Test,value = T)){
	
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
	Count[is.infinite(Count)] <- NA

	y <- as.numeric(as.character(Count))
  
	x <- as.numeric(as.character(tempI$System.Time.s))

  if(!any(all(is.na(x))|all(is.na(y)))){
	NameTemp <- Names[it.a]
  if(log2Fun[it.a]){
    y <- log10(y)
    CountUp <- log10(CountUp)
    CountLo <- log10(CountLo)
    Count <- log10(Count)
    
    BestV <- log10(BestV)
    NameTemp <- paste("log10",NameTemp)
    
  } 
	if(exists("CountUp")){
	  CountUp[is.infinite(CountUp)] <- NA
	}
	if(exists("CountLo")){
 	CountLo[is.infinite(CountLo)] <- NA
	}
	if(exists("Count")){
	  
	Count[is.infinite(Count)] <- NA
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
		rangeVal <- range(quantile(c(Count,CountUp,CountLo),probs = c(0.01,0.99),na.rm = T),na.rm = T)
		
	}else{
		rangeVal <- range(quantile(c(Count[!is.infinite(Count)]),probs=c(0.01,0.99,na.rm = T)),na.rm = T)

	}
	#if(rangeVal[1] > 0){rangeVal[1] <- 0}
if(!is.na(BestV) & a != grep("precision",(Vec.Test),value =T )){
	if(rangeVal[2] < BestV)
	rangeVal[2] <- BestV

}

xYear <- x > (max(x,na.rm = T)-31536000)	
xYear <- 1:length(x)
x <<- x[xYear]
y <<- y[xYear]


if(!any(c(all(is.na(x)),all(is.na(y))))){

plot(x, y,axes = F,type = "n",lwd = 3,xlab = "",ylab = NameTemp,main = paste(i, paste(TypeQC[it.a],NameTemp,sep = ": "),sep = "\n"),cex.main =2.25,cex.lab = 2,ylim = rangeVal,xlim = c(min(x,na.rm = T),as.numeric(Sys.time())))
abline(v = pretty(x),col = "grey",lty = "dotted")
if(exists("Quan") ){
  names(Quan)[is.na(names(Quan))] <- "PLACEHOLDER"
  
  if(any(names(Quan) == a)){
  QuanA <- Quan[names(Quan) == a][[1]]
  if(is.matrix(QuanA)){
    QuanA <- QuanA[,colnames(QuanA) == i]
  }
  or <- order(as.numeric(gsub("%","",names(QuanA))))
  grad.cols.vec <-  c("black","#D55E00", "#F0E442", "#009E73")
  
  try(abline(h=QuanA[or], lty = "dotted",col = colorRampPalette(rev(grad.cols.vec))(5)[or],lwd = 1 ))
  try(text(hui<<-rep(par()$usr[2],length(QuanA)),hui2 <<- as.numeric(QuanA),names(QuanA),pos = 3,cex = 0.5,xpd = NA),silent = T)
  }
}
if(it.a == 1){
  mtext(Sys.Date(),3,adj = 0,line = 3,cex = 0.5)
}
it.a <- it.a+1 # Counter for Names

	if(BordPlot){

	#lines(x,CountUp,type = "l",lty = "dashed",lwd = 1,col = "red")
	#lines(x,CountLo,type = "l",lty = "dashed",lwd = 1,col = "red")
	  
	  # x <<- x
	  coup <- CountUp[xYear]
	  colo <- CountLo[xYear]
	  # if(length(coup)>0){
	  #   stop()
	  # }
	  sel <- (is.na(coup)|is.na(colo)|is.na(x))|(is.infinite(coup)|is.infinite(colo)|is.infinite(x))
	  sel <- !sel
    polygon(c(x[sel], rev(x[sel])), c(coup[sel], rev(colo[sel])),
     col = "grey80", border = NA)
     }
  # y <<- y
  lines(x,y,lwd =2*lwdfac,col = "grey30")
	time <- as.numeric(as.character(tempI$System.Time.s))[!deleteTimeVec]
  label <- substr(TimeI[! deleteTimeVec,1],1,10)
  if(as.numeric(Sys.time())-max(time) > 86400){
    time <- c(time,as.numeric(Sys.time()))
    label <- c(label,"today")  
  }
  
	axis(1,time,label = label,las = 2,cex.axis = 2)
	axis(2,cex.axis = 2)
	
	if(!is.na(BestV)){
		# abline(h = BestV,col = "tomato",lty = "1414",lwd = 2*lwdfac)
	}
	if(length(MacLine) > 0){
		# abline(h = MacLine,col = "cadetblue",lty = "4949",lwd = 2*lwdfac)
	}
	}
 points(lowess(x,y),type = "l",col = "orange",lwd = 2*lwdfac)

# if(it.a == 2){legend("bottomleft",legend = c("All",i),col = c("tomato","cadetblue"),box.col = "transparent",title = "Best value",lty = "dashed",bg = "#FFFFFF70")}

}
}
}
	graphics.off()
	if(pdfShow){
	  try(system(paste("open",pdfName)))
	}
}
options(warn = unlist(os))

}
# try(plottingTimeLineFunction(AllData = collectList[grep("^Animal",collectList$Name),],folder=folder,finalMQQC = finalMQQC, TargetVec = "HSstd",PDF = T, RESettings = RESettings, TLname= "-All"),silent = F)	

# try(plottingTimeLineFunction(AllData = collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = T, RESettings = RESettings,TLname= "-high",pdfShow = F),silent = F)

# try(plottingTimeLineFunction(AllData = collectList,finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = T, RESettings = RESettings,TLname= "-high"),silent = T)

# LoadSettings( AllData= collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = T, RESettings = RESettings,TLname= "-high")
# (plottingTimeLineFunction(AllData = collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = T, RESettings = RESettings,TLname= "-high"))

#try(plottingTimeLineFunction(AllData = collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = F, RESettings = RESettings),silent = F)
# try(plottingTimeLineFunction(AllData = collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = F, RESettings = RESettings,TLname= "-high"),silent = F)
