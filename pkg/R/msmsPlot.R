msmsPlot <- 
function(pdfOut = T,path = "./", RawFilesUsed = NULL){
msmsPath <- list.files(path, pattern="msms.txt",full.name = T)
if(length(msmsPath) > 0){
	

	
	MSMS.Import <- read.table(msmsPath,colClasses = "character",sep = "\t",comment.char = "")
	
	
	colnames(MSMS.Import) <- tolower(make.names(MSMS.Import[1,]))
		MSMS.Import <- MSMS.Import[-1,]

	if(length(RawFilesUsed) > 0){
		mergeTemp <- merge.control(MSMS.Import$raw.file,RawFilesUsed)
		mergeTemp  <- mergeTemp[!is.na(mergeTemp)] 
		if(length(mergeTemp) > 0){
			MSMS.Import <- MSMS.Import[unique(mergeTemp),]
		}
	}
	if(length(unique(MSMS.Import$raw.file))== 1){
		
		.name 	<- paste("MSMS_Dens",unique(MSMS.Import$raw.file),sep = "_")
		.name 	<- gsub(".raw$","",.name)
		.name <- paste(.name,".pdf",sep = "")
	}else{
		.name <- "MSMS_Dis.pdf"
	}
	
		if(pdfOut){
		pdf(.name,pointsize = 20,width = 10)
	par(mai = c(1.5,1.5,0.3,0.2))
	}
	

	 test <- quantile(temp <- as.numeric(unlist(strsplit(as.character(MSMS.Import$intensities),";"))),na.rm  =T)
	 #hist(log10(temp),breaks = 100)


legInfo <- aggregate(MSMS.Import$intensities,list(round(as.numeric(MSMS.Import$retention.time),-1)+10),function(x){

	temp <- as.numeric(unlist(strsplit(as.character(x),";")))
	
	return(temp)
})
Data  					<- legInfo[2]
rownames(Data) 	<- sapply(legInfo[1],as.character)
Data <- apply(Data,1,unlist)
DataL <- unlist(lapply(Data,length))

Data2 <- Data[order(DataL,decreasing = T)]	
DataL <- DataL[order(DataL)]
cols <- rainbow(length(Data2)+1, alpha = 1,start = 0.1, end = 1,s = 0.9,v = 0.9)
a <- 1
maxv <- 0
maxx <- 2
orderMax <- c()
Data2$all <-  as.numeric(unlist(strsplit(as.character(MSMS.Import$intensities),";")))
DataL <- c(DataL,length(Data2$all))


colsVec <- lapply(Data2,function(x){
	dens <- density(as.numeric(log10(x)))
	maxv <<- max(maxv,max(dens$y),na.rm = T)
	maxx <<- c(min(maxx,min(dens$x),na.rm = T),max(maxx,max(dens$x),na.rm = T))
	orderMax <<- c(orderMax,max(dens$y,na.rm = T))
	return(dens)
})

maxx <- maxx+ maxx*c(0,0.2)

plot(1,type = "n",ylim = c(0, maxv),ylab = "Density",xlab = "log10(Intensity)",frame = F,xlim = maxx,main = "Fragment Ions Density Plot",sub = .name,mgp = c(2.3,1,0))
grid(lwd = 4)
a <- 1
colsVec <- colsVec[order(orderMax,decreasing = T)]
cols <- cols[order(orderMax,decreasing = T)]
DataL <- DataL[order(orderMax,decreasing = T)]
lapply(colsVec,function(x){
		points(x,col = "white",type = "l",lwd = 7)

	points(x,col = cols[a],type = "l",lwd = 4)
	
	a <<- a+1
})


legend("topright",legend = paste(names(colsVec),"n:",DataL)[order(names(colsVec))],col = cols[order(names(colsVec))],lwd = 10,bty = "n",cex = 1,xpd = NA, xjust = 0.5)
if(pdfOut){
graphics.off()	
}
return(c(quantile(Data2$all,na.rm = T)))

}else{return(rep(0,5))}
}