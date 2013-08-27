FUNFINAL <-
function(finalMQQC = "D:/resultsmqqc",folder,sucFolder="_RmqqcFile_Processed"){

dir.create(finalMQQC, showWarnings = F)
dir.create(allPath <- paste(finalMQQC,"all",sep = "/"), showWarnings = F)
dir.create(ECstdPath <- paste(finalMQQC,"ECstd",sep = "/"), showWarnings = F)
dir.create(ECstdPath <- paste(finalMQQC,"files",sep = "/"), showWarnings = F)

if(length(list.files(finalMQQC,pattern = "example.css",recursive = T))== 0){
unzip(list.files(paste(path.package("mqqc"),"data/",sep = "/"),full.name = T,pattern = "zip"),exdir = finalMQQC)	

# from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "example.css")
# to		 <- 	paste(finalMQQC,"example.css",sep = "/")
# try(file.copy(from,to))
# from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "tabber.js")
# to		 <- 	paste(finalMQQC,"tabber.js",sep = "/")
# try(file.copy(from,to))

}

Machines 	= c("Bibo","Kermit","Grobi","Bert","Tiffy")
HotLink = rep("-",length(Machines))
HotLinkCol = rep("#ffffff",length(Machines))
collectListPath <- paste(folder,sucFolder,"list_collect.csv",sep = "/")
	 	
if(file.exists(collectListPath)){
	collectList	<- read.csv(collectListPath, check.names = F,stringsAsFactors = F)
	collectList <- cbind(1:dim(collectList)[1],collectList)
	collectList   <- collectList[!duplicated(collectList$Name),]
	Names 		<- collectList$Name
	ECstd 			<- grep("._.*_.*_ECstd",collectList$Name)
	Normal 		<- grep("._.*_.*_ECstd",collectList$Name,invert = T)
	collectListAll <- list()
	collectListLife <- list()	
	it <- 1
	for(iList in list(ECstd,Normal)){
		tempListOne <- collectList[iList,]
		
		collectListSorted <- c()
		collectListSortedLife <- c()
		Names 		<- sapply(strsplit(tempListOne$Name,"_"),function(x){x[1]})
	for(iNames in unique(Names)){
		tempList <- tempListOne[Names == iNames,]
		tempList <- tempList[order(tempList$SourceTime,decreasing = T),]
		if(dim(tempList)[1] > 10){
			tempList <- tempList[1:10,]
		}
		if(any(iNames==Machines)
		#&!all(is.na(tempList[1,]))
		){
			collectListSortedLife <- rbind(collectListSortedLife,tempList[1,])
			if(it == 1){
				tempList$msms.count[1]
			HotLink[iNames==Machines] <- tempList$msms.count[1]
			HotLinkCol[iNames==Machines] <- tempList$TotalScoreColor[1]
						file.copy(gsub("csv$","pdf",tempList$exitPath[1]),paste(htmloutPath,"ECstd",paste(iNames,".pdf",sep = ""),sep = "/"), overwrite = T)

			}else{
				file.copy(gsub("csv$","pdf",tempList$exitPath[1]),paste(htmloutPath,"all",paste(iNames,".pdf",sep = ""),sep = "/"),overwrite = T)

			}
			
			
			
		}
		if(!all(is.na(tempList))){
			collectListSorted <- rbind(collectListSorted,tempList)
		}
	}
	collectListAll[[it]] <- collectListSorted
	collectListLife[[it]] <- collectListSortedLife
	#Prepare for HTML
	temp <- stringSplitter(as.character(collectListSorted$Name))
temp <- t(temp)
temp <<- temp
if(is.vector(temp)){temp <- as.matrix(temp)}
if(dim(temp)[2] == 5){
temp[,5] <- substr(temp[,5],start = 1,stop = 20)
input 		<- temp
colsTemp <- c("MS","Time","User","Species","misc")
}else{
input <- as.character(collectListSorted$Name)
colsTemp <- c("Sample")
}
## ColorCodeScore
colorCode <- collectListSorted$TotalScoreColor
colorCode <- paste("<font color = '",colorCode,"'>&#9829;</font>",sep = "")

TotalScore <- round(as.numeric(as.character(collectListSorted$TotalScore))*100) 
ncharL 		<- 3- nchar(TotalScore)
TotalScoreAdd <- sapply(ncharL,function(x){
	if(x !=0){
		x <- paste(rep(0,x),collapse = "")
		return(x)
	}else{return("")}
	
})
TotalScore <- paste(TotalScoreAdd ,TotalScore,sep = "")
colorCode <- paste(TotalScore,colorCode)

pathPdf  			<- gsub(".csv$",".pdf",collectListSorted$exitPath)
pathMsMsPdf <- gsub(".csv$",".pdf",collectListSorted$exitPath)
pathMsMsPdf <- paste(dirname(pathMsMsPdf),paste("MSMS_Dens_",basename(pathMsMsPdf),sep = ""),sep = "/")
pathMsMsPdf <- gsub("raw.pdf$","pdf", pathMsMsPdf)



###
# function to move pdf to html Output
###
htmlPdfFold <- "files"
# move all Files:
ActualFile <- list.files(paste(htmloutPath,htmlPdfFold,sep = "/"))
ToMove <- setdiff(basename(pathPdf),ActualFile)
if(length(ToMove) > 0){
ToMove <- merge.control(basename(unique(pathPdf)),ToMove)
ToMove <- unique(pathPdf)[ToMove]
file.copy(ToMove,paste(htmloutPath,htmlPdfFold,basename(ToMove),sep = "/"))
ToMove <- paste(dirname(ToMove),paste("MSMS_Dens_",basename(ToMove),sep = ""),sep = "/")
ToMove <- gsub("raw.pdf$","pdf", ToMove)
file.copy(ToMove,paste(htmloutPath,htmlPdfFold,basename(ToMove),sep = "/"))
}
#if(length(pathPdf) > 0){}
tempPaths 				<- paste(".",htmlPdfFold,basename(pathPdf),sep = "/")
tempPathsMsMs2 	<- paste(".", htmlPdfFold,basename(pathMsMsPdf),sep = "/")
collectListSorted$System.Time <- substr(collectListSorted$System.Time,2,nchar(as.character(collectListSorted$System.Time)))
substr(collectListSorted$System.Time , 11, 12) <- " "
if(length(pathPdf) >0){
#tempPathsMsMs <-as.vector(as.matrix(tempPaths)[2,])
tempPaths <- paste("<a href ='", tempPaths,"' target ='_blank' >MQQC</a>",sep = "")
tempPathsMsMs2 <- paste("<a href ='", tempPathsMsMs2,"'  target ='_blank' >MsMs</a>",sep = "")
#tempPathsMsMs2[tempPathsMsMs == ""] <- ""

collectListSorted <- as.data.frame(collectListSorted)

colnames(collectListSorted) <- make.names(colnames(collectListSorted))
try(collectListSorted <- cbind(	colorCode ,
													input, 
													as.character(collectListSorted$System.Time), 
													collectListSorted$msms.count, 
													collectListSorted$uniPepCount,
													round(as.numeric(collectListSorted$quan.msms.min),2) ,
													round(as.numeric(collectListSorted$mass.error.cal.50.),2), 
													collectListSorted$score.50., tempPaths, tempPathsMsMs2))

try(colnames(collectListSorted) <- c("",colsTemp,"Time","Peptides","Unique_Peptides","MSMS/min","mass_error_[ppm]","Score_M","",""))
alignVec <<- c("center","left",rep("center",(dim(collectListSorted)[2]-2)))

if(it ==1){
	 try(tableHtml2 <-HtmlTable(collectListSorted, tableDesign = "table-design2"))
if(!exists("tableHtml2")){tableHtml2 <- NULL}
	}else{
		 try(tableHtml <- HtmlTable(collectListSorted, align= alignVec))
	}
}else{	
	if(it == 1){
	tableHtml2 <- ""
	}else{ tableHtml <- ""}}
		
	it <- it+1

	}
#####

paste(finalMQQC,"all",paste(Machines,".pdf",sep = ""),sep = "/")

writeToHtml(sort(paste(".","ECstd",paste(Machines,".pdf",sep = ""),sep = "/")),sort(paste(".","all",paste(Machines,".pdf",sep = ""),sep = "/")),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 )


try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = HotLink,BGcolor =as.character(HotLinkCol)))
}

}