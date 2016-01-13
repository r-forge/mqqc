FUNFINAL <-
function(finalMQQC = "D:/resultsmqqc",folder,sucFolder="_RmqqcFile_Processed",Machines 	= c("Bibo","Kermit","Grobi","Bert","Tiffy"), dayThresh = 5, RESettings = NULL, RESettingsSep = "_",StandardIDs = c("ECstd","BSA"), placeholder = "PLACEHOLDER" ,ordertype = "source"){
#StandardIDs[1] = ""
  # ToDo Table is not rpeorted # html Path is not set 
  
  
htmlPdfFold <- "files"
  
dir.create(finalMQQC, showWarnings = F)
dir.create(allPath <- paste(finalMQQC,"all",sep = "/"), showWarnings = F)
dir.create(ECstdPath <- paste(finalMQQC, StandardIDs[2],sep = "/"), showWarnings = F)
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

HotLink = rep("-",length(Machines))
HotLinkCol = rep("#ffffff",length(Machines))
HotLinkPath = rep("-",length(Machines))
collectListPath <- paste(folder,sucFolder,"list_collect.csv",sep = "/")
if(file.exists(collectListPath)){
	
	collectList	<- read.csv(collectListPath, check.names = F,stringsAsFactors = F)
	collectList <- collectList[!is.na(collectList$msms.count),]
	collectList <- cbind(1:dim(collectList)[1],collectList)
	collectList   <- collectList[!duplicated(collectList$Name),]
	Names 		<- collectList$Name
	
	#RE work TODO

	if(length(grep(placeholder,RESettings$REpar)) > 0){
	StandardIDsRE <-  sapply(StandardIDs ,function(x){gsub(placeholder,x,RESettings$REpar)})
	try(StandardIDsRE <- gsub(paste(RESettingsSep,"$",sep = ""),"", StandardIDsRE))
	ECstd 			<- grep(StandardIDsRE[1],collectList$Name,ignore.case = T)
  
	BSA 			<- grep(StandardIDsRE[2],collectList$Name,ignore.case = T)	

	}else{
	
	TempNames <- grepRE( gsub(".raw","_raw",collectList$Name),"\\D*_\\d*_[^_]*_[^_]*")
	TempNames  <- sapply(strsplit(as.character(TempNames),"_"),function(x){x[length(x)]})
	
	ECstd <- 1:dim(collectList)[1]
	ECstd <- ECstd[TempNames == StandardIDsRE[1]]
	BSA <- 1:dim(collectList)[1]
	BSA <- BSA[TempNames == StandardIDsRE[2]]	
	
	}

	
  if(all(StandardIDs != "")){
	  Normal 		<- setdiff(1:dim(collectList)[1],unique(c(ECstd,BSA)))
    AnalysisType <- "both"
	  InitList <- list(ECstd,Normal, BSA)
	  itType = 1
	  itTypeCov <- F
	  
  }
	
	if(StandardIDs[1] != "" & StandardIDs[2] == ""  ){
	  Normal 		<- setdiff(1:dim(collectList)[1],unique(c(ECstd)))
	  AnalysisType <- "high"
	  InitList <- list(ECstd,Normal,0)
	  itType = 1
	  itTypeCov <- F
	  
	}
	
	if(all(StandardIDs[2] != "") & StandardIDs[1] == ""){
	  Normal 		    <- setdiff(1:dim(collectList)[1],unique(c(BSA)))
	  AnalysisType  <- "low"
	  InitList      <- list(0,Normal,BSA)
	  itType = 3
	  itTypeCov <- T
	  
	  
	}
	if(all(StandardIDs == "")){
	  Normal 		<-1:dim(collectList)[1]
	  AnalysisType <- "none"
	  InitList <- list(0,Normal,0)
	  itType = 2
	  itTypeCov <- F
	  
	  
	}  
	#InitList <<- InitList
	collectListAll <- list()
	collectListLife <- list()	
  
	if(Machines == "NA"|all(Machines== "") ){
	  Names <- grepRE(as.character(collectList$Name),RESettings$REmac)
	  Machines = unique(Names)
	  
	}else{Machines = Machines}
  
  
	if(!exists("funlastLoop", envir = .GlobalEnv)){funlastLoop <<- 2}
  	if(funlastLoop %% 10 == 0| funlastLoop == 2){
  		BSA <<- BSA
  		ECstd <<- ECstd
  		Normal <<- Normal
    if(AnalysisType == "both"|AnalysisType == "high" ){
  	try(CompareComplexStdFromTable(tempListOne = collectList[ECstd,],RESettings = RESettings,pdfShow = F,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = StandardIDs[1],PDF = T, Machines = Machines,StandardIDs = StandardIDs),silent = T)
  	try(plottingTimeLineFunction(AllData = collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = T, RESettings = RESettings,TLname= "-high"),silent = T)
  	try(CompareComplexStdFromTable(collectList[ECstd,],RESettings,F,finalMQQC, PDFname = "ComplexStandardComparison.jpg", TargetVec = StandardIDs[1],PDF = F, Machines = Machines),silent = T)
  	try(plottingTimeLineFunction(AllData = collectList[ECstd,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = F, RESettings = RESettings,TLname= "-high"),silent = T)
    }
    if(AnalysisType == "both"|AnalysisType =="low"){
      
  	try(CompareComplexStdFromTable(collectList[BSA,],RESettings,F,finalMQQC, PDFname = "LowComplexStandardComparison.pdf", TargetVec = StandardIDs[2],PDF = T, Machines = Machines),silent = T)
  	try(plottingTimeLineFunction(AllData = collectList[BSA,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = T, RESettings = RESettings,TLname= "-Low"),silent = T)
  	try(plottingTimeLineFunction(AllData = collectList[BSA,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = F, RESettings = RESettings,TLname= "-Low"),silent = T)
  	try(CompareComplexStdFromTable(collectList[BSA,],RESettings,F,finalMQQC, PDFname = "LowComplexStandardComparison.jpg", TargetVec = "BSA",PDF = F, Machines = Machines),silent = T)
    }
    
    if(length(Normal) > 0){
  	try(CompareComplexStdFromTable(collectList[Normal,],RESettings,F,finalMQQC, PDFname = "NormalSampleComparison.pdf",main = "MQQC Normal Samples Parameter Comparison", TargetVec = "",PDF = T, Machines = Machines),silent = T)
  	try(CompareComplexStdFromTable(collectList[Normal,],RESettings,F,finalMQQC, PDFname = "NormalSampleComparison.jpg",main = "MQQC Normal Samples Parameter Comparison", TargetVec = "",PDF = F, Machines = Machines),silent = T)
    try(plottingTimeLineFunction(AllData = collectList[Normal,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = T, RESettings = RESettings, TLname= "-All"),silent = T)	
  	try(plottingTimeLineFunction(AllData = collectList[Normal,],finalMQQC = finalMQQC, TargetVec = StandardIDs[1],PDF = F, RESettings = RESettings, TLname= "-All"),silent = T)
    }
	  }
	
	it <<- 1
	collectList$SourceTime[is.na(collectList$SourceTime)] <- collectList$System.Time.s[is.na(collectList$SourceTime)]
  
	for(iList in InitList){
		tempListOne <- collectList[iList,]
		collectListSorted <- c()
		collectListSortedLife <- c()
		
		#RE work TODO
		#Names 		<- sapply(strsplit(tempListOne$Name,"_"),function(x){x[1]})
		if(dim(tempListOne)[1] != 0){
		Names <- grepRE(as.character(tempListOne$Name),RESettings$REmac)
    # Fix, cause crash, if NA in Names
		Names[is.na(Names)] <- ""
    #NamesUni <- unique(Names)
    
	for(iNames in unique(Names)){
    
	  
    
    
		tempList <- tempListOne[Names == iNames,]
		tempList <- tempList[!is.na(tempList$SourceTime),]
		if(ordertype == "source"){
		  try(		tempList <- tempList[order(tempList$SourceTime,decreasing = T),])
		  
		}
		if(ordertype == "system"){
		  try(		tempList <- tempList[order(tempList$System.Time.s,decreasing = T),])
		  
		}
		
		
		if(dim(tempList)[1] > 10){
			tempList <- tempList[1:10,]
		}
		if(any(iNames==Machines)
		#&!all(is.na(tempList[1,]))
		){
		collectListSortedLife <- rbind(collectListSortedLife,tempList[1,])
			

		
			if(it == itType){
				
			if(itTypeCov){
				linkInput <- tempList$Coverage[1]
				HotLinkType = "Coverage"
			}else{
				linkInput <-	 tempList$msms.count[1]
				HotLinkType = "msms/min"

			}
				
			
      
      
			HotLink[iNames==Machines] <- linkInput
			HotLinkPath[iNames == Machines] <- paste(".",htmlPdfFold,basename(gsub(".csv$",".pdf", tempList $exitPath)),sep = "/")
			
			HotLinkCol[iNames==Machines] <- tempList$TotalScoreColor.Total[1]
			days <- abs(as.numeric(tempList$System.Time.s)-as.numeric(Sys.time()))/(60*60*24)
			tempList <<- tempList
      if(!is.na(days)){
			if(days > dayThresh){
					HotLinkCol[iNames==Machines] <- "#797979"
			}
      }
			
						file.copy(FCheck <-gsub("csv$","pdf",tempList$exitPath[1]),paste(finalMQQC, StandardIDs[1],paste(iNames,".pdf",sep = ""),sep = "/"), overwrite = T)

			}else{
				file.copy(FCheck <-gsub("csv$","pdf",tempList$exitPath[1]),paste(finalMQQC,"all",paste(iNames,".pdf",sep = ""),sep = "/"),overwrite = T)

			}
			
			
		}
		if(!all(is.na(tempList))){
			collectListSorted <- rbind(collectListSorted,tempList)
		}
	}
	collectListAll[[it]] <- collectListSorted
	collectListLife[[it]] <- collectListSortedLife

#Prepare for HTML
    tempInput <- as.character(collectListSorted$Name)
     if(length(tempInput) ==0){tempInput <- "NO DATA"}                         
temp <- stringSplitter(tempInput,RESettings = RESettings)

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
colorCode <- collectListSorted$TotalScoreColor.Total
colorCode <- paste("<font color = '",colorCode,"'>&#9829;</font>",sep = "")

TotalScore <- round(as.numeric(as.character(collectListSorted$TotalScore.Total))*100) 
#TotalScore[is.na(TotalScore)] <- 0
ncharL 		<- 3- nchar(TotalScore)
TotalScoreAdd <- sapply(ncharL,function(x){
	if(x !=0){
		x <- paste(rep(0,abs(x)),collapse = "")
		return(x)
	}else{return("")}
	
})
TotalScore <- paste(TotalScoreAdd ,TotalScore,sep = "")
colorCode <- paste(TotalScore,colorCode)

pathPdf  			<- gsub(".csv$",".pdf",collectListSorted$exitPath)
pathDepPepPie <- gsub(".csv$",".pdf",collectListSorted$exitPath)
pathDepPepPie <- paste(dirname(pathDepPepPie),paste("DepPepPie_",basename(pathDepPepPie),sep = ""),sep = "/")
pathDepPepPie <- gsub("raw.pdf$","pdf", pathDepPepPie)

# Same for chromatogram
pathChrom <- gsub(".csv$",".pdf",collectListSorted$exitPath)
pathChrom <- paste(dirname(pathChrom),paste("chromatogram_",basename(pathChrom),sep = ""),sep = "/")
pathChrom <- gsub("raw.pdf$","pdf", pathChrom)

pathPdf <-		gsub(".RAW_folder/","RAW_folder/",pathPdf) # !!!! Temporal Solution, folder is written without ".", should be better fixed while folder is written

###
# function to move pdf to html Output
###
# move all Files:
ActualFile <- list.files(paste(finalMQQC,htmlPdfFold,sepf = "/"))
ToMove <- setdiff(basename(pathPdf),ActualFile)


if(length(ToMove) > 0){
ToMove <- merge.control(basename(unique(pathPdf)),ToMove)
ToMove <- unique(pathPdf)[ToMove]

if(any(!file.exists(dirname(ToMove)))){
  FixedPath <- sapply(strsplit(ToMove,paste(sucFolder,"/",sep = ""),fixed = T),function(x){
    x <<- x
    return(paste(folder,sucFolder,x[2],sep = "/"))
  }) 
  ToMove <- FixedPath
}

FileExists <- file.exists(ToMove)
if(any(FileExists)){
file.copy(ToMove[FileExists],paste(finalMQQC,htmlPdfFold,basename(ToMove)[FileExists],sep = "/"))
}
#ToMove <- grep("BSAdp",ToMove,value = T)
if(any(!FileExists) ){
  ToMove <- ToMove#[!FileExists]
  ToMove <- paste(gsub("_folder$","raw_folder",dirname(ToMove)),basename(ToMove),sep = "/")
  FileExists <- file.exists(ToMove)
  if(any(FileExists)){
    file.copy(ToMove[FileExists],paste(finalMQQC,htmlPdfFold,basename(ToMove)[FileExists],sep = "/"))
  }  
  ToMove <- ToMove[FileExists]
}

### Move PDFs Types

#file.copy(ToMove,paste(finalMQQC,htmlPdfFold,basename(ToMove),sep = "/"))
if(length(ToMove) > 0){
ToMoveInit <- ToMove
ToMove <- paste(dirname(ToMoveInit),paste("MSMS_Dens_",basename(ToMoveInit),sep = ""),sep = "/")
ToMove <- gsub("raw.pdf$","pdf", ToMove)
file.copy(ToMove,Fcheck<<- paste(finalMQQC,htmlPdfFold,basename(ToMove),sep = "/"))
try(Fcheck <- file.exists(Fcheck))
ToMove <- paste(dirname(ToMoveInit),paste("DepPepPie_",basename(ToMoveInit),sep = ""),sep = "/")
ToMove <- gsub("raw.pdf$","pdf", ToMove)
SucDP <- file.copy(ToMove,Dcheck<<- paste(finalMQQC,htmlPdfFold,basename(ToMove),sep = "/"),overwrite = T)
try(Dcheck <- file.exists(Dcheck))
ToMove <- paste(dirname(ToMoveInit),paste("chromatogram_",basename(ToMoveInit),sep = ""),sep = "/")
ToMove <- gsub("raw.pdf$","pdf", ToMove)
Chrom <- file.copy(ToMove,Ccheck<<- paste(finalMQQC,htmlPdfFold,basename(ToMove),sep = "/"),overwrite = T)
try(Ccheck <- file.exists(Ccheck))
}
}
#if(length(pathPdf) > 0){}
tempPaths 				<- paste(".",htmlPdfFold,basename(pathPdf),sep = "/")
tempPathsDepPepPie2 	<- paste(".", htmlPdfFold,basename(pathDepPepPie),sep = "/")
pathChrom2   <- paste(".", htmlPdfFold,basename(pathChrom),sep = "/")
#if(exists("Fcheck")){
#tempPaths[!file.exists(pathPdf)] <- ""  
#tempPathsDepPepPie2[!file.exists(pathDepPepPie)] <- ""
#pathChrom2[!file.exists(pathChrom)] <- ""
#}
collectListSorted$System.Time <- substr(collectListSorted$System.Time,2,nchar(as.character(collectListSorted$System.Time)))
substr(collectListSorted$System.Time , 11, 12) <- " "
if(length(pathPdf) >0){
#tempPathsMsMs <-as.vector(as.matrix(tempPaths)[2,])
  tempPaths <- tempPaths
tempPathsLog <- file.exists(Hui <<- paste(finalMQQC,gsub("^./","",tempPaths),sep = "/"))
tempPaths <- paste("<a href ='", tempPaths,"' target ='_blank' >QC</a>",sep = "")
tempPaths[!tempPathsLog] <- ""
#tempPaths[!tempPathsLog] <- ""
# DP pie paths
tempPathsDepPepPie2 <- tempPathsDepPepPie2
tempPathsDepPepPie2Log <<-file.exists(paste(finalMQQC,gsub("^./","",tempPathsDepPepPie2),sep = "/"))
tempPathsDepPepPie2 <- paste("<a href ='", tempPathsDepPepPie2,"'  target ='_blank' >DP</a>",sep = "")
tempPathsDepPepPie2[!tempPathsDepPepPie2Log] <- ""
# Chromatogram Paths
paste(finalMQQC,"files",sep = "/")
pathChrom2Log <- file.exists(paste(finalMQQC,gsub("^./","",pathChrom2),sep = "/"))
tempPathsChrom2 <- paste("<a href ='", pathChrom2,"'  target ='_blank' >CH</a>",sep = "")
tempPathsChrom2[!pathChrom2Log] <- ""


collectListSorted <- as.data.frame(collectListSorted)

colnames(collectListSorted) <- make.names(colnames(collectListSorted))
CoverageVec <- collectListSorted[grep("Coverage",colnames(collectListSorted))[1]]
if(length(CoverageVec) == 0){CoverageVec <- "NA"}
try(collectListSorted <- cbind(	colorCode ,
													input, 
													as.character(collectListSorted$System.Time), 
													collectListSorted$msms.count, 
													#collectListSorted$uniPepCount,
													round(as.numeric(collectListSorted$quan.msms.min),2) ,
													round(as.numeric(collectListSorted$mass.error.uncal.50.),2), 
													collectListSorted$score.50., CoverageVec, tempPaths,tempPathsChrom2,tempPathsDepPepPie2))

try(colnames(collectListSorted) <- c("",colsTemp,"Time","Peptides","MSMS/min","mass_error_[ppm]","Score_M","Coverage","QC","Chrom","DP"))
alignVec <<- c("center","left",rep("center",(dim(collectListSorted)[2]-2)))

#collectListSorted  <- collectListSorted[order(collectListSorted[,7]),]
names(collectListSorted)[is.na(names(collectListSorted))] <- "..."
if(length(collectListSorted) == 0){collectListSorted <- matrix(c(rep("NO DATA",length(alignVec))),byrow = T,1)}
if(it ==1){
	 try(tableHtml2 <-HtmlTable(collectListSorted[!apply(collectListSorted,2,function(x){all(is.na(x))})], tableDesign = "table-design2"))
	if(!exists("tableHtml2")){tableHtml2 <- NULL}
}
if(it ==2){
		 try(tableHtml <- HtmlTable(collectListSorted[!apply(collectListSorted,2,function(x){all(is.na(x))})],  tableDesign = "table-design"))
		 if(!exists("tableHtml")){tableHtml <- NULL}
		 
	}
if(it ==3){
	 try(tableHtml3 <-HtmlTable(collectListSorted[!apply(collectListSorted,2,function(x){all(is.na(x))})], tableDesign = "table-design3"))
	if(!exists("tableHtml3")){tableHtml3 <- NULL}
	
}

}else{	
	if(it == 1){
	tableHtml2 <- ""
	}else{ tableHtml <- ""}
	if(it == 3){
		tableHtml3 <- ""
	}
}
		

}
#####

paste(finalMQQC,"all",paste(Machines,".pdf",sep = ""),sep = "/")
insertText <- list.files(finalMQQC,pattern = "insertText.txt",full.name = T)
if(length(insertText) > 0){
	insertText <- paste(readLines(insertText),collapse = "\n")
}else{
	insertText = ""
}
it <- it+1

}

	if(!exists("tableHtml")){tableHtml 	<<- "NO DATA"}  
	if(!exists("tableHtml2")){tableHtml2 <<- "NO DATA"}  
	if(!exists("tableHtml3")){tableHtml3 <<- "NO DATA"}  
#tableHtml <<- tableHtml # EC
#tableHtml2 <<- tableHtml2 # sample data
#tableHtml3 <<- tableHtml3 # BSA
if(all(Machines == "NA")){
  MachinesUsed = unique(collectListSorted$MS)
}else{MachinesUsed = Machines}
try(writeToHtml(inputVec = sort(paste(".", StandardIDs[1],paste(Machines,".pdf",sep = ""),sep = "/")),inputVec2 = sort(paste(".","all",paste(Machines,".pdf",sep = ""),sep = "/")),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 ,Table3 = tableHtml3, insertText = insertText,Machines = MachinesUsed, StandardIDs = StandardIDs))


try(htmlMod(pathHtml = paste(finalMQQC,"index.html",sep = "/"),Machines = MachinesUsed,Counts = HotLink,BGcolor =as.character(HotLinkCol),pdfPaths = HotLinkPath,Type = HotLinkType))

}
cat("\rfinished FUNFINAL function")
}

#  Param <- mqqcGUI()
#  folder <- Param$folder
#   RESettings <- Param[grep("^RE",names(Param))]
#    StandardIDs = c("","");placeholder = "PLACEHOLDER"
#    LoadSettings(sucFolder="_RmqqcFile_Processed",StandardIDs = c("ECstd","BSA"),finalMQQC=Param$htmloutPath,folder =Param$folder, RESettings = RESettings,Machines = Param$Machines,dayThresh = 5, RESettingsSep = "_")
#      LoadSettings(RESettingsSep = "_",StandardIDs = c("",""), placeholder = "PLACEHOLDER" )
#    funlastLoop = 2
#    try(	FUNFINAL(StandardIDs = c("ECstd","BSA"),finalMQQC=finalMQQC,folder =Param$folder, RESettings = RESettings,Machines = Param$Machines))
# #  system(paste("open ", paste(finalMQQC,"index.html",sep = "/"),sep = ""))
# #finalMQQC <- finalMQQC
# #sucFolder="_RmqqcFile_Processed"

