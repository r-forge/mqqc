FUNFINAL <-
function(finalMQQC = "D:/resultsmqqc/",folder,sucFolder="_RmqqcFile_Processed"){
returnVec <- c()
dir.create(finalMQQC)
dir.create(allPath <- paste(finalMQQC,"all",sep = "/"))
dir.create(ECstdPath <- paste(finalMQQC,"ECstd",sep = "/"))

if(length(list.files(finalMQQC,pattern = "example.css",recursive = T))== 0){
	
from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "example.css")
to		 <- 	paste(finalMQQC,"example.css",sep = "/")
try(file.copy(from,to))
from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "tabber.js")
to		 <- 	paste(finalMQQC,"tabber.js",sep = "/")
try(file.copy(from,to))
	
}


files 		<- list.files(paste(folder,sucFolder,sep = "/"),full.name = T,pattern = ".pdf",recursive = T)
filesInfo 	<- file.info(files)
ECstd 		<- grep("._.*_.*_ECstd_",basename(files))
ECstdfiles <- filesInfo[ECstd,]
if(length(ECstd) > 0){
  filesInfo <- filesInfo[-ECstd,]
}
icou <- 1
for(i in list(one = filesInfo,two = ECstdfiles)){

    if(icou == 1){finalPath <- allPath; icou <- icou+1}else{finalPath <- ECstdPath}
  
    machineCol     <- unlist(strsplit(basename(rownames(i)),"_.*",fixed = F))
    finalCol   			<- unlist(strsplit(list.files(finalPath),"_.*",fixed = F))

    # exclude older files from same machine
    machineCol <- machineCol[order(i$ctime)]
    i <- i[order(i$ctime),]
    i <- i[!duplicated(machineCol),]
    machineCol <- machineCol[!duplicated(machineCol)]
    # check if files are older than in folder
    fileFinal <- file.info(list.files(finalPath,full.name = T))
    newD 	<- cbind(machineCol,rownames(i),i$ctime,"new")
    oldD 	<- cbind(finalCol,rownames(fileFinal),fileFinal$ctime,"old")
    
    
    if(length(finalCol)== 0){oldD <- rep(0,4)}
    if(length(machineCol) == 0){newD <- rep(0,4)}
    mixAll <- rbind(oldD,newD)

    test <- sapply(unique(mixAll[,1]),function(x){
      name <- x
      x <- mixAll[mixAll[,1] == x,]
      if(is.vector(x)){x <- t(as.matrix(x))}
      old <- x[x[,4] == "old",]
      new <- x[x[,4] == "new",]
      
      if(length(new)==0){ new <- rep(0,4)}
      if(length(old)==0){ old <- rep(0,4)}
      
      if(is.matrix(new)&length(new) > 0){
        new <- new[max(new[,4]),]
      }
      if(is.matrix(old)&length(old) > 0){
        old <- old[max(old[,4]),]
      }
      
      if(old[3] < new[3]){
        
        unlink(old[2])
        paths 		<- new[2]
        finalFile <- paste(finalPath,paste(name,".pdf",sep = ""),sep = "/")
        print(paths)
        print(finalFile)
        file.copy(paths,finalFile)
      }
      if(exists("finalFile")){return(finalFile)}
      
      
    })
    returnVec <- c(returnVec,test)

    
  #     else{
#       
#     if(is.vector(newD)){
#       newD <- newD[order(newD[,3],decreasing = T),]
#       newD <- newD[!duplicated(newD[,1]),]
#     }
#     test <- sapply(rownames(i),function(x){
#                                    fileName <- list.files(x,pattern = ".pdf",full.name = T)
#                                    dupControl <- list.files(finalPath,pattern = basename(fileName))
#                                    if(length(dupControl) > 0){
#                                      outputFile <- make.names(gsub(".pdf",paste(Sys.time(),".pdf",sep = ""),basename(fileName)))
#                                    }
#                                    file.copy(fileName,paste(finalPath,outputFile,sep = "/"))})
#     
#     
#     }
}

pdfFiles 	<- list.files(finalMQQC,pattern = ".pdf",recursive = T)
ECstd 		<- pdfFiles %in% grep("^ECstd", pdfFiles, value = TRUE)

allData <- list.files(paste(folder,sucFolder,sep = "/"),pattern = "list_collect.csv",full.name = T)
allData <- unique(read.csv(allData))
EC			<- grep(as.character(allData$Name),"._.*_.*_ECstd_") 
ECdata 		<- allData[EC,]

allDataOrder <- allData[order(allData[,1]),]
allDataOrder <- allDataOrder[order(allDataOrder[,2]),]

machines 	<- unlist(strsplit(as.character(allDataOrder$Name),"_.*"))
finalDat<- c()
for(i in unique(machines)){
	x <- allDataOrder[machines ==i,]
	if(dim(x)[1] > 10){x <- x[1:10,]}
	finalDat <- rbind(finalDat,x)
}	

finalDat <- cbind(as.character(finalDat$Name), as.character(finalDat$System.Time), finalDat$msms.count,round(as.numeric(finalDat$quan.msms.min),2) ,round(as.numeric(finalDat$mass.error.cal.50),2),finalDat$score.50.)
colnames(finalDat) <- c("Sample","Time","Peptide Count","MSMS/min","mass error in ppm","Score M")

 try(tableHtml <-HtmlTable(finalDat))
if(!exists("tableHtml")){tableHtml <- NULL}





writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[!ECstd]),path = paste(finalMQQC,"index.html",sep = "/"),table = tableHtml)

return(returnVec)
}
		#  FUNFINAL(htmloutPath,folder,sucFolder)

#test <- FUNFINAL(folder = folder,sucFolder = sucFolder)