FUNFINAL <-
function(finalMQQC = "D:/resultsmqqc",folder,sucFolder="_RmqqcFile_Processed"){
returnVec <- c()
dir.create(finalMQQC)
dir.create(allPath <- paste(finalMQQC,"all",sep = "/"))
dir.create(ECstdPath <- paste(finalMQQC,"ECstd",sep = "/"))

if(length(list.files(finalMQQC,pattern = "example.css",recursive = T))== 0){
unzip(list.files(paste(path.package("mqqc"),"data/",sep = "/"),full.name = T,pattern = "zip"),exdir = finalMQQC)	

# from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "example.css")
# to		 <- 	paste(finalMQQC,"example.css",sep = "/")
# try(file.copy(from,to))
# from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "tabber.js")
# to		 <- 	paste(finalMQQC,"tabber.js",sep = "/")
# try(file.copy(from,to))

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
       # print(paths)
       # print(finalFile)
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
if(length(allData) > 0){

tryError <- class(try(allData <- unique(read.csv(allData))))
if(tryError == "try-error"){
allDataLines 	<- readLines(allData)
allDataLines2 <- strsplit(allDataLines,",")
length.lines 	<- unlist(lapply(allDataLines2,length))
allDataLines  <- allDataLines[length.lines[1] == length.lines]
write(allDataLines,allData)
allData <- allDataLines	

}
EC				<- grep("._.*_.*_ECstd",as.character(allData$Name)) 
ECdata 		<- allData[EC,]

tempOrd  <- regexpr("^([^_]*_[^_]*)",allData[,1])
final <- substr(allData[,1],start = tempOrd,stop = attributes(tempOrd)$match.length)
allDataOrder <- allData[order(final),]

#allDataOrder <- allDataOrder[order(allDataOrder[,2]),]

machines 	<- unlist(strsplit(as.character(allDataOrder$Name),"_.*"))
finalDat<- c()
for(i in unique(machines)){
	x <- allDataOrder[machines ==i,]
	if(dim(x)[1] > 10){x <- x[1:10,]}
	finalDat <- rbind(finalDat,x)
}	

### Choose for EC 

machinesEC 	<- unlist(strsplit(as.character(ECdata$Name),"_.*"))
finalECdat <- c()
for(i in unique(machinesEC)){
	x <- ECdata[machinesEC ==i,]
	if(dim(x)[1] > 10){x <- x[1:10,]}
	finalECdat <- rbind(finalECdat,x)
}	

finalDat <- cbind(as.character(finalDat$Name), as.character(finalDat$System.Time), finalDat$msms.count,finalDat$uniPepCount,round(as.numeric(finalDat$quan.msms.min),2) ,round(as.numeric(finalDat$mass.error.cal.50),2),finalDat$score.50.)
colnames(finalDat) <- c("Sample","Time","Peptides","Unique Peptides","MSMS/min","mass error in ppm","Score M")

 try(tableHtml <-HtmlTable(finalDat))
if(!exists("tableHtml")){tableHtml <- NULL}


try(finalECdat <- cbind(as.character(finalECdat $Name), as.character(finalECdat$System.Time), finalECdat$msms.count, finalECdat$uniPepCount,round(as.numeric(finalECdat$quan.msms.min),2) ,round(as.numeric(finalECdat$mass.error.cal.50),2), finalECdat$score.50.))
try(colnames(finalECdat) <- c("Sample","Time","Peptides","Unique Peptides","MSMS/min","mass error in ppm","Score M"))

 try(tableHtml2 <-HtmlTable(finalECdat, tableDesign = "table-Design2"))
if(!exists("tableHtml2")){tableHtml <- NULL}




writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[!ECstd]),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 )

# Machines = c("Bibo","Kermit","Grobi","Bert","Tiffy")
# htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts)
####
# modify Html
####
tempOrd  <- regexpr("^([^_]*_[^_]*)", ECdata[,1])
final <- substr(ECdata[,1],start = tempOrd,stop = attributes(tempOrd)$match.length)
allDataOrder <- allData[order(final),]

#allDataOrder <- allDataOrder[order(allDataOrder[,2]),]
machines 	<- unlist(strsplit(as.character(ECdata$Name),"_.*"))
if(length(machines) > 0){
te <- aggregate(as.character(ECdata$Name),list(machines),function(x){
tempOrd  <- regexpr("^([^_]*_[^_]*)",x)
final <- substr(x,start = tempOrd,stop = attributes(tempOrd)$match.length)

x <- x[order(final,decreasing = T)]
print(x)
return(x[1])


	
	
})


MCec 			<- merge.control(ECdata[,1],te[,2])
MCecDat 	<- ECdata[MCec[!is.na(MCec)],]
Machines 	= c("Bibo","Kermit","Grobi","Bert","Tiffy")
FinalCecDat	<- merge.control(te[,1],Machines)
Counts  <- MCecDat$msms.count[FinalCecDat]
BGcolor <- MCecDat$TotalScoreColor[FinalCecDat]
if(length(BGcolor)==0){
	
	BGcolor <- rep("#ffffff",length(Counts))
}
print(Counts)
try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = Counts,BGcolor =as.character(BGcolor))
)

}
}
return(returnVec)
}
		#  FUNFINAL(htmloutPath,folder,sucFolder)

#test <- FUNFINAL(folder = folder,sucFolder = sucFolder)