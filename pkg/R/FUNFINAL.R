FUNFINAL <-
function(finalMQQC = "D:/resultsmqqc",folder,sucFolder="_RmqqcFile_Processed"){
   finalDatPdfMover <- function(dat,fileDir = "files",finalMQQC,folder,sucFolder){
    dir.create(path.temp <- paste(finalMQQC, fileDir,sep = "/"), showWarnings = F)
    allFiles <- list.files(path.temp)
    toMoveFiles <- gsub(".csv",".pdf", basename(as.character(dat$File.Path)))

	#toMoveFiles <- setdiff(toMoveFiles, allFiles)
    
    
    info <- sapply(toMoveFiles,function(x){
      x <<- as.character(x)
      x <- gsub(".csv",".pdf",x)
      x <- list.files(paste(folder,sucFolder,sep = "/"),recursive = T,pattern = basename(x),full.name = T)
      pathEx <- paste(path.temp,basename(x),sep = "/")
      if(!file.exists(pathEx)){
      		file.copy(x,pathEx,overwrite = F)
      }
      rel.path <- paste(".", fileDir,basename(x),sep = "/")
      # the same for MSMS fragment distributions
      MSMSpath <- list.files(dirname(x),pattern = "^MSMS_Dens",full.name = T,recursive = T)
      if(length(MSMSpath) > 0){
        pathEx <- paste(path.temp,basename(MSMSpath),sep = "/")
        if(!file.exists(pathEx)){
        	file.copy(MSMSpath,pathEx , overwrite = F)
        }
        rel.path2 <- paste(".", fileDir,basename(MSMSpath),sep = "/")
      }else{rel.path2 <- ""}
      
      return.temp <- c(unique(rel.path),unique(rel.path2))
      return.temp <- unique(return.temp)
      
      if(length(unlist(return.temp)) != 2){return.temp <- c(rel.path[1],"NA")}
      if(length(unlist(return.temp)) != 2){return.temp <- c("NA","NA")}
      
      return(return.temp)
    
    }
    )
    
  }

stringSplitter <- function(x,maxN = 5,splitter = "_"){
	

  sapply(x,function(x, maxN,splitter){

tempI <- unlist(strsplit(as.character(x),splitter))
tempIlength <- maxN- length(tempI) 
if(tempIlength > 0){
tempI <- c(tempI,rep("NA", tempIlength))
}
if(tempIlength < 0){
	tempI <- c(tempI[1:4],paste(tempI[5:length(tempI)],collapse = "_"))
}
return(tempI)
	
},splitter = splitter,maxN=maxN)
}	
	
returnVec <- c()
dir.create(finalMQQC, showWarnings = F)
dir.create(allPath <- paste(finalMQQC,"all",sep = "/"), showWarnings = F)
dir.create(ECstdPath <- paste(finalMQQC,"ECstd",sep = "/"), showWarnings = F)

if(length(list.files(finalMQQC,pattern = "example.css",recursive = T))== 0){
unzip(list.files(paste(path.package("mqqc"),"data/",sep = "/"),full.name = T,pattern = "zip"),exdir = finalMQQC)	

# from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "example.css")
# to		 <- 	paste(finalMQQC,"example.css",sep = "/")
# try(file.copy(from,to))
# from <- 	list.files(path.package("mqqc"),recursive = T,full.name = T,pattern = "tabber.js")
# to		 <- 	paste(finalMQQC,"tabber.js",sep = "/")
# try(file.copy(from,to))

}


files 			<- list.files(paste(folder,sucFolder,sep = "/"),full.name = T,pattern = ".pdf",recursive = T)
files 			<- files[grep("^MSMS_Dens",basename(files),invert = T)]
filesInfo 	<- file.info(files)
ECstd 		<- grep("._.*_.*_ECstd",basename(files))
ECstdfiles <- filesInfo[ECstd,]
if(length(ECstd) > 0){
  filesInfo <- filesInfo[-ECstd,]
}
icou <- 1

###
# put youngest pdfs into target html folder
###
for(i in list(one = filesInfo,two = ECstdfiles)){

    if(icou == 1){finalPath <- allPath; icou <- icou+1}else{finalPath <- ECstdPath}
  
    machineCol     <- 	unlist(strsplit(basename(rownames(i)),"_.*",fixed = F))
    finalCol   			<- 	unlist(strsplit(list.files(finalPath),"_.*",fixed = F))

    # exclude older files from same machine
    machineCol <- machineCol[order(i$ctime)]
    i <- i[order(i$ctime),]
   # i <- i[!duplicated(machineCol),]
   # machineCol <- machineCol[!duplicated(machineCol)]
    # check if files are older than in folder
    fileFinal <- file.info(list.files(finalPath,full.name = T))
    newD 	<- cbind(machineCol,rownames(i),i$ctime,"new")
    oldD 	<- cbind(finalCol,rownames(fileFinal),fileFinal$ctime,"old")
    
    
    if(length(finalCol)== 0){oldD <- rep(0,4)}
    if(length(machineCol) == 0){newD <- rep(0,4)}
    mixAll <- rbind(oldD,newD)
    mixAll <-mixAll[!mixAll[,1]== "0",]
	if(is.vector(mixAll)){mixAll <- t(as.matrix(mixAll))}
   		test <- sapply(unique(mixAll[,1]),function(x){
      name <<- x
      x <- mixAll[mixAll[,1] == x,]
      if(is.vector(x)){x <- t(as.matrix(x))}
      old <- x[x[,4] == "old",]
      new <- x[x[,4] == "new",]
      
      if(length(new)==0){ new <- rep(0,4)}
      if(length(old)==0){ old <- rep(0,4)}
      
      if(is.matrix(new)&length(new) > 0){
        new <- new[new[,3] == max(new[,3]),]
        if(is.matrix(new)){
        dateExtract	<- unlist(lapply(strsplit(basename(new[,2]),"_"),function(x){x <- x[2]}))
        new <- new[dateExtract == max(dateExtract)]
        	
        }
      }
      if(is.matrix(old)&length(old) > 0){
        old <- old[max(old[,4]),]
      }
      
      if(as.numeric(old[3]) < as.numeric(new[3])){
        
        unlink(old[2])
        paths 		<- new[2]
        finalFile <- paste(finalPath,paste(strsplit(name,"_")[1],".pdf",sep = ""),sep = "/")
        file.copy(paths,finalFile, overwrite = T)
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


###
# prepare tables, bad solved, because pdf and data is treated separately -> problem if older file is processed later
###
pdfFiles 	<- list.files(finalMQQC,pattern = ".pdf",recursive = T)
ECstd 		<- pdfFiles %in% grep("^ECstd", pdfFiles, value = TRUE)
Samples 		<- pdfFiles %in% grep("^all/", pdfFiles, value = TRUE)

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
rest				<- grep("._.*_.*_ECstd",as.character(allData$Name), invert = T) 

ECdata 		<- allData[EC,]
allData			<- allData[rest,]

tempOrd  	<- regexpr("^([^_]*_[^_]*)",allData[,1])
final 			<- substr(allData[,1],start = tempOrd,stop = attributes(tempOrd)$match.length)
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
### create table for all
if(length(finalDat) == 0){tempPaths <- NULL; colorCode <- NULL; tempPathsMsMs2 <- NULL}else{
tempPaths <- finalDatPdfMover(finalDat,finalMQQC = finalMQQC,folder = folder,sucFolder = sucFolder)

tempPathsMsMs <-as.vector(as.matrix(tempPaths)[2,])
tempPaths <- paste("<a href ='", as.matrix(tempPaths)[1,],"' target ='_blank' >MQQC</a>",sep = "")
tempPathsMsMs2 <- paste("<a href ='", tempPathsMsMs,"'  target ='_blank' >MsMs</a>",sep = "")
tempPathsMsMs2[tempPathsMsMs == ""] <- ""

colorCode <- finalDat$TotalScoreColor
colorCode <- paste("<font color = '",colorCode,"'>&#9829;</font>",sep = "")
TotalScore <- round(as.numeric(as.character(finalDat$TotalScore))*100) 
ncharL 		<- 3- nchar(TotalScore)
TotalScoreAdd <- sapply(ncharL,function(x){
	if(x !=0){
		x <- paste(rep(0,x),collapse = "")
		return(x)
	}else{return("")}
	
})
TotalScore <- paste(TotalScoreAdd ,TotalScore,sep = "")
colorCode <- paste(TotalScore,colorCode)
}
temp <- stringSplitter(as.character(finalDat$Name))
temp <- t(temp)
temp <<- temp
if(is.vector(temp)){temp <- as.matrix(temp)}
if(dim(temp)[2] == 5){
temp[,5] <- substr(temp[,5],start = 1,stop = 20)
input 		<- temp

colsTemp <- c("MS","Time","User","Species","misc")
}else{
input <- as.character(finalDat$Name)
colsTemp <- c("Sample")

}

finalDat$System.Time <- substr(finalDat$System.Time,2,nchar(as.character(finalDat$System.Time)))
substr(finalDat$System.Time , 11, 12) <- " "

finalDat <- cbind(colorCode,input, as.character(finalDat$System.Time), finalDat$msms.count,finalDat$uniPepCount,round(as.numeric(finalDat$quan.msms.min),2) ,round(as.numeric(finalDat$mass.error.cal.50),2),finalDat$score.50., tempPaths, tempPathsMsMs2)
cols <- c("",colsTemp,"Time","Peptides","Unique_Peptides","MSMS/min","mass_error_[ppm]","Score_M","","")
colnames(finalDat) <- cols
alignVec <<- c("center","left",rep("center",(dim(finalDat)[2]-2)))

 try(tableHtml <- HtmlTable(finalDat, align= alignVec))

finalDat <<- finalDat
if(!exists("tableHtml")){tableHtml <- NULL}
### table for ECstd 
if(length(finalECdat) == 0){tempPaths <- NULL; colorCode <- NULL; tempPathsMsMs2 <- NULL}else{

tempPaths <- finalDatPdfMover(finalECdat,finalMQQC = finalMQQC,folder = folder,sucFolder = sucFolder)
tempPathsMsMs <-as.vector(as.matrix(tempPaths)[2,])
tempPaths <- paste("<a href ='", as.matrix(tempPaths)[1,],"' target ='_blank' >MQQC</a>",sep = "")
tempPathsMsMs2 <- paste("<a href ='", tempPathsMsMs,"' target ='_blank' >MsMs</a>",sep = "")
tempPathsMsMs2[tempPathsMsMs == ""] <- ""



colorCode <- finalECdat$TotalScoreColor
colorCode <- paste("<font color = '",colorCode,"'>&#9829;</font>",sep = "")
TotalScore <- round(as.numeric(as.character(finalECdat$TotalScore)) *100)

ncharL 		<- 3- nchar(TotalScore)
TotalScoreAdd <- sapply(ncharL,function(x){
	if(x !=0){
		x <- paste(rep(0,x),collapse = "")
		return(x)
	}else{return("")}
	
})

TotalScore <- paste(TotalScoreAdd ,TotalScore,sep = "")
colorCode <- paste(TotalScore,colorCode)
}
#finalECdat <<- finalECdat
temp <- stringSplitter(as.character(finalECdat$Name))
temp <- t(temp)
temp <<- temp
if(is.vector(temp)){temp <- as.matrix(temp)}
if(dim(temp)[2] == 5){
temp[,5] <- substr(temp[,5],start = 1,stop = 20)
input 		<- temp
colsTemp <- c("MS","Time","User","Species","misc")
}else{
input <- as.character(finalECdat$Name)
colsTemp <- c("Sample")
}


finalECdat$System.Time <- substr(finalECdat$System.Time,2,nchar(as.character(finalECdat$System.Time)))
substr(finalECdat$System.Time , 11, 12) <- " "
try(finalECdat <- cbind(colorCode ,input, as.character(finalECdat$System.Time), finalECdat$msms.count, finalECdat$uniPepCount,round(as.numeric(finalECdat$quan.msms.min),2) ,round(as.numeric(finalECdat$mass.error.cal.50),2), finalECdat$score.50., tempPaths, tempPathsMsMs2))


try(colnames(finalECdat) <- c("",colsTemp,"Time","Peptides","Unique_Peptides","MSMS/min","mass_error_[ppm]","Score_M","",""))

 try(tableHtml2 <-HtmlTable(finalECdat, tableDesign = "table-design2"))
if(!exists("tableHtml2")){tableHtml2 <- NULL}



writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[Samples]),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 )

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

maxMachines <- sapply(unique(machines),function(x){
	test 				<- ECdata[machines == x,]
	dateExtract	<- unlist(lapply(strsplit(as.character(test$Name),"_"),function(x){x <- x[2]}))
	test <- test[dateExtract == max(dateExtract),]
	test <- test[test$System.Time.s == max(as.numeric(as.character(test$System.Time.s))),]
	
	return(test[1,])

})
maxMachines <- t(maxMachines)
maxMachines <- as.data.frame(maxMachines)

Machines 	= c("Bibo","Kermit","Grobi","Bert","Tiffy")
FinalCecDat	<- merge.control(rownames(maxMachines),Machines)
Counts  <- maxMachines$msms.count[FinalCecDat]
Counts <- sapply(Counts,as.numeric)
Counts[!as.logical(lapply(Counts,length))] <- "-"

BGcolor <- maxMachines$TotalScoreColor[FinalCecDat]
BGcolor <- sapply(BGcolor,as.character)
BGcolor[!as.logical(lapply(BGcolor,length))] <- "#000000"
if(length(BGcolor)==0){
	
	BGcolor <- rep("#ffffff",length(Counts))
}
try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = Counts,BGcolor =as.character(BGcolor))
)

}
}
return(returnVec)
}
#FUNFINAL(finalMQQC,folder)
#FUNFINAL(htmloutPath,folder)
#FUNFINAL(finalMQQC,folder)
#
#test <- FUNFINAL(folder ="/Users/temp",sucFolder = "_RmqqcFile_Processed",finalMQQC = "/Users/temp/html")

