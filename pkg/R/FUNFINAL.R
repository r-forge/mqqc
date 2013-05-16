FUNFINAL <-
function(finalMQQC = "D:/resultsmqqc/",folder,subfolder){

dir.create(finalMQQC)
dir.create(allPath <- paste(finalMQQC,"all",sep = "/"))
dir.create(ECstdPath <- paste(finalMQQC,"ECstd",sep = "/"))

files <- list.files(paste(folder,sucFolder,sep = "/"),full.name = T)
filesInfo <- file.info(files)
ECstd <- grep("._.*_.*_ECstd_",basename(files))
ECstdfiles <- filesInfo[ECstd,]
if(length(ECstd) > 0){
  filesInfo <- filesInfo[-ECstd,]
}
icou <- 1
for(i in list(one = filesInfo,two = ECstdfiles)){

    if(icou == 1){finalPath <- allPath; icou <- icou+1}else{finalPath <- ECstdPath}
  
    splitI        <- strsplit(basename(rownames(i)),"_",fixed = T)
    splitIfinal   <- strsplit(list.files(finalPath),"_",fixed = T)
    machineCol <- c()
    finalCol <- c()
    for(a in splitI){
      tempA <- a[2]
      machineCol <- c(machineCol,tempA)
    }
    for(a in splitIfinal){
      tempA    <- a[1]
      finalCol  <- c(finalCol,tempA)
    }
    
    # exclude older files from same machine
    machineCol <- machineCol[order(i$ctime)]
    i <- i[order(i$ctime),]
    i <- i[!duplicated(machineCol),]
    machineCol <- machineCol[!duplicated(machineCol)]
    # check if files are older than in folder
    fileFinal <- file.info(list.files(finalPath,full.name = T))
    newD <- cbind(machineCol,rownames(i),i$ctime,"new")
    oldD <- cbind(finalCol,rownames(fileFinal),fileFinal$ctime,"old")
    
    if(length(finalCol)== 0){oldD <- rep(0,4)}
    mixAll <- rbind(oldD,newD)
    test <- sapply(unique(mixAll[,1]),function(x){
      print(x)
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
        paths <- list.files(new[2],pattern = ".pdf",full.name = T)
        file.copy(paths,paste(finalPath,paste(name,".pdf",sep = ""),sep = "/"))
      }
      
      
    })
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
}
