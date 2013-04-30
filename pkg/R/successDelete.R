successDelete <- 
  function(hotFolder,sucFolder = "_RmqqcFile_Processed",destDelete = F)
  {
    tempI <- list.files(listFolders(hotFolder),pattern = "evidence.txt",full.name = T,recursive = T)
    mqqcInfo <- NULL
    
    if(length(tempI) > 0){
        
        folderName  <- gsub(paste(hotFolder,"/",sep = ""),"",tempI)
        folderName  <- strsplit(folderName,"/")
        folderNameVec <- c()
        for(i in 1:length(folderName)){
          folderNameVec <- c(folderNameVec,folderName[[i]][1])
        }
        
        if(length(tempI)> 0){
          # Check if evidence was already processed if yes, no output of evidence path
          mqqcInfo <- c()
          for(i in 1:length(tempI)){
            tempmqqcInfo  <- list.files(dirname(tempI[i]),pattern = "mqqc",full.name = "T")
            if(length(tempmqqcInfo)!=0){
                dir.create(sucFolderPath <- paste(hotFolder,sucFolder,sep = "/"))
                file.rename(tempmqqcInfo,paste(sucFolderPath,paste(folderNameVec[i],Sys.Date(),sep = "_"),sep = "/"))
                if(destDelete){
                  unlink(paste(hotFolder,folderNameVec[i],sep = "/"),recursive  = T) 
                }
            }
            
          }
          # mqqcInfo   <- mqqcInfo[file.info(mqqcInfo)$isdir]
          
        }
    }
    return(mqqcInfo)
  }