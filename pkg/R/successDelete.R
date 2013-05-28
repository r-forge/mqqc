successDelete <- 
  function(hotFolder,sucFolder = "_RmqqcFile_Processed",destDelete = F)
  {
    tempI 				<- list.files(listFolders(hotFolder),pattern = "evidence.txt",full.name = T,recursive = T)
    tempImqqc 		<- list.files(listFolders(hotFolder),pattern = "mqqc",full.name = T,recursive = T, include.dirs = T)
    if(length(tempImqqc) > 0){
    tempI  <- tempI[merge.control(dirname(tempI),dirname(tempImqqc))]
    	if(any(!is.na(tempI))){
    		tempI <- tempI[!is.na(tempI)]
    	}
    }
	tempIproc <- sapply(dirname(tempI),list.files,pattern = "mqqcProcessed")
	#tempI <- tempI[!as.logical(sapply(tempIproc,length))]
    mqqcInfo <- NULL
    
    if(length(tempI) > 0){
        repl <- paste(hotFolder,"/",sep = "")
        repl <- gsub("//","/",repl)
        folderName  <- gsub(repl,"",tempI,fixed = T)
        #folderName  <- gsub("//","",folderName,fixed = T)
        
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
            # check if mqqc is in folder
            if(length(tempmqqcInfo)!=0){
  	          write("",paste(dirname(tempI[i]),"mqqcProcessed",sep = "/"))

               dir.create(sucFolderPath <- paste(hotFolder,sucFolder,sep = "/"))
                
               qcData <- list.files(tempmqqcInfo,pattern = ".csv",full.name = T)
                if(length(qcData)> 0){
                	writeName <- paste(hotFolder,sucFolder,"list_collect.csv",sep = "/")
                	for(ba in qcData){    
	                	checkList <- list.files(paste(hotFolder,sucFolder,sep = "/"),pattern = "list_collect.csv")
                		temp <- readLines(ba)  	
                		if(length(checkList) ==  0){
                			write(temp,file = writeName)
                		}else{
                			write(temp[2],file = writeName,append = T)
                		}
                	
                	}
                }
                
                if(any(basename(tempmqqcInfo)  == "mqqcProcessed")& any(basename(tempmqqcInfo)  != "mqqcProcessed")){
                 file.rename(tempmqqcInfo[basename(tempmqqcInfo) !=  "mqqcProcessed" ],paste(sucFolderPath,paste(Sys.Date(),folderNameVec[i],sep = "_"),sep = "/"))
                fileDelete <- paste(hotFolder,folderNameVec[i],sep = "/")
                
	#        listFiles 	<- list.files(fileDelete,recursive = T,full.name = T)
                time.vec 	<- as.numeric(Sys.time()) - as.numeric(file.info(fileDelete)$ctime)
              #  fileDelete <- fileDelete[time.vec > 86400]
                if(time.vec > 86400){
	                  unlink(fileDelete,recursive  = T) 
                }
                
                }
            }
            
          }
          # mqqcInfo   <- mqqcInfo[file.info(mqqcInfo)$isdir]
          
        }
    }
    return(mqqcInfo)
  }