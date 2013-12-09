successDelete <- 
function(hotFolder,sucFolder = "_RmqqcFile_Processed",destDelete = F)
{
  	folders <- listFolders(hotFolder)
  	folders <- folders[grep("^_RmqqcFile",basename(folders),invert = T)]
  	collectListPath <- paste(hotFolder,sucFolder,"list_collect.csv",sep = "/")
  	# Search only in folders which have not been processed, reduces search space
	if( file.exists(collectListPath)){
	collectList	<- 	read.csv(collectListPath, check.names = F,stringsAsFactors = F)
	imported 		<- collectList[,dim(collectList)[2]]
  	processed  <- unlist(lapply(strsplit(imported,"/combined"),function(x){return(x[1])}))
  	folders <- setdiff(folders,processed)	
	}
	if(length(folders) > 0){
    tempI 				<- list.files(folders,pattern = "evidence.txt",full.name = T,recursive = T)
    tempImqqc 		<- list.files(folders,pattern = "mqqc",full.name = T,recursive = T, include.dirs = T)
    if(length(tempImqqc) > 0){
    tempI  <- tempI[merge.control(dirname(tempI),dirname(tempImqqc))]
    	if(any(!is.na(tempI))){
    		tempI <- tempI[!is.na(tempI)]
    	}
    }
    tempI <- unique(tempI)
	tempIproc 			<- sapply(dirname(tempI),list.files,pattern = "mqqcProcessed")
  	tempIprocMoved <- sapply(dirname(tempI),list.files,pattern = "mqqcMoved")
    
	#tempI <- tempI[!as.logical(sapply(tempIproc,length))]
  tempIproc <- !as.logical(sapply(tempIproc,length)) 
    
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
        
        # Check if evidence was already processed if yes, no output of evidence path
          mqqcInfo <- c()
          for(i in 1:length(tempI)){
            tempmqqcInfo  <- list.files(dirname(tempI[i]),pattern = "mqqc",full.name = "T")
            # check if mqqc is in folder
            if(length(tempmqqcInfo)!=0){
  	          write("",paste(dirname(tempI[i]),"mqqcProcessed",sep = "/"))

               dir.create(sucFolderPath <- paste(hotFolder,sucFolder,sep = "/"), showWarnings = F)
                
               qcData <- list.files(tempmqqcInfo,pattern = ".csv",full.name = T)
                if(length(qcData)> 0&tempIproc[i]){
                	writeName <- paste(hotFolder,sucFolder,"list_collect.csv",sep = "/")
                	for(ba in qcData){    
	                	checkList <- list.files(paste(hotFolder,sucFolder,sep = "/"),pattern = "list_collect.csv",full.name = T)
                		temp <- readLines(ba)
                		temp <- paste(temp,c("File.Path",ba),sep = ",")  	
                		
                		if(length(checkList) ==  0){
                			write(temp,file = writeName)
                		}else{
                			checkListCol <- readLines(checkList,n = 1)
                				if(checkListCol!= temp[1]){
                					ImprCheckListCol 	<- unlist(strsplit(checkListCol,","))
                					ImprCheckListCol  <- gsub("\"","", ImprCheckListCol)
                					ImprTempCol 			<- unlist(strsplit(temp[1],","))

                					updateCheckList <- merge.control(ImprTempCol,ImprCheckListCol)
                					tempCheckList <- read.csv(checkList,quote = "")
                					#tempCheckList<- cbind(tempCheckList,"")
                					#updateCheckList[is.na(updateCheckList)] <- dim(tempCheckList)[2] 
                					#tempCheckList <- tempCheckList[, updateCheckList]
                					#colnames(tempCheckList) <- ImprTempCol
                					#file.rename(checkList,gsub("list_collect.csv$","list_collect_old.csv",checkList))
                					#write.csv(tempCheckList,checkList,row.names = F,quote = F)
                          tempDat <- unlist(strsplit(temp[2],","))[updateCheckList]
                          temp[2] <- paste(tempDat,sep = ",",collapse = ",")
                				}
                			try(write(temp[2],file = writeName,append = T))
                	#	PrepareMail("Title","message","henrik.zauber\\@mdc-berlin.de")
                		
                		}
                	
                	}
                }
      # start renaming      
      if(any(list.files(dirname(tempmqqcInfo))  == "mqqcProcessed")& any(basename(tempmqqcInfo)  != "mqqcProcessed")){
      write("",paste(dirname(tempI[i]),"mqqcMoved",sep = "/"))
       DelCont         <-  file.rename(tempmqqcInfo[basename(tempmqqcInfo) !=  "mqqcProcessed" ],paste(sucFolderPath,paste(Sys.Date(),folderNameVec[i],sep = "_"),sep = "/"))
       if(DelCont){
       	tempmqqcInfo <<- tempmqqcInfo
       	
       	lengthSplitSlash <- length(unlist(strsplit(hotFolder,"/")))
       	tempPath          <- unique(dirname(tempmqqcInfo))
       	
		
       		write("",paste(paste(unlist(strsplit(tempPath,"/"))[1:(lengthSplitSlash +1)],collapse ="/"),"DeleteTag",sep = "/"))
       	
       }
       
      }
      

      
      
            }
            
          }
          # mqqcInfo   <- mqqcInfo[file.info(mqqcInfo)$isdir]
          
        
    }
    }
    files <- list.files(hotFolder,full.name = T)
    files <- grep("^_RmqqcFile",files,value = T,invert = T)
     fileDelete <- list.files(files,pattern = "^DeleteTag$",recursive = T,full.name = T)
     DelFun <- 	function(fileDelete,time.thresh = 86400,move = T, destDelete = F,hotFolder){
                time.vec 	<- as.numeric(Sys.time()) - as.numeric(file.info(fileDelete)$ctime)
              #  fileDelete <- fileDelete[time.vec > 86400]
                if(time.vec >time.thresh){
                	if(destDelete){
                	 	unlink(dirname(fileDelete),recursive  = T) 
                	}
                	if(move&! destDelete){
                						file.rename(paste(hotFolder,basename(dirname(fileDelete)),sep = "/"),paste(hotFolder,"_RmqqcFile_Old",basename(dirname(fileDelete)),sep = "/"))

                	}
                }
             }
             
      if(length(fileDelete) > 0){    
     		if(destDelete){
     			lapply(fileDelete,DelFun,hotFolder = hotFolder, destDelete = T,move = F)     	
     		}else{
     			lapply(fileDelete,DelFun,hotFolder = hotFolder,move = T, destDelete = F)     	
     		}
     	}
	#        listFiles 	<- list.files(fileDelete,recursive = T,full.name = T)
	

    
    return(mqqcInfo)
  }