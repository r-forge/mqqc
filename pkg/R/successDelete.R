successDelete <- 
  function(hotFolder,sucFolder = "_RmqqcFile_Processed",destDelete = F,mqqcInfo = NULL)
  {
    #grep("20131216_AD_ECst",folders)
    folders <- listFolders(hotFolder)
    folders <- folders[grep("^_RmqqcFile",basename(folders),invert = T)]
    collectListPath <- paste(hotFolder,sucFolder,"list_collect.csv",sep = "/")
 
 try(ProgressTracker(folders) )  
    
    # Search only in folders which have not been processed, reduces search space
    if( file.exists(collectListPath)){
      collectList	<- 	read.csv(collectListPath, check.names = F,stringsAsFactors = F)
      collectListInitdim <- dim(collectList)
      collectListclean    <- apply(collectList,2,function(x){all(is.na(x))})
      collectList <- collectList[,!collectListclean]
      if(dim(collectList)[2] < collectListInitdim[2]){
      	write.csv(collectList, collectListPath,row.names = F,quote = F)
      }
      
      
      imported 		<- collectList[,dim(collectList)[2]]
      processed  <- unlist(lapply(strsplit(as.character(imported),"/combined"),function(x){return(x[1])}))
      folders <- setdiff(folders,processed)	
    }
    if(length(folders) > 0){
      tempI 				<- list.files(folders,pattern = "evidence.txt",full.name = T,recursive = T)
      tempImqqc 		<- list.files(folders,pattern = "mqqc",full.name = T,recursive = T, include.dirs = T)
      if(length(tempImqqc) > 0 & length(tempI) > 0){
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
      
      
      
      if(length(tempI) > 0){
      	#preparing Path and foldername
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
            qcData <- list.files(tempmqqcInfo,pattern = ".csv",full.name = T,recursive = T)
            qcData <- qcData[grep("unidentified_conta",basename(qcData),invert = T)]
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
                  #checkListCol <- gsub(";",",",checkListCol)
                  if(checkListCol!= temp[1]){
                   			ImprCheckListCol 	<- unlist(strsplit(checkListCol,","))
                    		ImprCheckListCol  <- gsub("\"","", ImprCheckListCol)
                    		ImprTempCol 			<- unlist(strsplit(temp[1],","))
                    
                   			tempCheckList 	<- read.csv(checkList,quote = "")
                          	NewData     	<- read.csv(ba,quote = "")
                    			newT        <- unique(c(colnames(tempCheckList),colnames(NewData)))
                    			newOrder 	<- merge.control(newT,colnames(tempCheckList))
                    			NewDatMatch 		<- match(newT,colnames(NewData))
                    			NewData <-t(NewData)[NewDatMatch, ]
                    			
                    			
                    			tempCheckListMatch 	<- match(newT ,colnames(tempCheckList))
                    			if(!any(is.na(tempCheckListMatch))){
								              tempCheckList <- tempCheckList[,tempCheckListMatch]
                    			}else{
                    				tempCheckList  <- t(tempCheckList)[tempCheckListMatch,]
                    				rownames(tempCheckList) <- newT
                    				tempCheckList <- t(tempCheckList)
                    			}
                   			  #newOrder <- merge.control(colnames(NewData),colnames(tempCheckList))
                   			
                    			NewOrderNew <- setdiff(1:length(newT), newOrder)
                    			if(length(NewOrderNew) > 0){
                    				newOrder <- c(newOrder, NewOrderNew)
                    			}
                    			NewData <- NewData
                    			tempCheckList <- apply(tempCheckList,2,as.character)
                    			tempCheckListNew <- rbind(tempCheckList,as.character(NewData))
                    			colnames(tempCheckListNew) <- newT
                    		try(write.csv(tempCheckListNew, writeName,row.names = F,quote = F))
        						#write.csv(tempCheckList, writeName,row.names = F,quote = F)
        						
                    #tempCheckList<- cbind(tempCheckList,"")
                    #updateCheckList[is.na(updateCheckList)] <- dim(tempCheckList)[2] 
                    #tempCheckList <- tempCheckList[, updateCheckList]
                    #colnames(tempCheckList) <- ImprTempCol
                    #file.rename(checkList,gsub("list_collect.csv$","list_collect_old.csv",checkList))
                    #write.csv(tempCheckList,checkList,row.names = F,quote = F)
                  }else{
                  try(write(temp[2],file = writeName,append = T))
                  }
                }
                
              }
            }
            # start renaming      
            if(any(list.files(dirname(tempmqqcInfo))  == "mqqcProcessed")& any(basename(tempmqqcInfo)  != "mqqcProcessed")){
              write("",paste(dirname(tempI[i]),"mqqcMoved",sep = "/"))
              MovTest <- tempmqqcInfo[basename(tempmqqcInfo) !=  "mqqcProcessed" & basename(tempmqqcInfo) !=  "mqqcMoved" ]
              DelCont <- F
              if(length(MovTest) > 0){
                	DelCont         <-  file.rename(MovTest[1],paste(sucFolderPath,paste(Sys.Date(),folderNameVec[i],sep = "_"),sep = "/"))
              }
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
    files <- files[grep("^_RmqqcFile",basename(files),value = F,invert = T)]
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


