checkMqqcInfo <-
function(hotFolder){
	tempFolders 	<- listFolders(hotFolder)
	tempFiles		  <- listFiles(hotFolder)
		

	return.vec <- c()
		
	# check Folder
	if(length(list.files(hotFolder,pattern = "_RmqqcFile_Info")) > 0){
		try(	mqqcinfo 		<- unique(read.table(paste(hotFolder,"_RmqqcFile_Info.txt",sep = "/"),header = F,sep = "\t",skip = 0)))
		for(i in 1:length(mqqcinfo[,1])){
      
  	mqqcinfo.paths 	<- 	dirname(as.character(mqqcinfo[i,1]))	
		mqqcinfo.paths 	<-	sapply(mqqcinfo.paths,basename)
		diff.data		    <-  setdiff(tempFolders,mqqcinfo.paths)

		if(length(diff.data) > 0){
			mqqcinfo <- mqqcinfo[mqqcinfo.paths != diff.data,]
			mqqcinfo[i,2] <- 1
		}
		# Check if MQ started
		
			tempI <- list.files(dirname(as.character(mqqcinfo[i,1])),pattern = "combined",full.name = T)
			tempI <- tempI[file.info(tempI)$isdir]
			
			if(length(tempI)> 0){
				mqqcinfo[i,3] <- 1
			}
		
		
		# Check if evidence is available
			tempI <- list.files(dirname(as.character(mqqcinfo[i,1])),pattern = "evidence.txt",full.name = T,recursive = T)
			
			tempI <- tempI[!file.info(tempI)$isdir]
			
			if(length(tempI)> 0){
				mqqcinfo[i,4] <- 1
			
			
			# Check if evidence was already processed if yes, no output of evidence path
			mqqcInfo	<- list.files(dirname(tempI),pattern = "mqqc")
			mqqcInfo 	<- mqqcInfo[file.info(mqqcInfo)$isdir]
			
			if(length(mqqcInfo) > 0){
				mqqcinfo[i,5] <- 1
			}else{
				return.vec <- c(return.vec, tempI)
			}
			
			}
		
		
		
		}
	
	#(mqqcinfo)
	write.table(as.matrix(mqqcinfo) ,file = paste(hotFolder,"_RmqqcFile_Info.txt",sep = "/"),quote = F,sep = "\t",row.names = F,col.names = F)		

	}
	
	
	
	return(return.vec)
	
	
}
#checkMqqcInfo(folder)