checkMqqcInfo <- function(hotFolder){
		tempFolders 	<- listFolders(hotFolder)
		tempFiles		<- listFiles(hotFolder)

try(	mqqcinfo 		<- unique(read.table(paste(hotFolder,"mqqcInfo.txt",sep = "/"),header = F,sep = "\t",skip = 0)))
#print(mqqcinfo)	
#print("done")
	return.vec <- c()
	# check Folder
	if(exists("mqqcinfo")){

		mqccinfo.paths 	<- 	(dirname(as.character(mqqcinfo[,1])))
		mqccinfo.paths 	<-	sapply(mqccinfo.paths,basename)
		diff.data		<-  setdiff(mqccinfo.paths, tempFolders)

		if(length(diff.data) > 0){
			mqqcinfo <- mqqcinfo[mqccinfo.paths != diff.data,]
		}
		mqqcinfo[,2] <- 1
		# Check if MQ started
		for(i in 1:length(mqqcinfo[,1])){
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
				return.vec <- c(return.vec, tempI)
			}
		}
	
	#(mqqcinfo)
	write.table(as.matrix(mqqcinfo) ,file = paste(hotFolder,"mqqcInfo.txt",sep = "/"),quote = F,sep = "\t",row.names = F,col.names = F)		

	}
	
	
	
	
	return(return.vec)
	
	
}
