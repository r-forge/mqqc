evidenceCheck <- 
function(hotFolder, match = "evimsms", sucFolder)
{
	collectListPath <- paste(hotFolder,sucFolder,"list_collect.csv",sep = "/")
	if( file.exists(collectListPath)){
	collectList	<- 	read.csv(collectListPath, check.names = F,stringsAsFactors = F)
	imported 		<- collectList[,dim(collectList)[2]]
  	processed  <- unlist(lapply(strsplit(imported,"/combined"),function(x){return(x[1])}))
	}
	folders <- listFolders(hotFolder)
  	folders <- grep("_RmqqcFile_",listFolders(hotFolder),fixed = T,value = T,invert = T)
  	if(exists("processed")){
  		  try(folders <- setdiff(folders,processed))
  	}
  	if(length(folders)> 0){
 	tempI <- list.files(listFolders(folders),pattern = "evidence.txt",full.name = T,recursive = T)
  if(match == "evimsms"){
   		msms <- list.files(dirname(tempI),pattern = "msms.txt",full.name = T,recursive = T)	
   		tempI <- intersect(dirname(tempI),dirname(msms))
   		tempI <- paste(tempI,"evidence.txt",sep = "/")
   }
      
  mqqcInfo <- NULL
  if(length(tempI)> 0){
    # Check if evidence was already processed if yes, no output of evidence path
    mqqcInfo <- c()
    for(i in 1:length(tempI)){
        tempmqqcInfo  <- list.files(dirname(tempI[i]),pattern = "mqqc")
        if(length(tempmqqcInfo)==0){
          mqqcInfo    <- c(mqqcInfo,tempI[i])
        }
      
    }
   # mqqcInfo 	<- mqqcInfo[file.info(mqqcInfo)$isdir]

  }
  }else{mqqcInfo <- NULL}
  return(mqqcInfo)
}

