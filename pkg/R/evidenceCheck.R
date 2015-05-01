evidenceCheck <- 
function(hotFolder, match = "evimsms", sucFolder)
{
	collectListPath <- paste(hotFolder,sucFolder,"list_collect.csv",sep = "/")
	if( file.exists(collectListPath)){
	collectList	<- 	read.csv(collectListPath, check.names = F,stringsAsFactors = F)
	imported 		<- collectList[,dim(collectList)[2]]
  	processed  <- unlist(lapply(strsplit(as.character(imported),"/combined"),function(x){return(x[1])}))
	}
	folders <- listFolders(hotFolder)
  	folders <- grep("_RmqqcFile_",listFolders(hotFolder),fixed = T,value = T,invert = T)
  	if(exists("processed")){
  		  try(folders <- setdiff(folders,processed))
  	}
  	if(length(folders)> 0){
      
 	tempI <- list.files(folders,pattern = "Finish_writing_tables 11.finished",full.name = T,recursive = T)
 	if(length(tempI) == 0){
 	  tempI <- list.files(folders,pattern = "evidence.txt",full.name = T,recursive = T)
    tempInoproc <- file.exists(paste(dirname(dirname(tempI)),"proc",sep = "/"))
    tempI <- tempI[!tempInoproc]
 	}
  tempI <- grep(paste(folders,"_RmqqcFile_",sep = "/"),tempI,invert = T,value = T)
  #tempI <- paste(folders, tempI,sep = "")
  #finishTables  <- list.files(paste(dirname(dirname(tempI)),"proc",sep = "/"),pattern = "Finish_writing_tables 11.finished",full.name = T)

  if(match == "evimsms"& length(tempI) > 0 ){
      tempI <- paste(dirname(dirname(tempI)),"txt/evidence.txt",sep = "/")
      tempI <- gsub("//","/",tempI)
    
   		msms          <- list.files(dirname(tempI),pattern = "msms.txt",full.name = T,recursive = T)	
   		if(length(msms != 0)){
    		tempIt <- intersect(dirname(tempI),dirname(msms))  			
   			if(length(tempIt) > 0){
 	  		tempI <- paste(tempIt,"evidence.txt",sep = "/")
   			}   		
   		}
   		   			
   		
   }
      
  mqqcInfo <- NULL
  if(length(tempI)> 0){
    # Check if evidence was already processed if yes, no output of evidence path
    mqqcInfo <- c()
    for(i in 1:length(tempI)){
    	tempI <<- tempI
        tempmqqcInfo  <- list.files(dirname(tempI[i]),pattern = "mqqc",full.name = T)
        if(length(tempmqqcInfo)==0){
          mqqcInfo    <- c(mqqcInfo,tempI[i])
        }
    }
   # mqqcInfo 	<- mqqcInfo[file.info(mqqcInfo)$isdir]

  }
  }else{mqqcInfo <- NULL}
  return(mqqcInfo)
}

#print(evidenceCheck(hotFolder = "/Users/henno/temp/",sucFolder = "/"))