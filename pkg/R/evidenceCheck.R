evidenceCheck <- 
function(hotFolder)
{
  tempI <- list.files(listFolders(hotFolder),pattern = "evidence.txt",full.name = T,recursive = T)
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
  return(mqqcInfo)
}