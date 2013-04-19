evidenceCheck <- 
function(hotFolder)
{
  tempI <- list.files(listFolders(hotFolder),pattern = "evidence.txt",full.name = T,recursive = T)
  mqqcInfo <- NULL
  if(length(tempI)> 0){
    # Check if evidence was already processed if yes, no output of evidence path
    mqqcInfo	<- list.files(dirname(tempI),pattern = "mqqc")
    mqqcInfo 	<- mqqcInfo[file.info(mqqcInfo)$isdir]

  }
  if(length(mqqcInfo)  == 0 & length(tempI) > 0){
    return(tempI)
  }else{
    return(NULL)
  }
}