CleaningFun <-
function(folder,Old = "_RmqqcFile_Old"){
# Clean Analysis Folders

Folders <- listFolders(folder)
Folders <- grep("_RmqqcFile_", Folders,value = T,invert = T)
cat("CleaningFun")
sapply(Folders,function(x){
	cat(paste("\r",x))
mqqcTag 					<- list.files(x,pattern = "mqqc", recursive = T)
mqqcMovedTag 		<- list.files(x,pattern = "mqqcMoved", recursive = T)
DeleteTag 				<- list.files(x,pattern = "DeleteTag", recursive = T)
FileInfo 					<- file.info(x)
if(length(mqqcTag) == 2 &length(mqqcMovedTag) == 0 & length(DeleteTag) == 0){


	
}

if(length(mqqcTag) > 0 &length(DeleteTag) == 0 ){
	cat("\rwriting DeleteTag in ",x)
	write("",paste(x,"DeleteTag",sep = "/"))
	
}

Time <- (as.numeric(Sys.time()) -as.numeric(FileInfo$mtime) )/60/60/24
if(Time > 5){
	cat("\rwriting DeleteTag in ",x)
	write("",paste(x,"DeleteTag",sep = "/"))
}	
	
})	
}
