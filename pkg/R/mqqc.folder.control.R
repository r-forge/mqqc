mqqc.folder.control <-
function(hotFolder){
	cat("\r",hotFolder,rep(" ",100))

	temp.files 	<- list.folders(hotFolder)
	
	if(length(list.files(hotFolder,pattern = "_RmqqcFile_Info.txt")) > 0){
	tryError <- class(try(	mqqcinfo 	<- unique(read.table(paste(hotFolder,"_RmqqcFile_Info.txt",sep = "/"),header = F,sep = "\t"))
))	
	if(tryError != "try-error"){	
		for(i in temp.files){
			tempI 	<- list.files(i, include.dirs = T,pattern = "evidence.txt",recursive = T,full.name = T
			)
		}
	}
	}
	
}
