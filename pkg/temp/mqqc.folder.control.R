mqqc.folder.control <- function(hotFolder){
	print(hotFolder)

	temp.files 	<- list.folders(hotFolder)
	
	
	mqqcinfo 	<- unique(read.table(paste(hotFolder,"mqqcInfo.txt",sep = "/"),header = F,sep = "\t"))
	

	
	for(i in temp.files){
		print(i)
		tempI 	<- list.files(i, include.dirs = T,pattern = "evidence.txt",recursive = T,full.name = T
		)
		print(tempI)
	}
	
}


listFolders 	<- function(folder){
	temp.files 	<- file.info(list.files(folder))
	print(temp.files)
	temp.files 	<- rownames(temp.files)[temp.files$isdir]
}
listFiles 	<- function(folder){
	temp.files 	<- file.info(list.files(folder))
	temp.files 	<- rownames(temp.files)[!temp.files$isdir]
}

#mqqc.folder.control(hotFolder<- "~/documents/Skripte/R-standard-WD/test-folder")
temp.files 	<- listFiles(hotFolder)
temp.folder <- listFolders(hotFolder)
#mqqcinfo 	<- unique(read.table(paste(hotFolder,"mqqcInfo.txt",sep = "/"),header = F,sep = "\t"))