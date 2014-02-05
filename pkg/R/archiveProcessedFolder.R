archiveProcessedFolder <-
function(folder = NULL, sucFolder = "_RmqqcFile_Processed"){
	if(file.exists(folder)){
		setwd(folder)
		setwd(sucFolder)
		temp <- list.files()
		FolderNames <- grep("list_colle", temp,invert = T,value = T)
		tempNameInfo 	<- file.info(FolderNames)
		SelectVec <- as.numeric(Sys.time())-as.numeric(tempNameInfo$mtime) > 7776000
		FolderNames <- FolderNames[SelectVec]
		tempName 	<- substring(FolderNames,1,10)

		tempNameSplit 	<- strsplit(tempName,"-")	
				
		year <- sapply(tempNameSplit,function(x){
			x[1]
		})
		month <- sapply(tempNameSplit,function(x){
			x[2]
		})
		unimonth <- unique(month)
		uniyear <- unique(year)
		if(!file.exists("Archive")){
			dir.create(archiveName<- "MqqcArchive")
		}
		if(!file.exists("MqqcArchive/ArchiveList.txt")){
			write("Folder\tPath","MqqcArchive/ArchiveList.txt")
		}	
		for(y in uniyear){
			FolderNamesY <- FolderNames[uniyear == y]
			unimonthY <- unimonth[uniyear == y ]
			path <- paste(archiveName,y,sep = "/")
			
			if(!file.exists(path)){
				dir.create(path)
			}
			for(m in unimonth){
				pathM <- paste(path,m,sep = "/")
			
				if(!file.exists(pathM)){
					dir.create(pathM)
				}
				FolderNamesYM <- FolderNamesY[unimonthY == m]
				sapply(FolderNamesYM,function(x){
					file.rename(x,NewPath <- paste(pathM,x,sep = "/"))
					write(paste(x,"\t./", NewPath,sep = ""),"MqqcArchive/ArchiveList.txt",append =  T)

				})
			}
		
		}
		
	
	}
}
