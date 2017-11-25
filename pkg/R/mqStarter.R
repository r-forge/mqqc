mqStarter <-
function(temp.batch.n, InfoString = "_RmqqcFile_",folder,...){
  
 
  
  PointGrp <- grep(".",unlist(strsplit(temp.batch.n,"")),fixed = T)
  temp.batch.n <- basename(temp.batch.n)
  if(length(PointGrp) < 0){
    folder.name <-substr(temp.batch.n,1,(max(PointGrp)-1))
  }else{
    folder.name <- paste(temp.batch.n,"folder",sep = "_")
    
  }
	folder.name <- gsub(".","",folder.name,fixed = T)
	if(length(list.files(pattern = folder.name)) > 0){
		folder.name <- paste(folder.name,make.names(Sys.time()),sep = "")			
	}
	setwd(folder)
	dir.create(folder.name)
	write(temp.batch.n,paste(folder.name,"RawFileName.txt",sep = "/"))
	tryError <- class(try(file.rename(basename(temp.batch.n),path.file <-   paste(getwd(),folder.name,"temp.raw",sep = "/")),silent = T))
	Sys.sleep(0.1)
	if(tryError != "try-error"){
	vec <- c(path.file,0,0,0,0)
	if(length(list.files(pattern = paste(InfoString,"Info.txt",sep = "")))== 0){
		try(write.table(t(as.matrix(vec)),file = 	paste(InfoString,"Info.txt",sep = ""),quote = F,sep = "\t",row.names = F,col.names = F),silent =T) 
	}else{
		try(write.table(t(as.matrix(vec)),file = paste(folder,paste(InfoString,"Info.txt",sep = ""),sep = "/"),append = T,quote = F,sep = "\t",row.names = F,col.names = F),silent = T)
	}
	# MQ function plus XML stuff
	mq.fun(filePath = path.file ,folder,...)
	}
	return(path.file)	
}

#temp.batch.n <- "~/Grobi_20131213_HZ_MM_huha.raw"
#InfoString = "_RmqqcFile_"
#templateFasta = "._.*_.*_PLACEHOLDER_"
#placeholder = "PLACEHOLDER"
#1. mqStarter
#2. mq.fun