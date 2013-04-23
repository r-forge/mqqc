mqStarter <-
function(temp.batch.n, mq.fun,InfoString = "_RmqqcFile_",folder){
  PointGrp <- grep(".",unlist(strsplit(temp.batch.n,"")),fixed = T)
  if(length(PointGrp) < 0){
    folder.name <-substr(temp.batch.n,1,(max(PointGrp)-1))
  }else{
    folder.name <- paste(temp.batch.n,"folder",sep = "_")
    
  }
	folder.name <- gsub(".","",folder.name,fixed = T)
	
	
	dir.create(folder.name)
	try(file.rename(temp.batch.n,path.file <-   paste(getwd(),folder.name,temp.batch.n,sep = "/")))
	Sys.sleep(0.1)
	vec <- c(path.file,0,0,0,0)
	if(length(list.files(pattern = paste(InfoString,"Info.txt",sep = "")))== 0){
		write.table(t(as.matrix(vec)),file = 	paste(InfoString,"Info.txt",sep = ""),quote = F,sep = "\t",row.names = F,col.names = F)
	}else{
		write.table(t(as.matrix(vec)),file = paste(InfoString,"Info.txt",sep = ""),append = T,quote = F,sep = "\t",row.names = F,col.names = F)
	}
	# MQ function plus XML stuff
	mq.fun(path.file,folder)
	
	return(path.file)	
}
