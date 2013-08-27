mqStarter <-
function(temp.batch.n, InfoString = "_RmqqcFile_",folder,cores,SpeciesTable,templateFasta,placeholder){
  PointGrp <- grep(".",unlist(strsplit(temp.batch.n,"")),fixed = T)
  if(length(PointGrp) < 0){
    folder.name <-substr(temp.batch.n,1,(max(PointGrp)-1))
  }else{
    folder.name <- paste(temp.batch.n,"folder",sep = "_")
    
  }
	folder.name <- gsub(".","",folder.name,fixed = T)
	if(length(list.files(pattern = folder.name)) > 0){
		folder.name <- paste(folder.name,make.names(Sys.time()),sep = "")			
	}
	
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
	mq.fun(path.file,folder,cores, SpeciesTable,templateFasta,placeholder)
	
	return(path.file)	
}

#temp.batch.n <- "~/Grobi_20131213_HZ_MM_huha.raw"
#InfoString = "_RmqqcFile_"
#templateFasta = "._.*_.*_PLACEHOLDER_"
#placeholder = "PLACEHOLDER"
#1. mqStarter
#2. mq.fun