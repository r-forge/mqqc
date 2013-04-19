mqStarter <- 
function(temp.batch.n, mq.fun = mq.fun){
	folder.name <-substr(temp.batch.n,1,(max(grep(".",unlist(strsplit(temp.batch.n,"")),fixed = T))-1))
	folder.name <- gsub(".","",folder.name,fixed = T)
dir.create(folder.name)
file.rename(temp.batch.n,path.file <- paste(getwd(),folder.name,temp.batch.n,sep = "/"))
Sys.sleep(0.1)
	vec <- c(path.file,0,0,0,0)
	if(length(list.files(pattern = "mqqcInfo.txt"))== 0){
		write.table(t(as.matrix(vec)),file = "mqqcInfo.txt",quote = F,sep = "\t",row.names = F,col.names = F)
	}else{
		write.table(t(as.matrix(vec)),file = "mqqcInfo.txt",append = T,quote = F,sep = "\t",row.names = F,col.names = F)
	}
	mq.fun(path.file)
	
	
return(path.file)	
}

#REM %MAXQUANTDIR%\bin\MaxQuantCmd.exe %DATADIR% %THREADS%

mq.fun <- function(file){
	# preparing XML
	mqpar.name 	<- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"mqpar",full.name = T)
	mqpar 		<- 	readLines(mqpar.name)
	xmlNEW 		<- 	xml.replace(c("filePaths"), file,mqpar)
	xmlNEW 		<- 	xml.replace(c("fileNames"),basename(file), xmlNEW)
	xmlNEW 		<- 	xml.replace(c("paramGroups"),rep(1,length(file)), xmlNEW,start.string = "<int>",end.string = "</int>")
	# writing XML
	write(xmlNEW,xml.path  <- paste(dirname(file),"mqpar.xml",sep = "/"))
	
	# creating string for system call of MQ
	#check MQ path
	checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
	if(length(checkMQ)==0){
		print("Choose MQ Directory!")
		MQloop <- T
		require(tcltk)
		while(MQloop){
			
			checkMQ <- tk_choose.dir( caption = "Please select folder containing MQ.")	
			print(checkMQ)
			checkMQ.bin <- list.files(paste(checkMQ,"bin",sep = "/"),pattern = "MaxQuantCmd.exe",full.name = T)
			print(checkMQ.bin)

			if(length(checkMQ.bin) != 0){
				write(checkMQ,file = paste(path.package("mqqc"),"data","MQpath",sep ="/"))
				MQloop = F
				
			}
		}	
	}else{
		checkMQ.bin <- readLines(checkMQ)
	}
	threads <- 1
	MQ		<- "MaxQuantCmd.exe"
	
	MQcmd <- paste(checkMQ.bin,"/",MQ," ", xml.path," ",threads,sep = "")
	MQcmd <- path.convert(MQcmd)
	print(MQcmd)
	
	#convert slashes to backslashes
	
	

}


temp.batch.n <-"cloud ler .pdf"

hui <- mqStarter(temp.batch.n,mq.fun)
