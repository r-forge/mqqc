mq.fun <-
function(filePath,folder){

	# creating string for system call of MQ
	#check MQ path
	checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
	if(length(checkMQ)==0){
		cat("\rChoose MQ Directory!",rep(" ",100))
		MQloop <- T
		require(tcltk)
		while(MQloop){
			
			checkMQ <- tk_choose.dir( caption = "Please select folder containing MQ.")	
			checkMQ.bin <- list.files(paste(checkMQ,"bin",sep = "/"),pattern = "MaxQuantCmd.exe",full.name = T)

			if(length(checkMQ.bin) != 0){
				write(checkMQ,file = paste(path.package("mqqc"),"data","MQpath",sep ="/"))
				MQloop = F
				
			}
		}	
	}else{
		checkMQ.bin <- readLines(checkMQ)
	}
  
	# preparing XML
	assign("filePath",filePath,envir = .GlobalEnv)
	
	mqpar.name 	<- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"^mqpar",full.name = T)
  if(length(mqpar.name)!=0){
  	mqpar 		<- 	readLines(mqpar.name)
  	xmlNEW 		<- 	xml.replace(c("filePaths"),path.convert(filePath),mqpar)
  	xmlNEW 		<- 	xml.replace(c("fileNames"),basename(filePath), xmlNEW)
  	xmlNEW 		<- 	xml.replace(c("paramGroups"),rep(1,length(filePath)), xmlNEW,start.string = "<int>",end.string = "</int>")
  	# writing XML
  	write(xmlNEW,xml.path  <- paste(dirname(filePath),"mqpar.xml",sep = "/"))
  	
    
  	threads <- 1
  	MQ		<- "MaxQuantCmd.exe"
  	
  	MQcmd <- paste(checkMQ.bin,"/bin/",MQ," ", xml.path," ",threads,sep = "")
  	MQcmd <- path.convert(MQcmd)
  
    
  	MQmanager(MQcmd,folder,cores =NULL)
  }else{
    print("Error in MQ start. No XML provided.")
  }
  
	#convert slashes to backslashes
}
