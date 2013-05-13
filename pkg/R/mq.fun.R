mq.fun <-
function(filePath,folder){

	# creating string for system call of MQ
	#check MQ path
	checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
	if(length(checkMQ)==0 & .Platform$OS.type == "windows"){
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
    mqpar   	<- 	readLines(mqpar.name)
    xmlNEW 		<- 	xml.replace(c("filePaths"),path.convert(filePath),mqpar)
    xmlNEW 		<- 	xml.replace(c("fileNames"),basename(filePath), xmlNEW)
    xmlNEW 		<- 	xml.replace(c("paramGroups"),rep(1,length(filePath)), xmlNEW,start.string = "<int>",end.string = "</int>")
    # change processFolder
    procFold <- grep("processFolder",xmlNEW)
    #  	print(xmlNew[procFold])
    input.path <- (dirname((filePath)))
    
    dir.create(paste(input.path,"combined/",sep = "/"))
    dir.create(input.path <-  paste(input.path,"combined/proc",sep = "/"))
    input.path <- path.convert(input.path)
    
    xmlNEW[procFold] <- gsub(paste("processFolder.*",sep =""),"",xmlNEW[procFold])
    xmlNEW[procFold] <- paste(xmlNEW[procFold],paste("processFolder=\"",input.path,"\">",sep = ""))  	
    # writing XML
    
    
  	write(xmlNEW,xml.path  <- paste(dirname(filePath),"mqpar.xml",sep = "/"))
  	
    
  	threads <- 1
  	MQ		<- "MaxQuantCmd.exe"
  	
  	MQcmd <- paste(checkMQ.bin,"/bin/",MQ," ", xml.path," ",threads,sep = "")
  	MQcmd <- path.convert(MQcmd)
  
  #  return(xmlNEW[2])
    print(MQcmd)
  	MQmanager(MQcmd,folder,cores =NULL)
  }else{
    print("Error in MQ start. No XML provided.")
  }
  
	#convert slashes to backslashes
}
#mq.fun("D:/mqqctest/Bert_20130403_PG_Ecoli_salt_16RAW_folder/Bert_20130403_PG_Ecoli_salt_16.RAW","D:/mqqctest/")
#folder <- "D:/mqqctest/"
#filePath <- "D:/mqqctest/Bert_20130403_PG_Ecoli_salt_161RAW_folder/Bert_20130403_PG_Ecoli_salt_161.RAW"
#temp2 <- "D:/mqqctest/Bert_20130403_PG_Ecoli_salt_16RAW_folder/Bert_20130403_PG_Ecoli_salt_16.RAW"