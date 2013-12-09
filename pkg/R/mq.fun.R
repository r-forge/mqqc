mq.fun <-
function(filePath,folder,cores=1,SpeciesTable = T,templateFasta = "._.*_.*_PLACEHOLDER",placeholder = "PLACEHOLDER",skipUnknown = T){
RunFile <- T
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

   
      
  if(SpeciesTable){
    species <- read.csv(paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"))
      
    regEx <- sapply(species$Abbreviation,function(x){gsub(placeholder,x, templateFasta,fixed = T)})
    
    temp   	<-as.logical(sapply(regEx,grep, x = basename(filePath)))
    temp[is.na(temp)] <- FALSE
  
  if(!all(!temp)){
    
    speciesUsed <- species[temp,]
  }else{
    speciesUsed <- species[species$Abbreviation == "default",]
  }	

  if(length(speciesUsed) > 0){
    
    if(dim(speciesUsed)[1] > 1){
      speciesUsed <- speciesUsed[1,]
    }
    UseOwnXML <- file.exists(as.character(speciesUsed$Xml))
    if(UseOwnXML){
      mqpar.name <- speciesUsed$Xml
    }
    
    try(tkdestroy(tempT))
    lastFile <- paste(basename(filePath),paste(unlist(speciesUsed[1,1:3]),collapse = "; "),sep = "\n")
    try(tkControl(NA,NA,lastFile,htmloutPath = htmloutPath))
    
    db <- speciesUsed$Fasta
    tryError <- class(try(dbControl <- readLines(as.character(speciesUsed$Fasta),n= 1)))
          	stop()

    if(tryError == "try-error"){
    
      
      if(skipUnknown){
      	
        cat("\nError, Could not read fasta, run is aborted.\n")
        RunFile <- F
      }else{
        cat("\nError, Could not read fasta, switched to default database.\n")
      }
      
      db <- list.files(path.package("mqqc"),pattern = "fasta",recursive = T,full.name =T)
    }
    if(length(grep("/",db,fixed = T)) > 0){
      db <- path.convert(db)
    }
    
  }else{
    db <- NA
  }
  
  }

  if(!UseOwnXML){
    mqpar.name 	<- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"^mqpar",full.name = T)
  }
  if(length(mqpar.name)!=0){
    mqpar   			<- 	readLines(mqpar.name)
    xmlNEW 		<- 	xml.replace(c("filePaths"),path.convert(filePath),mqpar)
    xmlNEW 		<- 	xml.replace(c("fileNames"),basename(filePath), xmlNEW)
    xmlNEW 		<- 	xml.replace(c("paramGroups"),rep(1,length(filePath)), xmlNEW,start.string = "<int>",end.string = "</int>")
    # change processFolder
    procFold <- grep("processFolder",xmlNEW)
    input.path <- (dirname((filePath)))
    
    dir.create(paste(input.path,"combined/",sep = "/"))
    dir.create(input.path <-  paste(input.path,"combined/proc",sep = "/"))
    input.path <- path.convert(input.path)
    
    xmlNEW[procFold] <- gsub(paste("processFolder.*",sep =""),"",xmlNEW[procFold])
    xmlNEW[procFold] <- paste(xmlNEW[procFold],paste("processFolder=\"",input.path,"\">",sep = ""))  	
    # writing XML
     
  
	if(!is.na(db)){

    	xmlNEW<- xml.replace("fastaFiles",db , xmlNEW) 
    } 
  }
    
    if(RunFile){
		write(xmlNEW,xml.path  <- paste(dirname(filePath),"mqpar.xml",sep = "/"))
  	
    
  		threads <- 1
  		MQ		<- "MaxQuantCmd.exe"
  	
  		MQcmd <- paste(checkMQ.bin,"/bin/",MQ," ", xml.path," ",threads,sep = "")
  		MQcmd <- path.convert(MQcmd)
  
  		#  return(xmlNEW[2])
  		cores <- as.numeric(cores)
  		if(is.na(cores[1])){cores <- 1;print("Warning, set number of threads to 1.")}
  		MQmanager(MQcmd,folder,cores =cores)
  	
  }else{
    print("Error in MQ start. No XML provided.")
  }
  
	#convert slashes to backslashes
}
#mq.fun("D:/mqqctest/Bert_20130403_PG_Ecoli_salt_16RAW_folder/Bert_20130403_PG_Ecoli_salt_16.RAW","D:/mqqctest/")
#folder <- "D:/mqqctest/"
#filePath <- "D:/mqqctest/Bert_20130403_PG_Ecoli_salt_161RAW_folder/Bert_20130403_PG_Ecoli_salt_161.RAW"
#temp2 <- "D:/mqqctest/Bert_20130403_PG_Ecoli_salt_16RAW_folder/Bert_20130403_PG_Ecoli_salt_16.RAW"