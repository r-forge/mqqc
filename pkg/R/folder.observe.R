folder.observe <-
function(folder = NULL,MQ = NULL,fastaFile = NULL,fun= mqStarter,temp.name = "test", DeleteFiles = F,cores = NULL,SpeciesTable = T,templateFasta = "._.*_.*_PLACEHOLDER_",placeholder = "PLACEHOLDER",FUNLAST = FUNFINAL,sucFolder = "_RmqqcFile_Processed",htmloutPath = "D:/_RmqqcFile_mqqcHtml",gui = T){
  tkControl()
  
  if(gui){
  	Param <- mqqcGUI()
  	for(i in 1:length(Param)){
		assign(names(Param)[i],Param[[i]])
	}
	cores <- as.numeric(cores)
  	
  	if(!Debug){
  		options(warn = -1,show.error.messages = F, showWarnCalls = F)
  	}else{
  		options(warn = 1,show.error.messages = T, showWarnCalls = T)}
  }
  
  if(.Platform$OS.type == "windows"){
		hui <- initFastaMQ(MQ=MQ,db=fastaFile,SpeciesTable = SpeciesTable)  
}
	
	temp.name <- "test"
	temp.name <- paste("_RmqqcFile_",temp.name,".txt",sep = "")
	
	if(length(folder) == 0){
		folder <- tk_choose.dir(caption = "Please Select your")
    
	}
	folder <<- folder
	# initiation
	setwd(folder)

	init.start 		<- list.files.nodir(folder,pattern = temp.name)
	
	if(length(init.start) == 0){
		catFun("initiate txt")
		files <- list.files.nodir(folder)
		setwd(folder)
		files <- files[! files == temp.name]
		write(files,file = temp.name)
	}else{
		catFun("load init.start")
		files <- readLines(init.start)
		files <- files[! files == temp.name]

	}

	
	loop <- T
	funlastLoop <- 0
	while(loop){
		
	####
	# Check if there is any evidence to process...
	####
	setwd(folder)

	evidenceToProcess <- checkMqqcInfo(folder)
  
  evidenceToProcess <- evidenceCheck(folder)  
	if(length(evidenceToProcess) > 0){
		for(i in 1:length(evidenceToProcess)){
      if(checkSize(evidenceToProcess[i])==0){
      			tkControl(paste(Sys.time(),"Status: Observing", folder),"Processing evidence.txt...")
			  tempI 				<- evidenceToProcess[i]
			  try(qcResults 	<- start.qc(tempI,placeholder=placeholder,templateFasta=templateFasta))
      }
		}
		# deletes folders with evidence.txt and mqqc, mqqc is moved to another folder
		# update export folder   

	}
	
funlastLoop +1

if(funlastLoop == 10){
		funlastLoop  <- 0
		

		if(is.function(FUNLAST)){
		  htmloutPath <<- htmloutPath
		  sucFolder <<- sucFolder
				  FUNLAST(htmloutPath,folder,sucFolder)
			  	  	try(  successDelete(folder,destDelete = DeleteFiles,sucFolder = sucFolder))  

		}

	}else{
		funlastLoop <- funlastLoop +1
	}
setwd(folder)
		
		
		
		catFun(paste(Sys.time(),"Status: Observing", folder))
		Sys.sleep(1)
		# exclude _RmqqcFile_ and use exclusively raw txt
		obs.files			  <- list.files()
   		obs.files <- obs.files[!file.info(obs.files)[,2]]
		temp.obs 			  <- grep("^_RmqqcFile_",obs.files)
	#	temp.obs 			 <- c(temp.obs,grep("raw$|txt$",list.files(),invert = T))
		temp.obs 			<- unique(temp.obs)
		obs.files 			<- grepSubsetControl(temp.obs, obs.files)
		obs.files 			<- grep("raw$|txt$",obs.files,value = T, ignore.case = T)
		obs.files.diff 		<- setdiff(obs.files,files) 
		obs.files.minus 	<- setdiff(files,obs.files) 

		if(length(obs.files) > 0){
			#cat(paste("\rfound.something",obs.files.diff))
			files <- c(files, obs.files.diff)
			write(files,file = temp.name)
			batch <- T
			init.sample <- obs.files
			while(batch){
			  temp.batch   	<- sapply(init.sample, checkSize)
			  temp.batch.n 	<- names(temp.batch)[temp.batch == 0][1]
			  if(!is.na(temp.batch.n)){
			    # starting Maxquant stuff
			    tryError <- class(try(fun(temp.batch.n,folder = folder,cores = cores, SpeciesTable = SpeciesTable, templateFasta = templateFasta, placeholder = placeholder)))
			    if(tryError == "try-error"){
			      #	return(temp.batch.n)
			      catFun(paste("error in file", temp.batch.n))
			    }
			  }
			  init.sample <- setdiff(init.sample,temp.batch.n)
			  #catFun(temp.batch.n)
			  
			  if(length(init.sample) == 0 | is.na(temp.batch.n)){
			    batch <- F
			  }
			  
			}
		#	catFun("Closed Loop")

		}else{
      		
      		if(.Platform$OS.type == "windows"){
		  		MQmanager(NULL,folder,cores =cores)
		  	}else{
		  		tkControl(paste(Sys.time(),"Status: Observing", folder),"")
		  	}
		  
		}
		 
        
		  
		
		

		if(length(obs.files.minus) > 0){
			catFun("Cleanup deleted files")
			for(te in 1:length(obs.files.minus)){
				files <- files[!files == obs.files.minus[te]]
			}
			write(files,file = temp.name)
		}

  
	}
  
  
	
return(folder = folder)	
}


#folder.observe("/users/testmqqc2")
