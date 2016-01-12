folder.observe <-
function(folder = NULL,MQ = NULL,fastaFile = NULL,fun= mqStarter,temp.name = "test", DeleteFiles = F,cores = NULL,SpeciesTable = T,templateFasta = "._.*_.*_PLACEHOLDER",placeholder = "PLACEHOLDER",FUNLAST = FUNFINAL,sucFolder = "_RmqqcFile_Processed",htmloutPath = "D:/_RmqqcFile_mqqcHtml",gui = T,SendMail = T, automatedStart = F,Machines = c("Bibo","Kermit","Grobi","Bert","Tiffy"), StandardIDs = c("ECstd","BSA"),source = "http://cran.us.r-project.org",TabOrd = "source"){
  .GlobalEnv$MQQCRestartNow <- "no"
  try(tkControl(htmloutPath = htmloutPath))
 
  if(length(grep("txtplot",library())) == 0){
	install.packages("txtplot", repos = source)
	}
  if(length(grep("tcltk2",library())) == 0){
    install.packages("tcltk2", repos = source)
  }
  
  

  ## Fox SpeciesTable
  Spec <-list.files(paste(path.package("mqqc"),"/data",sep = ""),full.name = T) 
  if(length(grep("MQQCspecies.csv$",Spec)) == 0){
    if(length(grep("MQQCspecies.csv.gz",Spec))> 0){
      MQQCspectab <- grep("MQQCspecies.csv.gz",Spec,value = T)[1]
      try(suc <- file.rename(MQQCspectab,to = gsub(".gz$","",MQQCspectab)))
      try(suc <- file.rename(MQQCspectab,to = gsub(".gz2$","",MQQCspectab)))
      
      if(suc){
      file.remove(MQQCspectab)
      }
      MQQCspectab <- grep("MQQCspecies.csv.bz",Spec,value = T)[1]
      
      try(suc <- file.rename(MQQCspectab,to = gsub(".bz$","",MQQCspectab)))
      try(suc <- file.rename(MQQCspectab,to = gsub(".bz2$","",MQQCspectab)))
      if(suc){
        file.remove(MQQCspectab)
      }
    
    }  
  }

  # Modifiying Species, Add DP
  
  
  if(length(grep("contaminants.csv$",Spec)) == 0){
    if(length(grep("contaminants.csv",Spec))> 0){
      MQQCspectab <- grep("contaminants.csv.gz",Spec,value = T)[1]
      try(suc <- file.rename(MQQCspectab,to = gsub(".gz$","",MQQCspectab)))
      try(suc <- file.rename(MQQCspectab,to = gsub(".gz2$","",MQQCspectab)))
      if(suc){
        file.remove(MQQCspectab)
      }
      
      MQQCspectab <- grep("contaminants.csv.bz",Spec,value = T)[1]
      try(suc <- file.rename(MQQCspectab,to = gsub(".bz$","",MQQCspectab)))
      try(suc <- file.rename(MQQCspectab,to = gsub(".bz2$","",MQQCspectab)))      
      if(suc){
        file.remove(MQQCspectab)
      }
    }
  }
  
  

#     if(length(grep("MQQCspecies.csv.gz")) > 0){
#       
#     }
#   }
  
	if(length(grep("widgetTools",library()))== 0){
	  try(source("http://bioconductor.org/biocLite.R"))
	  
	try(biocLite("widgetTools",ask ="n"))	
	}
	
	require("widgetTools")	
	require("txtplot")
  require("tcltk")
 
 
  
  ###???
  # Check MailList
  ###
  print("Checking Mail Settings")
  MailFile  <- list.files(MailPath<- paste(path.package("mqqc"),"data",sep = "/"),pattern = "^MailSettings$")
  if(length(MailFile) == 0){
  	write(paste("username","password","smtp.server","emailaddress",sep = "\n"),file = paste(MailPath,"MailSettings",sep = "/"))
  	
  }
  
    print("Initiate Settings")

if(automatedStart){
	Tryerror<- class(try(load(file=paste(path.package("mqqc"),"data/Param.Rdata",sep = "/"))))
	if(Tryerror== "try-error"){
			print("Error, could not auto start. No Param.Rdata available.")
			stop()
		}
	Param <- output
		for(i in 1:length(Param)){
			assign(names(Param)[i],Param[[i]])
		}
}else{
  
 	if(gui){
	Param <- mqqcGUI()
	while(.GlobalEnv$MQQCRestartNow == "yes"){
  		Param <- mqqcGUI()
	}
      print("Settings received")
	
  	
  	for(i in 1:length(Param)){
		  assign(names(Param)[i],Param[[i]])
	  }
	  
	  if(exists("REpar")){
	  		  			  	RESettings <- Param[grep("^RE",names(Param))]

	  	if(length(grep(placeholder,REmac)) >0){
	  	}
	  }else{print("Could't find input for regular expression Rules.")}
	  
      print("Settings loaded")
	  
	cores <- as.numeric(cores)
  	if(is.na(Debug)){Debug <- TRUE}
  	if(!Debug){
  		options(warn = -1,show.error.messages = F, showWarnCalls = F)
  	}else{
  		options(warn = 1,show.error.messages = T, showWarnCalls = T)
  		}
  }

}
StandardIDs = c(Param$StdIDhigh,Param$StdIDlow)
if(file.exists(as.character(Param$MQ))){
  write(Param$MQ,paste(path.package("mqqc"),"data","MQpath",sep ="/")) 
}
	 print("Preparing MQQC")
    if(!file.exists(htmloutPath)){
      htmloutPath <- paste(folder,paste("_RmqqcFile_html"),sep = "/")
      dir.create(htmloutPath)
    }
    try(PIDhtmltable(htmloutPath))
    try(writeToHtml(path =paste(htmloutPath,"index.html",sep = "/"),Machines = Param$Machines),silent = T)
    dir.create(paste(folder,"_RmqqcFile_Old",sep = "/"), showWarnings = F)

    dbPath <- paste(folder,paste("_RmqqcFile_databases"),sep = "/")
    
  if(!file.exists(dbPath)){
    dir.create(dbPath)
  }
    cat("Setting up Generic Database")
    GenericDBPathCounts <<- paste(dbPath,"NameCounts.rda",sep = "/")
    GenericDBPath <<- paste(dbPath,"GenericDB.fasta",sep = "/")
    if(!file.exists(GenericDBPath)){
    data(ArtificDB)
    data(NameCounts)
    data(ApexCutsID)
    na <- grep(">",ArtificDB,fixed = T)
    naN <-  ArtificDB[na]
    naM <- match(ApexCutsID[[1]],gsub(">","",naN))
    naS <- na[naM]
    Suse <- naS+1
    FiNa <- sort(c(naS,Suse))
    ArtificDBFirstSearch <- ArtificDB[FiNa]
    write(ArtificDB,GenericDBPath )
    write(ArtificDBFirstSearch,paste(dbPath,"GenericDBFirstSearch.fasta",sep = "/"))
#     cat("Collecting Names in Generic Database")
#     FaNa <- grep(">",ArtificDB,value = T,fixed = T)
#     if(!exists(FaNa)){
#     cat("Splitting Names in Generic Database")
#     FaNa <- strsplitslot(FaNa,1,"_")
#     FaNa <- table(FaNa)
#     names(FaNa) <- gsub("^>","",names(FaNa))
#     }
    
    try(rm(ArtificDB))
    cat("Setting up Generic Database Done")
    }
    
    
  if(!file.exists(fastaFile)){fastaFile <- NULL}
  if(.Platform$OS.type == "windows"){
		try(hui <- initFastaMQ(MQ=MQ,db=fastaFile,SpeciesTable = SpeciesTable))  
		###
		# REG QUery HKEY_CLASSES_ROOT\MSFileReader.XRawfile
		###
		Registry <- system("REG QUery HKEY_CLASSES_ROOT",intern = T)
		if(length(grep("MSFileReader",Registry,ignore.case=T,value = T)) == 0){
		  CheckVal <- tclvalue(tkmessageBox(message = "Warning!\nMSFileReader seems not to be installed. Continue without?",type = "yesno",title = "MQQC Warning",icon = "warning"))
		  if(CheckVal == "no"){
        stop("Abort by user. Missing MSFileReader installation.")
		  } 
		} 
		if(length(grep("Perl",Registry,ignore.case=T,value = T)) == 0){
		  CheckVal <- tclvalue(tkmessageBox(message = "Warning!\nPerl seems not to be installed. email alerts will be switched off.",type = "ok",title = "MQQC Warning",icon = "warning"))
		  if(CheckVal == "no"){
		    stop("Abort by user. Missing MSFileReader installation.")
		  } 
		}else{
			try(system("ppm install Net::SMTP"),silent = T)
			try(system("ppm install Authen::SASL"),silent = T)
		} 
  }

  print("Checking Species Table")
 
  if(SpeciesTable){
    
    species <- read.csv(paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"))
    dpruns <- species$DependentPeptides == 0
    dpcounter <- sapply(species$Abbreviation[dpruns],function(x){any(grepl(x,as.character(species$Abbreviation[!dpruns])))})
    addTab <- species[dpruns,][!dpcounter,]
    if(dim(addTab)[1] > 0){
      
      addTab$DependentPeptides <- 1
      addTab$Abbreviation <- paste(addTab$Abbreviation ,"dp",sep = "")
      species <- rbind(species,addTab)
      species <- species[order(species$Abbreviation),]
      species <- unique(species)
      write.csv(species,paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"),quote = F,row.names = F,sep = ",")
    }
    XMLCheck <- species$Xml[file.exists(as.character(species$Xml))]
    if(length(XMLCheck) > 0){
      sapply(XMLCheck,function(x){
        #try(initFastaMQ(MQ=MQ,db=fastaFile,SpeciesTable = SpeciesTable,fastaInput=x))       
      })
    }
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

	print("Starting Loop")
	print("")

	loop <<- T
	funlastLoop <<- 0
while(loop){
		
	####
	# Check if there is any evidence to process...
	####
	setwd(folder)
	funlastLoop  <<- funlastLoop +1
	
	#####
	# Initiation of MQ runs, if new Raw File in Folder was detected
	####
	if(funlastLoop %% 2 == 0){
		cat("\r Searching for MQ Results")
		try(ThreadControl(folder),silent = T)
	#evidenceToProcess <- checkMqqcInfo(folder)
 	 
  	evidenceToProcess <- evidenceCheck(folder,sucFolder = sucFolder)  # Takes long with many undeleted folders
	if(length(evidenceToProcess) > 0){
		for(i in 1:length(evidenceToProcess)){
			try(SizeChecked <- checkSize(evidenceToProcess[i])==0)
			if(!exists("SizeChecked")){SizeChecked <- F}
      if(SizeChecked){
      			tkControl(paste(Sys.time(),"Status: Observing", folder),"Processing evidence.txt...", htmloutPath = htmloutPath)
			  tempI 				  <- evidenceToProcess[i]
			  try(qcResults 	<- start.qc(tempI,placeholder=placeholder,RESettings= RESettings,SendMail= SendMail,exitPath = paste(folder,sucFolder,sep = "/"),BSAID = Param$BSAID))
      }
		}
		# deletes folders with evidence.txt and mqqc, mqqc is moved to another folder
		# update export folder   

	} # evidence loop
			cat("\r Searching for MQ Results done")

	} # funlastLoop

if(funlastLoop %% 2 == 0){
#		hi<- file.info(list.files())
#	hu<- 	order(hi[,4])
 	cat("\r Cleaning Folder")
   
  htmloutPath <<- htmloutPath
  	  				try(  successDelete(hotFolder =folder,destDelete = DeleteFiles,sucFolder = sucFolder))  
	sucFolder <<- sucFolder

 	cat("\r Updating Table")
					try(	FUNFINAL(finalMQQC=htmloutPath,folder =folder,sucFolder = sucFolder, RESettings = RESettings, Machines = Param$Machines, StandardIDs = StandardIDs,ordertype = TabOrd))
		
  
	}
if(funlastLoop %% 20 == 0){
  try(PIDhtmltable(htmloutPath))
}
setwd(folder)		
		
		catFun(paste(Sys.time(),"Status: Observing", folder))
		#Sys.sleep(5)
    for(xSleepTime in 1:10){
      xSleepTime <<- xSleepTime
      Sys.sleep(1)
      cat("\rSleeping",xSleepTime,"s")
      
    }

		# exclude _RmqqcFile_ and use exclusively raw txt
		obs.files			  <- list.files(folder,full.name = T)
   		obs.files      		 <- obs.files[!file.info(obs.files)[,2]]
		temp.obs 			  <- grep("^_RmqqcFile_",obs.files)
	#	temp.obs 			  <- c(temp.obs,grep("raw$|txt$",list.files(),invert = T))
		temp.obs 			  <- unique(temp.obs)
		obs.files 			<- grepSubsetControl(temp.obs, obs.files)
		obs.files 			<- grep("raw$|txt$",obs.files,value = T, ignore.case = T)
		obs.files.diff 		<- setdiff(obs.files,files) 
		obs.files.minus 	<- setdiff(files,obs.files) 
    	obs.files <- obs.files[grep("^_RmqqcFile_",basename(obs.files),invert = T)]
		
		if(length(obs.files) > 0){
		 	cat("\r Starting MQ run for", obs.files)	
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
			    tryError <- class(try(fun(temp.batch.n=temp.batch.n,folder = folder,cores = cores, SpeciesTable = SpeciesTable, templateFasta = RESettings, placeholder = placeholder,InfoString = "_RmqqcFile_", StandardIDs = StandardIDs)))
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
		#	cat SpeciesTable"Closed Loop")

		}else{
		 	cat("\r Checking MQ Queue", obs.files)	      		
      		if(.Platform$OS.type == "windows"){
		  		MQmanager(NULL,folder,cores =cores)
		  	}else{
		  		tkControl(paste(Sys.time(),"Status: Observing", folder),"", htmloutPath = htmloutPath)
		  	}
		 	cat("\r Checking MQ Queue DONE", obs.files)	      		
		  
		}
		 
        
		  
		
		

		if(length(obs.files.minus) > 0){
			catFun("Cleanup deleted files")
			for(te in 1:length(obs.files.minus)){
				files <- files[!files == obs.files.minus[te]]
			}
			write(files,file = temp.name)
		}

	if(funlastLoop %% 8640 == 0){
  		try(CleaningFun(folder))
  		try(archiveProcessedFolder(folder))
  	}
  		
	}
  	
  
	
return(folder = folder)	
}

#folder.observe("/users/testmqqc2")
