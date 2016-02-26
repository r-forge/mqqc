mq.fun <-
function(filePath,folder,cores=1,SpeciesTable = T,templateFasta = list(REpar = "._.*_.*_PLACEHOLDER_"),placeholder = "PLACEHOLDER",skipUnknown = T,UseOwnXML = F, StandardIDs = StandardIDs,GenDB = NULL,testFun = F){
RunFile <- T
if(exists("db")){
	if(length(db) == 0){
		db <- NA
	}
  if(!file.exists(as.character(db))){
    db <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"fasta$",full.name = T)
  }
}else{
  db <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"fasta$",full.name = T)
}


# creating string for system call of MQ
	#check MQ path
	checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)

	if(length(checkMQ)==0 & .Platform$OS.type == "windows"){
		cat("\rChoose MQ Directory!",rep(" ",100))
		MQloop <- T
		require(tcltk)
		while(MQloop){
			
			checkMQ <- tk_choose.dir( caption = "Please select folder containing MQ.")	
			checkMQ.bin <- list.files(paste(checkMQ.bin,"bin",sep = "/"),pattern = "MaxQuantCmd.exe",full.name = T)

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
      
    regEx <- sapply(species$Abbreviation,function(x){gsub(placeholder,x, templateFasta$REpar,fixed = T)})
    #regEx <-  paste(regEx,"_",sep = "")
    SearchString <- basename(filePath)
   	SearchString 	<-  gsub(".raw$","_raw", SearchString)
    temp   	<-as.logical(sapply(regEx,grep, x = SearchString))
    temp[is.na(temp)] <- FALSE
  
  if(!all(!temp)){
    
    speciesUsed <- species[temp,]
  }else{
    speciesUsed <- species[species$Abbreviation == "default",]
    speciesUsed$Fasta <- GenericDBPath
  }	
    print("HUI")
    
  if(length(speciesUsed) > 0){
    
    if(dim(speciesUsed)[1] > 1){
      speciesUsed <- speciesUsed[1,]
    }
    
    xmlp <- as.character(speciesUsed$Xml)
    if(length(xmlp) ==0){
      UseOwnXML <- F
    }else{ UseOwnXML <- file.exists(xmlp)}
   
    if(UseOwnXML){
      mqpar.name <- speciesUsed$Xml
    }
    
    try(tkdestroy(tempT),silent = T)
    lastFile <- paste(basename(filePath),paste(unlist(speciesUsed[1,1:3]),collapse = "; "),sep = "\n")
    try(tkControl(NA,NA,lastFile,htmloutPath = htmloutPath))
    
    db <- speciesUsed$Fasta
	  tryError <- class(try(dbControl <- readLines(as.character(speciesUsed$Fasta),n= 1)))
	  
    if(tryError == "try-error" & !testFun){
    
      
      if(skipUnknown){
			try(write.csv("",paste(dirname(filePath),"DeleteTag",sep = "/")))
			        cat("\nError, Could not read fasta, run is aborted.\n")
        RunFile <- F
      }else{
        cat("\nError, Could not read fasta, switched to default database.\n")
        db <- NA
      }
      
      db <- list.files(path.package("mqqc"),pattern = "fasta",recursive = T,full.name =T)
    }else{
      try(write("",paste(dirname(filePath),"MQEnterTag",sep = "/")))
      }
    if(length(grep("/",db,fixed = T)) > 0){
      db <- path.convert(db)
      try(dn <- GenericDBPath)
      
    }
    
  }else{
    if(skipUnknown){
      
    db <- NA
    
    }else{
      if(length(GenDB) == 0){
        GenDB <-NA 
        try(GenDB <- GenericDBPath)
      }
      db <- GenDB 
    }
  }
  
  }

  #check DB
if(!is.na(db)){
dbXML <- readLines(paste(checkMQ.bin,"/bin/conf/databases.xml",sep = ""))

CheckDB<- grep(paste("filename=\"",basename(db),sep = ""),dbXML)
if(length(CheckDB) == 0){
  entry <- paste("      <databases filename=\"",basename(db),"\" search_expression=\"(.*)\" replacement_expression=\"%1\" />",sep = "")
  entrypoint <- grep("</Databases>",dbXML,ignore.case = T)
  XMLOUT <- c(dbXML[1:(entrypoint-1)],entry,dbXML[entrypoint:length(dbXML)])
  cat("\rWARNING: added DB to MQ config xml")  
}
}

  if(!UseOwnXML){
    mqpar.name 	<- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"init_mqpar_lf.xml",full.name = T)
    try(mqpar.name2 	<- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"GDB.xml",full.name = T))
    if(length(GenDB) > 0){
      mqpar.name <- mqpar.name2
    }
  }
  if(length(mqpar.name)!=0){
    mqpar   			<- 	readLines(as.character(mqpar.name))
    xmlNEW         <- xml.replace(c("filePaths"),path.convert(filePath),mqpar)
    
    
    if(length(speciesUsed$DependentPeptides) > 0){
      if(speciesUsed$DependentPeptides){
        mqparDP <- grep("<dependentPeptides>",xmlNEW)
        xmlNEW[mqparDP] <- gsub(">false<",">true<",xmlNEW[mqparDP])
        mqparDP <- grep("allPeptides",xmlNEW,ignore.case = T) 
        xmlNEW[mqparDP] <- gsub("writeAllPeptidesTable=\"false\"","writeAllPeptidesTable=\"true\"",xmlNEW[mqparDP])
      }
    }
    
    MQVER <- gsub("[^0-9,.]", "",grep("maxQuantVersion",mqpar,value = T))
    MQVER <- as.numeric(paste(unlist(strsplit(MQVER,".",fixed = T))[1:2],collapse = "."))
    if(MQVER <1.4){
      xmlNEW 		<- 	xml.replace(c("fileNames"),basename(filePath), xmlNEW)
      xmlNEW 		<- 	xml.replace(c("parameterGroups"),rep(0,length(filePath)), xmlNEW,start.string = "<int>",end.string = "</int>")
      
      # change processFolder
      procFold <- grep("process",xmlNEW)
      input.path <- (dirname((filePath)))
      
      dir.create(paste(input.path,"combined/",sep = "/"))
      dir.create(input.path <-  paste(input.path,"combined/proc",sep = "/"))
      input.path <- path.convert(input.path)
      
      xmlNEW[procFold] <- gsub(paste("processFolder.*",sep =""),"",xmlNEW[procFold])
      xmlNEW[procFold] <- paste(xmlNEW[procFold],paste("processFolder=\"",input.path,"\">",sep = ""))  	
      # writing XML
      
    }
    if(!is.na(db)){
      xmlNEW<- xml.replace("fastaFiles",db , xmlNEW)
      if(length(GenDB) > 0){
        if(!is.na(GenDB)){
          if(any(grepl("<fastaFilesFirstSearch />",xmlNEW))){
            where <- grep("<fastaFilesFirstSearch />",xmlNEW)
            xmlNEWstart <- xmlNEW[1:(where-1)]
            xmlNEWend <- xmlNEW[(where+1):length(xmlNEW)]
            xmlNEWmiddle <- xmlNEW[where]
            xmlNEWmiddle <- c(gsub(" />",">",xmlNEWmiddle,fixed = T))
            xmlNEWmiddle <- c(xmlNEWmiddle,"      <string>/bla.fasta</string>" ,gsub("<fasta","</fasta",xmlNEWmiddle))
            xmlNEW <- c(xmlNEWstart,xmlNEWmiddle,xmlNEWend)
          }
          xmlNEW<- xml.replace("fastaFilesFirstSearch",gsub("GenericDB.fasta$", "GenericDBFirstSearch.fasta",db), xmlNEW) 
        }
      }
      
      
    } 
  }
	  if(speciesUsed$Abbreviation == "default"){
	    xmlNew<- gsub("<proteinFdr>.*.</proteinFdr>","<proteinFdr>1</proteinFdr>",mqpar)
	  }
    if(RunFile){
		  write(xmlNEW,xml.path  <- paste(dirname(filePath),"mqpar.xml",sep = "/"))
  	  
      if(file.exists(db)){
       # try(CheckFastaDB(checkMQ.bin,basename(db)))
      }
    
  		threads <- 1
  		MQ		<- "MaxQuantCmd.exe"
  	  if(MQVER >1.4){
        threads = ""
  	  }
  		MQcmd <- paste("\"",checkMQ.bin,"/bin/",MQ,"\" \"", xml.path,"\" ",threads,sep = "")
  	#	MQcmd <- path.convert(MQcmd)
  
  		#  return(xmlNEW[2])
  		cores <- as.numeric(cores)
  		if(is.na(cores[1])){cores <- 1;print("Warning, set number of threads to 1.")}
  		
      try(MQmanager(MQcmd,folder,cores =cores))
    try(   	write(MQcmd,paste(dirname(filePath),"MQQC_MQcmd.txt",sep = "/")))
  	  
      
  }else{
    print("Error in MQ start. No XML provided.")
	  
    
    try(   	write.csv("",paste(dirname(filePath),"DeleteTag",sep = "/")))
  
  }
}

# mq.fun("./Icke_20151144_HZ_BSAs_biueo",folder = folder,StandardIDs = c("ECstd","BSA"),testFun = F )
