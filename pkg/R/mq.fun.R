mq.fun <-
  function(filePath,folder,cores=1,SpeciesTable = T,templateFasta = list(REpar = "._.*_.*_PLACEHOLDER_"),placeholder = "PLACEHOLDER",skipUnknown = T,UseOwnXML = F, StandardIDs = c("BSA","ECstd"),GenDB = NULL,testFun = F,MQfilter = "all",GenericDBPath = NULL,MSFRAGGERpath = MSFRAGGERpath,MSFcores = MSFcores,case_sensitive_matching= F,MSFraggerFactor = 1,...){
    RunFile <- T
    continue = T
    filePath <<- filePath
    if(exists("db")){
      if(length(db) == 0){
        db <- NA
      }
      if(!file.exists(as.character(db))){
        db <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "Ecoli.*.fasta$",full.name = T)
      }
    }else{
      db <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "Ecoli.*.fasta$",full.name = T)
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
      try(checkMQ.bin <- readLines(checkMQ),silent = T)
    }
    
    # preparing XML
    assign("filePath",filePath,envir = .GlobalEnv)
    
    
    if(SpeciesTable){
      species <- read.csv(paste(path.package("mqqc"),"data/MQQCspecies.txt",sep = "/"),sep = "\t")
      
      regEx <- sapply(species$Abbreviation,function(x){gsub(placeholder,x, templateFasta$REpar,fixed = T)})
      #regEx <-  paste(regEx,"_",sep = "")
       (fil <-paste(dirname(filePath),"RawFileName.txt",sep = "/"))
      if(file.exists(fil)){
        SearchString <- readLines(fil)[1]
      }else{
        SearchString  <- basename(filePath)
      }
      SearchString 	<-  gsub(".raw$","_raw", SearchString)
      temp   	<- as.logical(sapply(regEx,grep, x = SearchString,ignore.case = !case_sensitive_matching)) #  
      temp[is.na(temp)] <- FALSE
      continue = F
      if(MQfilter == "all"){continue = T}
      if(MQfilter == "db"){
        cat("applying DB filter")
        if(any(temp)){
          continue = T
        }
      }
      
      if(MQfilter == "std"){
        cat("applying std filter")
        
        if(any(species$Abbreviation[temp] == StandardIDs)){
          continue = T
        }
      }
      # cat("applying no filter")
      
      if(!all(!temp)){
        
        speciesUsed <- species[temp,]
        GenDB <- NULL
      }else{
        if(continue){
          speciesUsed <- species[species$Abbreviation == "default",]
          if(is.na(speciesUsed$Fasta)){
            speciesUsed$Fasta <- "THISISARANDOMPATHTHATSHOULDNTEXIST"
          }
          if(!file.exists(as.character(speciesUsed$Fasta))){
            speciesUsed$Fasta <- GenericDBPath
            GenDB <- GenericDBPath
          }else{
            GenDB <- speciesUsed$Fasta
          }
        }else{
          speciesUsed <- NULL
        }
      }	
      # print("HUI")
      # print(speciesUsed)
      if(length(speciesUsed) > 0){
        
        if(dim(speciesUsed)[1] > 1){
          speciesUsed <- speciesUsed[1,]
        }
        
        xmlp <- as.character(speciesUsed$Xml)
        if(length(xmlp) ==0|xmlp == "ToDo"){
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
            write(c("Could not read Fasta",as.character(speciesUsed$Fasta)),file="error.txt")
            RunFile <- F
            SendMailFun(Message = paste("Hi\nyour file",basename(filePath),"was detected by MQQC.\nThere was a problem with the fasta-path which is attached to the parameter-ID.\nYour MQ-QC run was therefore aborted.\nSincerely\nYour MQQC") ,title = paste("MQQC warning:",basename(filePath)),filename = basename(filePath),RESettings = templateFasta)
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
          #try(db <- GenericDBPath)
          
        }
      }else{
        if(skipUnknown){
          
          db <- NA
          print("SKIP")
          
        }else{
          # print("FOR")
          if(length(GenDB) == 0){
            GenDB <-NA 
            try(GenDB <- GenericDBPath)
            
          }
          db <- GenDB 
        }
      }
      
    }
    if(!is.na(db)){
    MQdbadder(checkMQ.bin,db)
    MQdbadder(checkMQ.bin,"GenericDBFirstSearch.fasta")
    mycofasta <- paste(path.package("mqqc"),"data/uniref90mycoOnlyName.fasta",sep = "/")
    MQdbadder(checkMQ.bin,mycofasta)
    
    #check DB
    # if(!is.na(db)){
    # dbXML <- readLines(paste(checkMQ.bin,"/bin/conf/databases.xml",sep = ""))
    # 
    # CheckDB<- grep(paste("filename=\"",basename(db),sep = ""),dbXML)
    # if(length(CheckDB) == 0){
    #   entry <- paste("      <databases filename=\"",basename(db),"\" search_expression=\"(.*)\" replacement_expression=\"%1\" />",sep = "")
    #   entrypoint <- grep("</Databases>",dbXML,ignore.case = T)
    #   XMLOUT <- c(dbXML[1:(entrypoint-1)],entry,dbXML[entrypoint:length(dbXML)])
    #   cat("\rWARNING: added DB to MQ config xml")  
    # }
    # }
    
    if(!UseOwnXML){
      mqpar.name 	<- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"init_mqpar_lf.xml",full.name = T)
      try(mqpar.name2 	<- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"GDB.xml",full.name = T))
      if(length(GenDB) > 0){
        mqpar.name <- mqpar.name2
      }
    }
    if(length(mqpar.name)!=0){
      mqpar   			<- 	readLines(as.character(mqpar.name))
      xmlNEW         <- xml.replace(parent = c("filePaths"),insert = path.convert(filePath),mqpar)
      
      
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
        
        xmlNEW <- xml.replace("fastaFiles",c(db,gsub("/","\\\\",mycofasta)) , xmlNEW)
        
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
    if(speciesUsed$Abbreviation == "default"&!file.exists(as.character(speciesUsed$Fasta))){
      xmlNEW<- gsub("<proteinFdr>.*.</proteinFdr>","<proteinFdr>1</proteinFdr>",xmlNEW)
      xmlNEW<- gsub("<peptideFdr>.*.</peptideFdr>","<peptideFdr>0.5</peptideFdr>",xmlNEW)
      #grep("peptideFdr",xmlNEW,value = T)
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
      print(continue)
      
      #  return(xmlNEW[2])
      cores <- as.numeric(cores)
      if(is.na(cores[1])){cores <- 1;print("Warning, set number of threads to 1.")}
      print(db)
      
      
      if(file.exists(MSFRAGGERpath)){
        MSFcmd <- ""
        try({
          DB <- db # taken from MQstarter
          andromeda <- dirname(filePath) # path with temp.raw, script will select all  rawfiles
          write(rscript_msf(),msfpath<-paste(dirname(filePath),"msfragger_rscript.R",sep= "/"))
          Rscript_system<- "rscript"
          MSFcmd <- paste(Rscript_system,msfpath,MSFRAGGERpath,DB,andromeda,MSFcores,dirname(filePath))
          write(MSFcmd,paste(dirname(filePath),"msfragger_cmd",sep= "/"))
          # system(MSFcmd,wait=F)
        })
        MQcmd <- paste(MQcmd,MSFcmd,sep= "###")
        
      }else{
        print("MSFRAGGER not found. No open search.")
      }
      
      
      
      if(continue){
        try(MQmanager(MQcmd,folder,cores =cores,msfraggerFactor = MSFraggerFactor))
      }
      
      try(   	write(MQcmd,paste(dirname(filePath),"MQQC_MQcmd.txt",sep = "/")))
      
      #initialize MSFRAGGERRUN:
      
      #write("msfragger_started",paste(dirname(x),"msfragger_started",sep = "/"))
      # MSFRAGGERpath
      # MSFcores

      
    }else{
      print("Error in MQ start. No XML provided.")
      if(!file.exists(paste(dirname(filePath),"DeleteTag",sep = "/"))){
        try(write.csv("",paste(dirname(filePath),"DeleteTag",sep = "/")))
      
      SendMailFun(Message = paste("Hi\nyour file",basename(filePath),"was detected by MQQC.\nThere was no match to any parameter ID.") ,title = paste("MQQC warning:",basename(filePath)),filename = basename(filePath),RESettings = templateFasta)
      }
      RunFile <- F
      
      try(   	write.csv("",paste(dirname(filePath),"DeleteTag",sep = "/")))
      
    }
    }else{
      cat("no valid DB Found")
      if(!file.exists(paste(dirname(filePath),"DeleteTag",sep = "/"))){
        try(write.csv("",paste(dirname(filePath),"DeleteTag",sep = "/")))
        print(templateFasta)
        SendMailFun(Message = paste("Hi\nyour file",basename(filePath),"was detected by MQQC.\nThere was no match to any parameter ID.") ,title = paste("MQQC warning:",basename(filePath)),filename = basename(filePath),RESettings = templateFasta)
      }
      RunFile <- F
      }
  }
# filePath <- "hucke_111111_HZ_EC.raw"

# mq.fun(filePath,MQfilter = "std",templateFasta = RESettings)
#tryError <- class(try(mqStarter(temp.batch.n=temp.batch.n,folder = folder,cores = cores, SpeciesTable = SpeciesTable, templateFasta = RESettings, placeholder = placeholder,InfoString = "_RmqqcFile_", StandardIDs = StandardIDs,MQfilter = MQfilter)))
# mq.fun("C:/AG Selbach/mqqc/Statler_20160723_HZ_BSA_FakeFileraw_folderX2016.07.22.13.49.44/Statler_20160723_HZ_BSA_FakeFile.raw",folder = folder)
# GenericDBPath
# LoadSettings(cores=1,SpeciesTable = T,templateFasta = list(REpar = "._.*_.*_PLACEHOLDER_"),placeholder = "PLACEHOLDER",skipUnknown = T,UseOwnXML = F, StandardIDs = StandardIDs,GenDB = NULL,testFun = F)
# mq.fun(filePath="./Tiffy_20151005_HZ_EC_test.raw",folder = folder,StandardIDs = c("ECstd","BSA"),testFun = F )
