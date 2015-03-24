initFastaMQ <-
function(newFasta = T,db =NULL,MQ=NULL,SpeciesTable = F, default = "auto",fastaInput = NULL,OwnXML = F,standardDB  = T)
{
	
  if(standardDB){
    db <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"fasta$",full.name = T)
  }
  # check MQ path

  checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
  if(length(checkMQ)==0){
    cat("\rChoose MQ Directory!",rep(" ",100))
    MQloop <- T
    
    if(length(MQ)> 0){
    	checkMQ <- MQ
        write(checkMQ,file = paste(path.package("mqqc"),"data","MQpath",sep ="/"))
      checkMQ.bin <- list.files(paste(MQ,"bin",sep = "/"),pattern = "MaxQuantCmd.exe",full.name = T)
      if(length(checkMQ.bin) == 0){
        MQloop = T
      }else{
        MQloop = F
      }
    }
    
    require(tcltk)
    while(MQloop|0){
      
      checkMQ <- tk_choose.dir( caption = "Please select folder containing MQ.")  
      checkMQ.bin <- list.files(paste(checkMQ,"bin",sep = "/"),pattern = "MaxQuantCmd.exe",full.name = T)
      
      if(length(checkMQ.bin) != 0){
        write(checkMQ,file = paste(path.package("mqqc"),"data","MQpath",sep ="/"))
        MQloop = F
        checkMQ.bin <- checkMQ  
      }
    }	
  }else{
    if(length(MQ) > 0){
      checkMQ.bin <- MQ
    }else{
      checkMQ.bin <- readLines(checkMQ)
    }
  }
  
  if(SpeciesTable){
  	try(checkSpeciesTable(checkMQ.bin))
  }
  # check fasta:
  
  mqpar.name  <- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"^mqpar",full.name = T)
  if(SpeciesTable){
    if(length(mqpar.name)== 0){
      newFasta  <- T
    }else{
      newFasta  <- F    
    }
  }
  
  if(length(mqpar.name)== 0&newFasta){
    mqpar.name  <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"init_mqpar_lf.xml",full.name = T)
    if(length(fastaInput) != 0){
      if(file.exists(fastaInput)){
        OwnXML <- T
        mqpar.name <- fastaInput
      }
      
    }
    
    xmlTemplate <- readLines(mqpar.name) 
    Filters <- matrix(c("fasta", ".fasta", "All files", "*"),
                      2, 2, byrow = TRUE)
    loopDB <- T
    while(loopDB ){
      loopDB <- F
      if(length(db)==0){
         db        <- tk_choose.files(filters = Filters,caption = "Select a fasta file for MQ search!")
      }
      if(!file.exists(db)){
        db <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"fasta$",full.name = T)
      }
      
         #db <- db2 
      warning.col <- c()
      warnings1 <- "no"
      warnings2 <- "no"
      for(i in db){
        tempI <- list.files(dirname(i),pattern = basename(i))
        if(length(tempI)== 0){
          warnings1 <- "yes" #warnings1 <- tclvalue(tkmessageBox(message = paste(i,"is not existent.\nDo you like to choose a new fasta file?"),type = "yesno"))
          if((warnings1) == "yes"){
            db <- db[!db== i]
          }
  
          
        }else{print("Found Database")}
       	####
          # Xml
          ####
          
          
          dbLib <- list.files(checkMQ.bin,recursive = T,pattern = "databases.xml",full.name = T)[1]
          
          dbLib <- readLines(dbLib)
          
          
          test.grep <- grep(paste("filename=\"",basename(i),"\"",sep  =""),dbLib,fixed = T)
#           if(length(test.grep)== 0){
#                    warnings2 <- "yes" #<- tkmessageBox(message = paste(i,"is not existent.\nDo you like to choose a new fasta file?"),type = "yesno")
#             if((warnings2) == "yes"){
#               db <- db[!db== i]
#             }
#           }
        warning.col <- c((warnings1),(warnings2))  
      }
      if(any(warning.col=="yes")){
        loopC <- tclvalue(tkmessageBox(message = paste(i,"is not existent.\nRemaining databases:",paste(db,collapse = "\n"),"Do you like to choose a new fasta file?"),type = "yesno"))
        if(loopC == "yes"){
          loopDB <- T
        }
      }
      if(length(db)== 0& !loopDB){
        tkmessageBox(message= "No db loaded, please rerun mqqc and set a proper database for MQ search.")
        stop("no db loaded")
      }
      db <- path.convert(db)
      xmlNew<- xml.replace("fastaFiles",db ,xmlTemplate)  
      # writing XML
      if(OwnXML){
        xmlOutPath <- mqpar.name
      }else{
        xmlOutPath <- paste(path.package("mqqc"),"data/mqpar.xml",sep ="/")
        
      }
      write(xmlNew,xmlOutPath)  
      
    }
    mqpar.name   <-   list.files(path.package("mqqc"),pattern = "^mqpar",recursive = T,full.name = T)
    
    if(length(mqpar.name) == 0 & SpeciesTable){
      mqpar.name <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"init_mqpar",full.name = T)
      file.copy(mqpar.name,paste(dirname(mqpar.name),"mqpar.xml",sep = "/"))
    }
    return(xmlNew)
      
    }
    
}
#hui <- initFastaMQ()
  	#hui <- initFastaMQ(MQ=MQ,db=fastaFile,SpeciesTable = SpeciesTable)  
