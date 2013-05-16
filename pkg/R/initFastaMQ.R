initFastaMQ <-
function(newFasta = T,db =NULL,MQ=NULL,SpeciesTable = F, default = "auto")
{
	

  # check MQ path
  checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
  if(length(checkMQ)==0){
    cat("\rChoose MQ Directory!",rep(" ",100))
    MQloop <- T
    
    if(length(MQ)> 0){
      checkMQ.bin <- list.files(paste(MQ,"bin",sep = "/"),pattern = "MaxQuantCmd.exe",full.name = T)
      if(length(checkMQ.bin) == 0){
        MQloop = T
      }else{
        MQloop = F
      }
    }
    
    require(tcltk)
    while(MQloop){
      
      checkMQ <- tk_choose.dir( caption = "Please select folder containing MQ.")  
      checkMQ.bin <- list.files(paste(checkMQ,"bin",sep = "/"),pattern = "MaxQuantCmd.exe",full.name = T)
      
      if(length(checkMQ.bin) != 0){
        write(checkMQ,file = paste(path.package("mqqc"),"data","MQpath",sep ="/"))
        MQloop = F
        checkMQ.bin <- checkMQ  
      }
    }	
  }else{
    checkMQ.bin <- readLines(checkMQ)
  }
  
  if(SpeciesTable){
  	checkSpeciesTable()
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
    mqpar.name  <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"init_mqpar",full.name = T)
    xmlTemplate <- readLines(mqpar.name) 
    Filters <- matrix(c("fasta", ".fasta", "All files", "*"),
                      2, 2, byrow = TRUE)
    loopDB <- T
    while(loopDB ){
      loopDB <- F
      if(length(db)==0){
         db        <- tk_choose.files(filters = Filters,caption = "Select a fasta file for MQ search!")
      }
         #db <- db2 
      warning.col <- c()
      warnings1 <- "no"
      warnings2 <- "no"
      for(i in db){
        tempI <- list.files(dirname(i),pattern = basename(i))
        if(length(tempI)== 0){
          alarm()
          warnings1 <- "yes" #warnings1 <- tclvalue(tkmessageBox(message = paste(i,"is not existent.\nDo you like to choose a new fasta file?"),type = "yesno"))
          if((warnings1) == "yes"){
            db <- db[!db== i]
          }
  
          
        }
        
          dbLib <- list.files(checkMQ.bin,recursive = T,pattern = "databases.xml",full.name = T)[1]
          dbLib <- readLines(dbLib)
          test.grep <- grep(paste("filename=\"",basename(i),"\"",sep  =""),dbLib,fixed = T)
          if(length(test.grep)== 0){
                   warnings2 <- "yes" #<- tkmessageBox(message = paste(i,"is not existent.\nDo you like to choose a new fasta file?"),type = "yesno")
            if((warnings2) == "yes"){
              db <- db[!db== i]
            }
          }
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
      write(xmlNew,paste(path.package("mqqc"),"data/mqpar.xml",sep ="/"))  
      }
    mqpar.name   <-   list.files(paste(path.package("mqqc"),"data",sep ="/"),"^mqpar",full.name = T)
    
    if(length(mqpar.name) == 0 & SpeciesTable){
      mqpar.name <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"init_mqpar",full.name = T)
      file.copy(mqpar.name,paste(dirname(mqpar.name),"mqpar.xml",sep = "/"))
    }
    return(xmlNew)
      
    }
    
}
#hui <- initFastaMQ()
#initFastaMQ( SpeciesTable = T)