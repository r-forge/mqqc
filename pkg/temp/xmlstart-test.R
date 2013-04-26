initFastaMQ <-
function(x)
{
  newFasta <- T
  # check MQ path
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
  
  # check fasta:
  
  mqpar.name   <- 	list.files(paste(path.package("mqqc"),"data",sep ="/"),"^mqpar",full.name = T)
  if(length(mqpar.name)== 0|newFasta){
    mqpar.name  <- list.files(paste(path.package("mqqc"),"data",sep ="/"),"init_mqpar",full.name = T)
    xmlTemplate <- readLines(mqpar.name) 
    Filters <- matrix(c("fasta", ".fasta", "All files", "*"),
                      2, 2, byrow = TRUE)
    loopDB <- T
    while(loopDB){
      loopDB <- F
     db        <- tk_choose.files(filters = Filters)
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
      
      xmlNew<- xml.replace("fastaFiles",db,xmlTemplate)  
        
      }
      
      
    }
    
}

