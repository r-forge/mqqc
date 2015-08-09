MQmanager <- 
function(MQcmd = NULL,folder,File = "_RmqqcFile_Manager.tMQcmd",cores = 1, StandardIDs = c("ECstd","BSA")){
  
  # Checking cores, if cores is NULL
  if(length(cores) == 0 & .Platform$OS.type == "windows"){
  	try(cores <- system("wmic cpu get NumberOfCores",intern = T))
  
    cores <- as.numeric(unlist(strsplit(cores,"")))
    cores <- cores[!is.na(cores)] -1
    
    if(length(cores) == 0){
      cores <- 1
      
    }
  }
  
  # checking for task txt
  setwd(folder)
  TL1 <- system("tasklist",intern = T)
  TL <- grep("MaxQuantCmd.exe", TL1, ignore.case = T)
  if (length(TL) == 0) {
    TL <- grep(pattern <- "MaxQuantTask.exe", TL1, ignore.case = T)
  }
  if (length(TL) == 0) {
    TL <- grep(pattern <- "MAXQUA~1.EXE", TL1, ignore.case = T)
  }
  
  if(!exists("TL")){TL <- NULL}
   mqqcRunningMQ <<- paste("",length(TL),"/",cores)

  try(tkControl(paste(Sys.time(),"Status: Observing", folder),paste("\nCores used",length(TL),"/",cores),htmloutPath = htmloutPath))  
  TL <- cores - length(TL)
  MQman <- list.files(folder , pattern = File)
  
  if(length(MQcmd) > 0){
    
    if(length(MQman) == 0){
      
      write(MQcmd,File)
    }else{
      
      write(MQcmd,File,append = T)
    }
  }
  MQman <- list.files(folder , pattern = File)
  
  if(length(MQman) > 0){
  
  
  TL <- TL[TL>0]
  if(length(TL) > 0){
    for(i in 1:TL){
      try(tempi <- readLines(File))
      tempi <<- tempi
      first <- grep(paste(StandardIDs,collapse = "|"),tempi)
      if(length(first) > 0){
      	last <- tempi[-first]
      	first <- tempi[first]
      	if(length(last) > 0){
      		firstlast <- c(first,last)
      		if(length(firstlast) > 0 ){
      			tempi <- firstlast 
      		}	
      	}
      }
      if(length(tempi)> 0){
        Sys.sleep(1)
        tempSys <- paste(tempi[1])
        catFun(tempSys)
        tempPath <- unlist(strsplit(unlist(strsplit(tempSys,"\" \""))[2],"\" "))[1]
        if(file.exists(tempPath)){
        	    try(   	write.csv("",paste(dirname(tempPath),"MQStartTag",sep = "/")))

        	
        }
        system(tempSys, wait = F)
        tempi <- tempi[-1]
        
        
        if(length(tempi) == 0){
          
          unlink(File)
        }else{
          write(tempi,File)
        }
        
      }
    }
  }
  }

}

#MQmanager(MQcmd <- "test",folder <- "D:/mqqctest/")