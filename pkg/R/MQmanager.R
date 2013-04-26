MQmanager <- 
function(MQcmd,folder,File = "_RmqqcFile_Manager.tMQcmdt",cores = 1){
  # Checking cores, if cores is NULL
  if(length(cores) == 0){
  try(cores <- system("wmic cpu get NumberOfCores",intern = T))
  
    cores <- as.numeric(unlist(strsplit(cores,"")))
    cores <- cores[!is.na(cores)] -1
    
    if(length(cores) == 0){
      cores <- 1
      
    }
  }
  
  # checking for task txt
  MQman <- list.files(folder , pattern = File)
  setwd(folder)
  TL <- system("tasklist",intern = T)
  TL <- grep("MAXQUA",TL)
  try(tkControl(paste(Sys.time(),"Status: Observing", folder),paste("\nCores used",length(TL),"/",cores)))  
  TL <- cores - length(TL)
  
  if(length(MQcmd) > 0){

    if(length(MQman) == 0){
      
      write(MQcmd,File)
    }else{
      
      write(MQcmd,File,append = T)
    }
  }
  
  if(length(MQman) > 0){
  
  
  TL <- TL[TL>0]
  if(length(TL) > 0){
#    print(TL)
    for(i in 1:TL){
      try(tempi <- readLines(File))
      #print(tempi)
      if(length(tempi)> 0){
        alarm()
        system("start")
        system(tempi[1], wait = F)
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