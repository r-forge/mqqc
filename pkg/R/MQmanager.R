MQmanager <- 
function(x,folder,File = "_RmqqcFile_Manager.txt",cores = 1){
  MQman <- list.files(folder , pattern = File)
  if(cores > 1){cores <- cores -1}
  setwd(folder)
  if(length(MQman) == 0){
    
    write(x,File)
  }else{
    
    write(x,File,append = T)
  }
  
  TL <- system("tasklist",intern = T)
  TL <- grep("MAXQUA",TL)
  TL <- cores - length(TL)
  TL <- TL[TL>0]
  if(length(TL) > 0){
  print(TL)
    for(i in 1:TL){
      print(i)
      try(tempi <- readLines(File))
      #print(tempi)
      if(length(tempi)> 0){
        cat("hui")
        system(tempi[1], wait = F)
        tempi <- tempi[-1]
        
       
        if(length(tempi) == 0){
          print("hui")
          unlink(File)
        }else{
          write(tempi,File)
        }
        
      }
    }
  }
  

}

#MQmanager(x <- "test",folder <- "D:/mqqctest/")