checkSpeciesTable <-
  function(){
    checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
    if(length(checkMQ)> 0){
      
      dbLib <- list.files(readLines(checkMQ),recursive = T,pattern = "databases.xml",full.name = T)[1]
      dbLib <- readLines(dbLib)
      species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv",full.name = T,recursive = T)
      file.rename(species.path,species.path2 <- paste(dirname(species.path),"MQQCspecies.csv",sep = "/"))
      species <- read.csv(species.path2)
      
      colBad <- c()
      for(i in 1:length(species$Fasta)){
        test.grep <- grep(paste("filename=\"",basename(as.character(species$Fasta[i])),"\"",sep  =""),dbLib,fixed = T)
        print(test.grep)
        if(length(test.grep) == 0){
          colBad <- c(colBad,i)
        }
        
      }
      
      if(length(colBad) > 0){
        tkFix <- tkmessageBox(type = "yesno",message = paste("Fasta Names are not found in MaxQuant databases.xml:\n",paste(species$Abbreviation[colBad],collapse = "; "),"\nDo you like to fix the table?"))
        if(tclvalue(tkFix) == "yes"){
          fix(species)
          write.csv(species,file =paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"),quote = F, row.names = F)
          
        }
        
      }
      
    }
  }