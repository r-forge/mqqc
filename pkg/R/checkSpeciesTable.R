checkSpeciesTable <-
  function(checkMQ){
    if(length(checkMQ)> 0){
      
      dbLib <- list.files(checkMQ,recursive = T,pattern = "databases.xml",full.name = T)[1]
      dbLib <- readLines(dbLib)
      species.path <- list.files(path.package("mqqc"),pattern = "MQQCspecies.txt",full.name = T,recursive = T)
      file.rename(species.path,species.path2 <- paste(dirname(species.path),"MQQCspecies.txt",sep = "/"))
      species <- read.csv(species.path2,sep = "\t")
      
      colBad <- c()
      for(i in 1:length(species$Fasta)){
        test.grep <- grep(paste("filename=\"",basename(as.character(species$Fasta[i])),"\"",sep  =""),dbLib,fixed = T)
        hm <- gsub(" ","",dbLib[test.grep])


        if(length(test.grep) == 0|!file.exists(as.character(species$Fasta[i]))){
          colBad <- c(colBad,i)
        }
        
      }
      
      if(length(colBad) > 0){
      	tkFix <- tkmessageBox(type = "yesnocancel",icon = "question",message = "Could not resolve paths to fasta files. Fix paths?")
        if(tclvalue(tkFix) == "no"){          fix(species)
          write.table(species,file =paste(path.package("mqqc"),"data/MQQCspecies.txt",sep = "/"),quote = F, row.names = F,sep = "\t")
          
        }
        if(tclvalue(tkFix) == "yes"){
        	try(speciesTK(species))	
        	 if(exists("mqqcSpeciesSet",envir = .GlobalEnv)){
        		species$Fasta <- .GlobalEnv$mqqcSpeciesSet
              write.txt(species,file =paste(path.package("mqqc"),"data/MQQCspecies.txt",sep = "/"),quote = F, row.names = F,sep = "\t")
        	}
        }
       
      
      }
      
    }
  }
#checkSpeciesTable()