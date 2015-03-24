speciesFunFix <- 	function(){
		     species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv$",full.name = T,recursive = T)
			 species <- read.csv(species.path[1])
			 try(species <- fix(species))	
			 write.csv(species,file =species.path,quote = F, row.names = F)
         print("wrote Species Table")
		
	}