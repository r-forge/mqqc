speciesFunFix <- 	function(){
		     species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.txt$",full.name = T,recursive = T,sep = "\t")
			 species <- read.csv(species.path[1])
			 try(species <- fix(species))	
			 write.table(species,file =species.path,quote = F, row.names = F,sep = "\t")
         print("wrote Species Table")
		
	}