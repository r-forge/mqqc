search.evidence <-
function(path){
		funTemp 	<- list.files(paste(dirname(path),"combined/txt/",sep = "/"),pattern = "evidence.txt",full.names = T)
	if(length(funTemp) > 0){
		cat("\rfound evidence.txt",rep(" ",100))
		return(funTemp)
	}else{
		funTemp 	<- list.files(dirname(path),pattern = "evidence.txt",full.names = T)
			if(length(funTemp) > 0){
				return(funTemp)
			}else{
				
		cat("\rfound no evidence.txt",rep(" ",100))
		return(NULL)	
			}

	}
}
