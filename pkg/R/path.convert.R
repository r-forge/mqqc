path.convert <-
function(path){
	if(length(grep("/",path)) > 0){
		path <- gsub("/","\\",path,fixed = T)
		return(path)
	}
	if(length(grep("\\",path,fixed = T)) > 0){
		path <- gsub("\\","/",path,fixed = T)
		return(path)
	}
	
}
