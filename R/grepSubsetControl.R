grepSubsetControl <- 
function(grepVec,obj,mode = 1){
	if(length(grepVec)> 0){
	if(is.vector(obj)){
		obj <- obj[-grepVec]
	}
	if(is.matrix(obj)){
		if(mode == 1){
			obj <- obj[-grepVec,]
		}
		if(mode == 2){
			obj <- obj[,-grepVec]
		}
	}
	}
	return(obj)
	
}
