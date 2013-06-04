graph.layout.change <-
function(.layout,.names,templates,change.matrix){
	if(length(templates) > 0){
	for(i in 1:length(templates)){
	temp <- grep(templates[i],.names,fixed = T)

	if(length(temp) > 0){
		if(length(temp) > 1){

			temp.2 <- gsub(" ","",.names[temp],fixed = T) 
			temp.3 <- gsub(" ","",templates,fixed = T) 
			temp.4 <- temp.2== temp.3[i]
			temp.vec <- temp[temp.4]
			if(length(temp.vec == 1)&!is.na(temp.vec)){
				temp <- temp.vec
			}else{
				temp <- temp[1]
			}
			}
		
		if(temp > dim(.layout)[1]){
		try(	.layout[temp,] <- .layout[temp,]*change.matrix[i,])
	}else{}
	}
	
	}
	}
	return(.layout)
}
