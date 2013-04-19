xml.mod <-
function(template,xml,gsub.string = "</string>"){
	start 	<- grep(paste("<",template,">",sep = ""),xml,fixed = T)
	stop	<- grep(paste("</",template,">",sep = ""),xml,fixed = T)
	insert.fun 	<- xml[(start+1):(stop-1)]
	
	insert.clean <- c()
	for(i in 1:length(insert.fun)){
		temp.i <- insert.fun[i]
		temp.i <- unlist(strsplit(temp.i,""))
		temp.i.ex <- grep(">",temp.i,fixed = T)
		if(length(temp.i.ex) > 0){
			temp.i <- temp.i[-c(1:temp.i.ex[1])]
		}
		#temp.i.ex <- grep(">",temp.i,fixed = T)
		temp.i <- paste(temp.i,collapse = "")
		temp.i <- gsub(gsub.string,"",temp.i,fixed = T)
		insert.clean[i] <- temp.i
	}
	
	
	return(list(template = c(start,stop),insert = insert.fun,insert.clean = insert.clean ))
	
}
