xml.mod <-
function(template,xml,gsub.string = "</string>"){
	
	 xmlpos  <- grep(template,xml)
	 if(length(xmlpos) == 1 & any(grepl("<filePaths />",xml))){
	  filePathsInsert <- c("   <filePaths>","<string>C:\\thisisapath\temp.raw</string>","</filePaths>")
	  xml <- c(xml[1:(xmlpos-1)],filePathsInsert,xml[(xmlpos+1):length(xml)])
	  
	  
	 }
	 start 	<- grep(paste("<",template,">",sep = ""),xml,fixed = T)
	 stop	<- grep(paste("</",template,">",sep = ""),xml,fixed = T)
	if(any(start == 0|stop == 0)){
	  warning("Error in XML file at xml.mod.R")   
	}
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
	
	
	return(list(template = c(start,stop),insert = insert.fun,insert.clean = insert.clean ,xml = xml))
	
}
