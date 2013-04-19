xml.replace <-
function(parent,insert,xml,start.string = "<string>",end.string = "</string>" ) {
	
	for(a in parent){
		temp.xml 		<- xml.mod(a,xml)
		temp.range 		<- temp.xml$template
		temp.substr		<- (temp.range[1]+1):(temp.range[2]-1)
		if(length(temp.substr) > 0 ){
			measure.init <- xml[temp.substr[1]]
			measure.init <- unlist.fun(measure.init,"")
			xml			 <- xml[-temp.substr]
			vec.collect <- c()
			for(i in insert){
				temp.i <- paste(paste(rep(" ",measure.init),collapse = "",sep = ""), start.string,i,end.string,collapse = "",sep = "")
				vec.collect <- c(vec.collect,temp.i)
			}
		}
		if(length(insert) > 0){
			xml <- c(xml[1:temp.xml$template[1]],vec.collect,xml[(temp.xml$template[1]+1):length(xml)])
		}
	}
		return(xml)
		 
	
}
