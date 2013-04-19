#mqpar <- readLines("/Volumes/data-hz/DEMO/mqpar.xml")

xml.mod <- function(template,xml,gsub.string = "</string>"){
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

unlist.fun <- function(x,split="",template = "<"){		
	temp.i <- unlist(strsplit(x,split))
	temp.i <- grep(template,temp.i,fixed = T)[1]
	temp.i <- temp.i -1
	return(temp.i)
	
}


xml.replace <- function(parent,insert,xml,start.string = "<string>",end.string = "</string>" ) {
	
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

path.convert <- function(path){
	if(length(grep("/",path)) > 0){
		path <- gsub("/","\\",path,fixed = T)
		return(path)
	}
	if(length(grep("\\",path,fixed = T)) > 0){
		path <- gsub("\\","/",path,fixed = T)
		return(path)
	}
	
}

search.evidence <- function(path){
		funTemp 	<- list.files(paste(dirname(path),"combined/txt/",sep = "/"),pattern = "evidence.txt",full.names = T)
	if(length(funTemp) > 0){
		print("found evidence.txt")
		return(funTemp)
	}else{
		funTemp 	<- list.files(dirname(path),pattern = "evidence.txt",full.names = T)
			if(length(funTemp) > 0){
				return(funTemp)
			}else{
				
		print("no evidence.txt found")
		return(NULL)	
			}

	}
}
stop()

search.evidence(mq.path.extract("filePaths",mqpar))


temp 	<- xml.replace(c("filePaths","fileNames"),c("one.raw","two.raw"),mqpar)
temp2 	<- xml.replace(c("paramGroups"),c(1,1),temp,start.string = "<int>",end.string = "</int>")
mq.path.extract <- function(parent,xml){path.convert(xml.mod(parent,xml)$insert.clean)}
#xml.replace("filePaths",c("one.raw","two.raw"),mqpar)