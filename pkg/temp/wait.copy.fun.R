wait.copy.fun <- function(x,type = "size"){

if(type == "md5sum"){
require(tools)
loop <- T
while(loop){	
	temp <- md5sum(x)
	Sys.sleep(1)
	temp2 <- md5sum(x)
	if(all(!is.na(c(temp,temp2)))){
	if(temp == temp2){
		
		loop <- F
	}else{}
		
	}else{}
}
}

if(type == "tryRead"){
require(tools)
loop <- T
while(loop){	
	temp <- class(try(readLines(x),n = 1))
	Sys.sleep(1)
		if(temp != "try-error"){
		
		loop <- F
	}else{}
		
}
}


if(type == "size"){

loop.t <- T
while(loop.t){	
	temp 	<- class(try(file.1 <- file.info(x)$size))
	Sys.sleep(1)
	temp2 	<- class(try(file.2 <- file.info(x)$size))
	if( all(!is.na(c(file.1,file.2)))){
		Sys.sleep(2)
		temp2 	<- class(try(file.2 <- file.info(x)$size))
		if( all(!is.na(c(file.1,file.2)))){
		
		cat("\r",file.1," # ",file.2,"\n")
		if(file.1 == file.2){
			loop.t <- F
		}
		
		
	}else{}
		
	}
}
}
}
