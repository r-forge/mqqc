check.file.changing <-
function(files,mode = "size"){
	temp <- file.info(files)
	
	temp1 <- temp[,names(temp) == mode]
	temp1 <- cbind(rownames(temp),temp1)
	Sys.sleep(2)
	temp2 <- temp[,names(temp) == mode]
	temp2 <- cbind(rownames(temp),temp2)
	
	temp.all	<- merge(temp1,temp2,by = 1)
	
	temp.all.F 	<- apply(temp.all[,2:3],1,function(x){x<- all(x == 0);return(x)})

	if(!all(!temp.all.F)){
		temp.all 	<- temp.all[!temp.all.F,]
	}
	temp.all <- temp.all[temp.all[,2] == temp.all[,3],1]
	#}#else{temp.all <- ""}
	
	return(temp.all)
}
