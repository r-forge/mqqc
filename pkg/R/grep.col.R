grep.col <-
function(string,data){x <- grep(string,colnames(data),fixed = T)
if(length(x) == 0){
	 x <- 0
}
if(length(x) > 1){
	cat("\rwarning, more than one match found")
}
return(x)	
	}
