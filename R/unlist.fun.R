unlist.fun <-
function(x,split="",template = "<"){		
	temp.i <- unlist(strsplit(x,split))
	temp.i <- grep(template,temp.i,fixed = T)[1]
	temp.i <- temp.i -1
	return(temp.i)
	
}
