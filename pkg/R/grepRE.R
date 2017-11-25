grepRE <- 
function(Vec = tempListOne$Name,RE = RESettings$REmac,invert = F){
  # RE <<- RE
  # Vec <<- Vec
	testMatch  <- regexec(as.character(RE), as.character(Vec))
	testMatch <<- testMatch
	testMatchPos <- t(sapply(testMatch,function(x){c(unlist(x[c(1)]),attributes(x)[1])}))
	VecRet <- substr(as.character(Vec), testMatchPos[,1], (as.numeric(testMatchPos[,1])+as.numeric(testMatchPos[,2])-1))
	if(!invert){
	
	VecRet <- gsub("_$","", VecRet)
	}else{
				VecRet <- apply(cbind(VecRet,Vec),1,function(x){gsub(x[1],"",x[2])})
	}
	
	return(VecRet)
}



