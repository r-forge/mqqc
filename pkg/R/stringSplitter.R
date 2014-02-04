stringSplitter <- 
function(x,maxN = 5,splitter = "_"){
	

  sapply(x,function(x, maxN,splitter){

tempI <- unlist(strsplit(as.character(x),splitter))
tempIlength <- maxN- length(tempI) 
if(tempIlength > 0){
tempI <- c(tempI,rep("NA", tempIlength))
}
if(tempIlength < 0){
	tempI <- c(tempI[1:4],paste(tempI[5:length(tempI)],collapse = "_"))
}
return(tempI)
	
},splitter = splitter,maxN=maxN)
}	