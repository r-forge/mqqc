merge.control <-
function(input,ref){
#input 	<- sub.info[,2]
#ref 	<- rownames(row.plot.data)

	input <- as.character(input)
	input <- cbind(input,c(1:length(input)))
	
	ref <- as.character(ref)
	ref <- cbind(ref,c(1:length(ref)))

	.merge	<- merge(as.matrix(ref),as.matrix(input),by = 1,all.x = T)
	.merge 	<- .merge[order(as.numeric(as.character(.merge[,2]))),]
	return(as.numeric(as.character(.merge[,3])))
	
	
}
