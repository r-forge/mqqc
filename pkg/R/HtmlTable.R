HtmlTable <- 
function(x ,tableDesign = "table-design",align = NULL){
if(length(align) == 0|length(align)!= dim(x)[2]){
	#print("uhiwfuweihfiuwehf")
	align <- rep("center",dim(x)[2])
}		
	#print(align)

.cols <- colnames(x)
x 		<- apply(x,2,as.character) 
if(is.vector(x)){x <- t(as.matrix(x))}
htmlTableString <-paste( "<table rules = \"cols\"  id=\"",tableDesign,"\" class=\"tablesorter\">",sep = "")

collapse	<- function(...,sep = "\n"){x <- paste(unlist(list(...)),collapse = sep) ;return(x)}
htmlRow <- function(x,start ="<thead>" ,end = "</thead>",stringTag = "th",align ){
	alignImqc <<- 1
	x <- sapply(x,function(x,align){
		ret <- paste("<", stringTag," align = '",align[alignImqc],'',"'>",as.character(x),"</", stringTag,">",sep = "")
		alignImqc <<- alignImqc+1
		return(ret)
	},align = align)
	x <- c(start,x,end)
}

header <-htmlRow(.cols,align = rep("center",dim(x)[2]))
		#print("f")

rows <- apply(x,1,htmlRow,start = "<tr align = \"center\">",end = "</tr>", stringTag = "td", align = align)
htmlTable <- collapse(htmlTableString,header,"<tbody>",rows,"</tbody>","</table>")
return(htmlTable)
}
#hui <- HtmlTable(tr)
#FUNFINAL(htmloutPath,folder)
