HtmlTable <- 
function(x ,tableDesign = "table-design"){
.cols <- colnames(x)
x 		<- apply(x,2,as.character) 
if(is.vector(x)){x <- t(as.matrix(x))}
htmlTableString <-paste( "<table rules = \"cols\"  id=\"",tableDesign,"\" class=\"tablesorter\">",sep = "")

collapse	<- function(...,sep = "\n"){x <- paste(unlist(list(...)),collapse = sep) ;return(x)}
htmlRow <- function(x,start ="<thead>" ,end = "</thead>",stringTag = "th"){
	x <- sapply(x,function(x){
		paste("<", stringTag,">",as.character(x),"</", stringTag,">",sep = "")
	})
	x <- c(start,x,end)
}

header <-htmlRow(.cols)

rows <- apply(x,1,htmlRow,start = "<tr align = \"center\">",end = "</tr>", stringTag = "td")
htmlTable <- collapse(htmlTableString,header,"<tbody>",rows,"</tbody>","</table>")
return(htmlTable)
}
#hui <- HtmlTable(finalDat)
#print(hui)
