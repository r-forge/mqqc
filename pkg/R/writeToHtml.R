writeToHtml <- 
function(inputVec,path,width = 2000,height = 2000){
initHtml <- "<!DOCTYPE html>
<html>
<body>

<h1>MQQC - results</h1>"
endHtml <- "</body>
</html>"

collVec <- c()
for(i in inputVec){
	tempI <- i
	header 		<- paste("<h2>",basename(i),"</h2>")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height , "\">")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVec <- paste(collVec,hui,sep = "\n")
	
}

finalHtml <- paste(initHtml, collVec,endHtml)
write(finalHtml,file = path)
system("open test.html")
}

