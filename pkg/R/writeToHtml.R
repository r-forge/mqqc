writeToHtml <- 
function(inputVec="test", inputVec2 = "test",path = "file.html",width = 1000,height = 750){
initHtml <- "<!-- $Id: example.html,v 1.4 2006/03/27 02:44:36 pat Exp $ -->
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<title>Simple Tabber Example</title>

<script type=\"text/javascript\" src=\"tabber.js\"></script>
<link rel=\"stylesheet\" href=\"example.css\" TYPE=\"text/css\" MEDIA=\"screen\">
<link rel=\"stylesheet\" href=\"example-print.css\" TYPE=\"text/css\" MEDIA=\"print\">

<script type=\"text/javascript\">

/* Optional: Temporarily hide the \"tabber\" class so it does not \"flash\"
   on the page as plain HTML. After tabber runs, the class is changed
   to \"tabberlive\" and it will appear. */

document.write('<style type=\"text/css\">.tabber{display:none;}<\\/style>');
</script>

</head>
<body>

<h1>MQQC</h1>
<div class=\"tabber\">"

endHtml <- "</div>

</body>
</html>"

collVec <- c()
for(i in inputVec){
	tempI <- i
	header 		<- paste("<h2>",basename(i),"</h2>")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height , "\">")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVec <- paste(collVec,hui,sep = "\n")
	
}

tabTop1 <- "

     <div class=\"tabbertab\">
	  <h2>E.coli standard</h2>"
	  
tab1 <- paste(tabTop1,collVec,"</div>",sep = "\n")

####
# second tab
####
collVec2 <- c()
for(i in inputVec2){
	tempI <- i
	header 		<- paste("<h2>",basename(i),"</h2>")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height , "\">")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVec2 <- paste(collVec2,hui,sep = "\n")
	
}

tabTop2 <- "

     <div class=\"tabbertab\">
	  <h2>all</h2>"
	  
tab2 <- paste(tabTop2,collVec2,"</div>",sep = "\n")



finalHtml <- paste(initHtml, tab1,tab2,endHtml)
write(finalHtml,file = path)
#system("open test.html")
}
#writeToHtml(path = "/Users/henno/Downloads/tabber/test/test.html", inputVec = "contaminants.test.pdf",inputVec2 = "contaminants.test.pdf")
