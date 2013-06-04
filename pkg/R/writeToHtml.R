writeToHtml <- 
function(inputVec="test", inputVec2 = "test",path = "index.html",width = 1080,height = 540,Table = NULL,Table2 = NULL){
initHtml <- "<!-- $Id: example.html,v 1.4 2006/03/27 02:44:36 pat Exp $ -->
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<title>MQQC</title>

<script type=\"text/javascript\" src=\"tabber.js\"></script>
<script type=\"text/javascript\" src=\"jquery.js\"></script> 
<script type=\"text/javascript\" src=\"tablesorter/jquery.tablesorter.js\"></script> 


<script type=\"text/javascript\" src=\"tabber.js\"></script>
<link rel=\"stylesheet\" href=\"example.css\" TYPE=\"text/css\" MEDIA=\"screen\">
<link rel=\"stylesheet\" href=\"example-print.css\" TYPE=\"text/css\" MEDIA=\"print\">

<link rel=\"stylesheet\" href=\"example.css\" TYPE=\"text/css\" MEDIA=\"screen\">
<link rel=\"stylesheet\" href=\"example-print.css\" TYPE=\"text/css\" MEDIA=\"print\">
<link rel=\"stylesheet\" href=\"green/style.css\" TYPE=\"text/css\" MEDIA=\"screen\">

<script type=\"text/javascript\">


/* Optional: Temporarily hide the \"tabber\" class so it does not \"flash\"
   on the page as plain HTML. After tabber runs, the class is changed
   to \"tabberlive\" and it will appear. */

document.write('<style type=\"text/css\">.tabber{display:none;}<\\/style>');
</script>

<script type=\"text/javascript\">
$(function() {		
	$(\"#table-design\").tablesorter({sortList:[[0,0],[2,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});	
$(function() {		
	$(\"#table-design2\").tablesorter({sortList:[[0,0],[2,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});
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
	header 		<- paste("<h1>",basename(i),"</h1>")
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
	header 		<- paste("<h1>",basename(i),"</h1>")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height , "\">")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVec2 <- paste(collVec2,hui,sep = "\n")
	
}

tabTop2 <- "

     <div class=\"tabbertab\">
	  <h2>all</h2>"
	  
tab2 <- paste(tabTop2,collVec2,"</div>",sep = "\n")

####
# table
####





if(length(Table)> 0){	
	tabTop3 <- "

     <div class=\"tabbertab\">
	  <h2>Tables</h2>
	  
	  
	  "

tab3 <- paste(tabTop3,Table,"</div>",sep = "\n")

	
}else{tab3 = "

     <div class=\"tabbertab\">
	  <h2>Tables</h2>
	  </div>
 	  <a>no table found</a>
 
	  " 
	  
	  }
	  
####
# table
####





if(length(Table2)> 0){	
	tabTop4 <- "

     <div class=\"tabbertab\">
	  <h2>Tables EC standard</h2>
	  
	  
	  "

tab4 <- paste(tabTop4,Table2,"</div>",sep = "\n")

	
}else{tab4 = "

     <div class=\"tabbertab\">
	  <h2>Tables EC standard</h2>
	  </div>
 	  <a>no table found</a>
 
	  " 
}


finalHtml <- paste(initHtml, tab1,tab2,tab3,tab4,endHtml)
print(finalHtml)
write(finalHtml,file = path)
#system("open test.html")
}
#test <- matrix(1:100,10,10)
#colnames(test) <- 1:10
#writeToHtml(path = "/Users/html/index.html", inputVec = "c(1,2,3)",inputVec2 = c("a"),table = matrix(1:10,2,2))
#writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[!ECstd]),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml)

#try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = Counts,BGcolor = rep("#FFFFFF",length(Counts))))
#system("open /Users/html/index.html")
