writeToHtml <- 
function(inputVec="test", inputVec2 = "test",path = "index.html",width = 1080,height = 540,Table = NULL,Table2 = NULL,Table3 = NULL,Machines = c("Bibo","Kermit","Bert","Tiffy","Grobi")){
	if(!exists("mqqcRunningMQ")){mqqcRunningMQ <- " no info available"}
	#PB <- "<img src=\"./green/PB.gif\"  /><br>"
	
	
initHtml <- paste("
<!-- $Id: example.html,v 1.4 2006/03/27 02:44:36 pat Exp $ -->
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

document.write('<style type=\"text/css\">.tabber{display:none;}</style>');
</script>

<script type=\"text/javascript\">
$(function() {		
	$(\"#table-design2\").tablesorter({sortList:[[1,2],[2,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});	
$(function() {		
	$(\"#table-design3\").tablesorter({sortList:[[1,2],[2,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});	
$(function() {		
	$(\"#table-design\").tablesorter({sortList:[[1,2],[2,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});
</script>
</head>

<body link=\"#cccccc\" alink=\"#0fff00\" vlink=\"#df5320\">

<div id=\"container\" style=\"width:100%\">

<div id=\"header\" style=\"background-color:#426787\">
<h1 style=\"margin-bottom:0;color:#ffffff\">MQQC - MaxQuant Quality Control ",packageVersion("mqqc"),"</h1></div>

<div id=\"menu\" style=\"height:100px;width:40%;float:left;\"> Last update: ",Sys.time(), "<br>MQ Threads:",mqqcRunningMQ,"
<form><input type=button value=\"Refresh\" onClick=\"history.go()\"></form>
<p>
<a href = \"./CheckListFormular.docx\">Check List Formular</a><br>
</p>
</div>

<div id=\"menu\" style=\"height:100px;width:40%;float:left;\"> 
<embed src=\"./insertText.txt\" width = \"1080\" height = \"540\">
</div>

<div id=\"content\" style=\"height:100px;width:20%;float:left;\">
	<div class=\"tabber\"> 
	<TABLE WIDTH=100% HEIGHT=100px BORDER=0>
	
	<!-- dropzone -->
	
	<TR>
	<TD ALIGN=center VALIGN=bottom> Bibo </TD>
	<TD ALIGN=center VALIGN= bottom> Kermit </TD>
	<TD ALIGN=center VALIGN= bottom> Grobi </TD>
	<TD ALIGN=center VALIGN= bottom> Bert </TD>
	<TD ALIGN=center VALIGN= bottom> Tiffy </TD>
	</TR>

	<TR>
	<TD ALIGN=center VALIGN=CENTER bgcolor=	 #000000 >
	  <font color=#fffffff> - </font><br>
	<TD ALIGN=center VALIGN=CENTER bgcolor=	 #000000 >
	  <font color=#fffffff> - </font><br>
	<TD ALIGN=center VALIGN=CENTER bgcolor=	 #000000 >
	  <font color=#fffffff> - </font><br>
	<TD ALIGN=center VALIGN=CENTER bgcolor=	 #000000 >
	  <font color=#fffffff> - </font><br>
	<TD ALIGN=center VALIGN=CENTER bgcolor=	 #000000 >
	  <font color=#fffffff> - </font><br>
	</TR>
	<!-- dropzone -->

	<TR>
	</TR>	

	</TABLE>

</div>

<!-- <div id=\"content\" style=\"height:100px;width:10%;float:left;\">

</div> -->


</div>

<div id=\"footer\" style=\"height:5px;clear:both;text-align:center;\">

</div>

<div id=\"container\" style=\"width:100%\">

<div class=\"tabber\">

",sep = "")




endHtml <- "</div>

</body>
</div>

</div>

<div id=\"footer\" style=\"background-color:#426787;height:5px;clear:both;text-align:center;\">
<a href = #> top </a>
</div>
</html>"


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
if(length(Table3)> 0){	
	tabTop5 <- "

     <div class=\"tabbertab\">
	  <h2>Tables BSA standard</h2>
	  
	  
	  "

tab5 <- paste(tabTop5,Table3,"</div>",sep = "\n")

	
}else{tab5 = "

     <div class=\"tabbertab\">
	  <h2>Tables BSA standard</h2>
	  </div>
 	  <a>no table found</a>
 
	  " 
}
collVec <- c()
for(i in inputVec){
	
	tempI <- i
	header 		<- paste("<h1>",basename(i),"</h1>")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height , "\">",sep = "")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVec <- paste(collVec,hui,sep = "\n")
	
}

tabTop1 <- "

     <div class=\"tabbertab\">
	  <h2> LIVE VIEW E.coli standard</h2>"
	  
tab1 <- paste(tabTop1,collVec,"</div>",sep = "\n")

####
# second tab
####
collVec2 <- c()
for(i in inputVec2){
	tempI <- i
	header 		<- paste("<h1>",basename(i),"</h1>")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height , "\">",sep = "")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVec2 <- paste(collVec2,hui,sep = "\n")
	
}

tabTop2 <- "

     <div class=\"tabbertab\">
	  <h2> LIVE VIEW all</h2>"
	  
tab2 <- paste(tabTop2,collVec2,"</div>",sep = "\n")

######
# Timelines
######

collVecTL <- c()
for(i in Machines){
	tempI <- i
	header 		<- paste("<h1>",basename(i),"</h1>")
	i <- paste("./","TimeLines/",i,"-TimeLine.pdf",sep = "")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height*0.55 , "\">",sep = "")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVecTL <- paste(collVecTL,hui,sep = "\n")
	
}

tabTopTL <- "

     <div class=\"tabbertab\">
	  <h2> ECstd Timeline </h2>"
	  
tabTL <- paste(tabTopTL,collVecTL,"</div>",sep = "\n")


finalHtml <- paste(initHtml,paste(tab4,collapse = ""),paste(tab5,collapse = ""),paste(tab3,collapse = ""), tab1,tab2,tabTL,endHtml,collapse ="")
write(finalHtml,file = path)
#system("open test.html")
}
#test <- matrix(1:100,10,10)
#colnames(test) <- 1:10
#writeToHtml(path = "/Users/html/index.html", inputVec = "c(1,2,3)",inputVec2 = c("a"),Table = 1,Table2 = 2)
#writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[!ECstd]),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml)

#try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = Counts,BGcolor = rep("#FFFFFF",length(Counts))))
# writeToHtml(sort(paste(".","ECstd",paste(Machines,".pdf",sep = ""),sep = "/")),sort(paste(".","all",paste(Machines,".pdf",sep = ""),sep = "/")),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 )

# system("open /Users/html/index.html")
# writeToHtml(inputVec = sort(paste(".","ECstd",paste(Machines,".pdf",sep = ""),sep = "/")),
# inputVec2 = sort(paste(".","all",paste(Machines,".pdf",sep = ""),sep = "/")),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 ,Table3 = tableHtml3)
# system(paste("open ",paste(finalMQQC,"index.html",sep = "/")))