writeToHtml <- 
function(inputVec="test", inputVec2 = "test",path = "index.html",width = 1080,height = 540,Table = NULL,Table2 = NULL,Table3 = NULL,Machines = c("Bibo","Kermit","Bert","Tiffy","Grobi"), insertText = "",StandardIDs = c("ECstd","BSA")){
if(!exists("mqqcRunningMQ")){mqqcRunningMQ <- "no info available"}
	#PB <- "<img src=\"./green/PB.gif\"  /><br>"
	
but1 <- "  <form method=\"get\" action=\"./CheckListFormular.docx\" target=\"_blank\" NAME=\"spaceform\" STYLE=\"margin: 0px; padding: 0px;\">
  <button type=\"submit\">Check List Formular</button>
  </form>"
but2 <- "<form method=\"get\" target=\"_blank\" action=\"./html/MQQC-Documentation.pdf\">
  <button type=\"submit\">MQQC Documentation</button>
  </form>"
but1 <- "
<a href = \"./CheckListFormular.docx\" style=\"color:grey;text-decoration: none;\" target=\"_blank\" >Check List Formular</a>
"
but2 <- "
    <a href = \"./html/MQQC-Documentation.pdf\" style=\"color:grey;text-decoration: none;\" target=\"_blank\">MQQC Documentation; </a>
"
but3 <- "
    <a href = \"./MQQC_PID.html\" style=\"color:grey;text-decoration: none;\" target=\"_blank\">ParameterIDs</a>
"
if(!file.exists(paste(dirname(path),"insertText.txt",sep = "/"))){
	write("",paste(dirname(path),"insertText.txt",sep = "/"))
}	

if(!file.exists(paste(dirname(path),"CheckListFormular.docx",sep = "/"))){but1 <- ""}
if(!file.exists(paste(dirname(path),"html/MQQC-Documentation.pdf",sep = "/"))){but2 <- ""}

if(but1 !="" & but2!= ""){
  but1 <- paste(but1,"<br>",sep = "",collapse = "")
}


initHtml <- paste("
<html lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<title>MQQC</title>

<script type=\"text/javascript\" src=\"html/tabber.js\"></script>
<script type=\"text/javascript\" src=\"html/jquery.js\"></script> 
<script type=\"text/javascript\" src=\"html/tablesorter/jquery.tablesorter.js\"></script> 


<script type=\"text/javascript\" src=\"html/tabber.js\"></script>
<link rel=\"stylesheet\" href=\"html/example.css\" TYPE=\"text/css\" MEDIA=\"screen\">
<link rel=\"stylesheet\" href=\"html/example-print.css\" TYPE=\"text/css\" MEDIA=\"print\">

<link rel=\"stylesheet\" href=\"html/example.css\" TYPE=\"text/css\" MEDIA=\"screen\">
<link rel=\"stylesheet\" href=\"html/example-print.css\" TYPE=\"text/css\" MEDIA=\"print\">
<link rel=\"stylesheet\" href=\"html/green/style.css\" TYPE=\"text/css\" MEDIA=\"screen\">

<script type=\"text/javascript\">


/* Optional: Temporarily hide the \"tabber\" class so it does not \"flash\"
   on the page as plain HTML. After tabber runs, the class is changed
   to \"tabberlive\" and it will appear. */

document.write('<style type=\"text/css\">.tabber{display:none;}</style>');
</script>

<script type=\"text/javascript\">
$(function() {		
	$(\"#table-design2\").tablesorter({sortList:[[6,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});	
$(function() {		
	$(\"#table-design3\").tablesorter({sortList:[[6,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});	
$(function() {		
	$(\"#table-design\").tablesorter({sortList:[[6,1]], widgets: ['zebra']});
	$(\"#options\").tablesorter({sortList: [[0,0]], headers: { 3:{sorter: false}, 4:{sorter: false}}});
});
</script>
</head>

<body link=\"#cccccc\" alink=\"#0fff00\" vlink=\"#df5320\">

<div id=\"container\" style=\"width:100%\">

<div id=\"header\" style=\"background-color:#426787\">
<h1 style=\"margin-bottom:0;color:#ffffff\">MQQC - MaxQuant Quality Control ",packageVersion("mqqc"),"</h1></div>

<div id=\"menu\" style=\"height:100px;width:40%;float:left;\"> Last update: ",Sys.time(), "<br>MQ Threads: <a href = \"./MqqcRunningFileInfo.html\" target ='_blank'  >",mqqcRunningMQ,"

  <form NAME=\"spaceform\" STYLE=\"margin: 0px; padding: 0px;\"><input type=button value=\"Refresh\" onClick=\"history.go(0)\"></form>

",but1,but2,but3,"


</div>

<div id=\"menu\" style=\"height:100px;width:20%;float:left;\"> 
",insertText,"
</div>

<div id=\"content\" style=\"height:100px;width:40%;float:left;\">
	<TABLE WIDTH=100% HEIGHT=100px BORDER=0>
	
	<!-- dropzone -->
	
	<TR>",
  paste("<TD ALIGN=center VALIGN=bottom>",Machines,"</TD>",collapse = "\n")                

	,"</TR>

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
"


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
	tabTop4 <- paste("

     <div class=\"tabbertab\">
	  <h2>Tables ", StandardIDs[1]," standard</h2>
	  
	  
	  ")

tab4 <- paste(tabTop4,Table2,"</div>",sep = "\n")

	
}else{tab4 = paste("

     <div class=\"tabbertab\">
	  <h2>Tables", StandardIDs[1], "standard</h2>
	  </div>
 	  <a>no table found</a>
 
	  " )
}
if(length(Table3)> 0){	
	tabTop5 <- paste("

     <div class=\"tabbertab\">
	  <h2>Tables ", StandardIDs[2]," standard</h2>
	  
	  
	  ")

tab5 <- paste(tabTop5,Table3,"</div>",sep = "\n")

	
}else{tab5 = paste("

     <div class=\"tabbertab\">
	  <h2>Tables",StandardIDs[2],"standard</h2>
	  </div>
 	  <a>no table found</a>
 
	  " )
}
collVec <- c()
for(i in inputVec){
	
	tempI <- i
	header 		<- paste("<h1>",gsub(".pdf$","",basename(i)),"</h1>")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height , "\">",sep = "")
	hui 			<- paste(header,pdfPath,sep = "\n")
	collVec <- paste(collVec,hui,sep = "\n")
	
}

tempName <- StandardIDs[1]

tabTop1 <- paste("<div class=\"tabbertab\">
	  <h2> LIVE VIEW",tempName,"standard</h2>")
	  
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

if(1){
tabTop2 <- "

     <div class=\"tabbertab\">
	  <h2> LIVE VIEW all</h2>"
	  
tab2 <- paste(tabTop2,collVec2,"</div>",sep = "\n")
}
######
# Timelines
######

collVecTL <- c()
for(i in Machines){
	tempI <- i
	header 		<- paste("<h1>",basename(i),"</h1>")
	
  if(StandardIDs[1] != ""){
	i <- paste("./","TimeLines/",i,"-TimeLine-high.pdf",sep = "")
  }else{
    if(StandardIDs[2] != ""){
      i <- paste("./","TimeLines/",i,"-TimeLine-low.pdf",sep = "")
    }else{
      i <- paste("./","TimeLines/",i,"-TimeLine-All.pdf",sep = "")
      
    }
  }

	#jpgPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height*0.55 , "\">",sep = "")
	
	jpgPath  <- paste(	"<img src=\"",gsub("pdf$","jpg",i),"\" width=\"", width,"\" alt=\"MQQCTimeline\">",sep = "")
	pdfPath 	<- paste("<embed src=\"",i,"\" width = \"",width,"\" height = \"",height*0.55 , "\">",sep = "")
	PdfLink <- paste("<a href=\"",i,"\" target=\"_blank\">",jpgPath,"</a>",sep = "")
	
	hui 			<- paste(header, PdfLink,sep = "\n")
	collVecTL <- paste(collVecTL,hui,sep = "\n")
	
}

tabTopTL <- paste("

     <div class=\"tabbertab\">
	  <h2> ",StandardIDs[1]," Timeline </h2>")
	  
tabTL <- paste(tabTopTL,collVecTL,"</div>",sep = "\n")

#####
# All Comparisons
#####
ComparisonFiles <- list.files(paste(dirname(path),"TimeLines",sep = "/"),pattern = "Comparison.pdf$")
if(length(ComparisonFiles) > 0){
Order <- sapply(c("^Complex","^LowComplex","^Normal"),grep, ComparisonFiles)
ComparisonFiles <- paste(".","TimeLines", ComparisonFiles,sep = "/")
collVecParCo <- c()
for(i in 1:length(Order)){
  if(length(unlist(Order[i])) > 0){
	tempI 			<- ComparisonFiles[unlist(Order[i])]
	nameComp 	<- "Platzhalter"
	if(i == 1){nameComp <- "High complex standard"}
	if(i == 2){nameComp <- "Low complex standard"}
	if(i == 3){nameComp <- "Samples"}
	
	header 			<- paste("<h1>",nameComp,"</h1>")
	pdfPath	<- paste("<embed src=\"", tempI,"\" width = \"",width,"\" height = \"",height , "\">",sep = "")
	jpgPath <- paste(	"<img src=\"",gsub("pdf$","jpg", tempI),"\" width=\"", width,"\" alt=\"MQQCComparison\">",sep = "")
	
	PdfLink <- paste("<a href=\"", tempI,"\" target=\"_blank\">", jpgPath,"</a>",sep = "")
	
	hui 				<- paste(header, PdfLink ,sep = "\n")
	collVecParCo 	<- paste(collVecParCo,hui,sep = "\n")
  }
}
}else{
	collVecParCo <- "NO DATA"
}
tabTopParCo <- paste("

     <div class=\"tabbertab\">
	  <h2> Parameter Comparisons </h2>")
	  
tabParCo <- paste(tabTopParCo, collVecParCo,"</div>",sep = "\n")

#====================================================================================
if(StandardIDs[1] == ""){
	tab4 = ""
	#tabTL = ""
	tab1 = tab2
}
if(StandardIDs[2]== ""){
	tab5 = ""
	tab2 = tab1 # Not Sure if that works
}
tab4 <<- tab4
tab5 <<- tab5
tab3 <<- tab3
finalHtml <- 	paste(initHtml,

				paste(tab4,collapse = ""), # ECstd
				paste(tab5,collapse = ""), # BSA
				paste(tab3,collapse = ""), # All
				tab1,						# LiveViewEcoli
				#tab2,						# 
				tabTL,						# Timeline EC
				tabParCo,					# Parameter Comparison
				endHtml,
				
	"<div style=\"float:right\"><br><a href = \"./disclaimer.html\" target = \"_blank\"> MQQC can be used under the terms of the  GNU General Public License  </a></div> 
	<div style = \"float:left\"><br> &copy; 2014 Henrik Zauber <a href=\"https://www.mdc-berlin.de/40599418/en/globals/impressum/\" target=_blank>MAX-DELBRUECK CENTRUM FOR MOLECULAR MEDICINE</a>; Berlin-Buch; Germany</div>"
	,
	"</html>"
	,
	collapse ="")
write(finalHtml,file = path)
disclaimer <- paste("
<html lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<title>MQQC</title>
<div id=\"header\" style=\"background-color:#426787\">
<h1 style=\"margin-bottom:0;color:#ffffff\">MQQC - MaxQuant Quality Control ",packageVersion("mqqc"),"</h1></div>
<div id=\"header\" style=\"background-color: #009ec2\"> <h3 style=\"margin-bottom:0;color:#ffffff\">  License </h3> </div>

<div id=\"bottom_text\">
	<br>
<a> This program is free software: you can redistribute it and/or modify it under the terms of the <a href = \"http://www.gnu.org/copyleft/gpl.html\" target = \"blank\"> GNU General Public License </a> as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.</a> 
      
					 </div>

<p>
   &copy; 2014 Henrik Zauber <a href=\"https://www.mdc-berlin.de/40599418/en/globals/impressum/\" target=_blank>MAX-DELBRUECK CENTRUM FOR MOLECULAR MEDICINE</a>; Berlin-Buch; Germany </p>					 
					 ", endHtml,"</html>")
write(disclaimer,file =paste(dirname(path),"disclaimer.html",sep = "/"))

#print(path)
#system(paste("open",path))
}
#try(  FUNFINAL(StandardIDs = c("",""),finalMQQC=Param$htmloutPath,folder =Param$folder, RESettings = RESettings,Machines = Param$Machines))
#finalMQQC = htmloutPath
#StandardIDs <- c("","")
#Machines = "hui"
#try(writeToHtml(inputVec = sort(paste(".", StandardIDs[1],paste(Machines,".pdf",sep = ""),sep = "/")),inputVec2 = sort(paste(".","all",paste(Machines,".pdf",sep = ""),sep = "/")),path = paste(finalMQQC,"index.html",sep = "/"),Table = "tableHtml",Table2 = "tableHtml2" ,Table3 = "tableHtml3", insertText = "insertText",Machines = "Machines", StandardIDs=c("","")))
#path = paste(finalMQQC,"index.html",sep = "/")
#system(paste("open",path ))
# try(writeToHtml(inputVec = sort(paste(".", StandardIDs[1],paste(Machines,".pdf",sep = ""),sep = "/")),
# inputVec2 = sort(paste(".","all",paste(Machines,".pdf",sep = ""),sep = "/")),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 ,Table3 = tableHtml3, insertText = insertText,Machines = Machines, StandardIDs = StandardIDs))


# try(htmlMod(pathHtml = paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = HotLink,BGcolor =as.character(HotLinkCol)))
#writeToHtml()
