htmlMod <- 
function(pathHtml,Machines = c("Bibo","Kermit","Grobi","Bert","Tiffy")
,Counts = rep(NA,length(Machines)),BGcolor  = rep("#000000",length(Machines))
){
    collapse <- function(..., sep = "\n") {
        x <- paste(unlist(list(...)), collapse = sep)
        return(x)
    }

tabTop <- "<BODY LEFTMARGIN=0 TOPMARGIN=0 MARGINWIDTH=4 MARGINHEIGHT=2>
<TABLE WIDTH=100% HEIGHT=1000% CELLSPACING=0 CELLPADDING=2 BORDER=0>
<TR>
<TD VALIGN=TOP ALIGN=LEFT rowspan = \"2\">
  <h1>MQQC</h1>
</TD><TD WIDTH=100%></TD>
"
tabBot <- "</TR>

<TR>
<TD WIDTH=100%></TD>

</TR>
</TABLE>
</BODY>"
Row <- c()
RowHead <- c()
for(i in 1:length(Machines)){

RowHead <- c(RowHead ,paste("<TD ALIGN=center VALIGN=TOP>", Machines[i],"</TD>",collapse = " "))
Row 		<- 	c(Row,paste("<TD ALIGN=center VALIGN=CENTER bgcolor=	",BGcolor[i],">
  <font color=#fffffff>",Counts[i],"</font><br>"))
	
}
temp <- c(tabTop, RowHead,"</TR>\n
<TR>","	<TD VALIGN=CENTER ALIGN=LEFT rowspan = \"2\">
	  <h1></h1>
	</TD>",Row
,tabBot)
temp <- unlist(strsplit(temp,"\n"))
html <- readLines(pathHtml)
title <- grep("<h1>MQQC</h1>", html,fixed = T)

print(html)
print(title)
print(length(grep("VALIGN=TOP",html[(title-1)],fixed = T)) > 0)
if(length(grep("VALIGN=TOP",html[(title-1)],fixed = T)) == 0){
html <- html[ - title]
html <- c(html[1:(title)],temp,html[(title):length(html)])
}else{
Start <- grep("<BODY LEFTMARGIN=0 TOPMARGIN=0 MARGINWIDTH=4 MARGINHEIGHT=2>",html)
End <- grep("<TD WIDTH=100%></TD>",html)
FEnd <- grep("</BODY>",html)
FEnd <- FEnd[FEnd >max(End)]
html <- html[-c(Start:FEnd)]
html <- c(html[1:Start],temp,html[Start:length(html)])
	
}
write(html, pathHtml)
}
#writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[!ECstd]),path = #paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml)

#try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = Counts,BGcolor = rep("#123456",length(Counts))))
#system("open /Users/html/index.html")