htmlMod <- 
function(pathHtml,Machines = c("Bibo","Kermit","Grobi","Bert","Tiffy")
,Counts = rep(NA,length(Machines)),BGcolor  = rep("#000000",length(Machines))
){
 collapse <- function(..., sep = "\n") {
        x <- paste(unlist(list(...)), collapse = sep)
        return(x)
    }

tabTop <- "<TR>
"
tabBot <- "</TR>"
Row <- c()
RowHead <- c()
for(i in 1:length(Machines)){

RowHead <- c(RowHead ,paste("<TD ALIGN=center VALIGN=bottom >", Machines[i],"</TD>",collapse = " "))
Row 		<- 	c(Row,paste("<TD ALIGN=center VALIGN=CENTER bgcolor=	",BGcolor[i],">
  <font color=#fffffff>",Counts[i],"</font><br> </TD>"))
	
}
temp <- c(tabTop, RowHead,"</TR>\n
<TR>",Row
,tabBot)
temp <- unlist(strsplit(temp,"\n"))
temp <<- temp
html <- readLines(pathHtml)
title <- grep("<!-- dropzone -->",html,fixed = T)


html <- c(html[1:(title[1])],temp,html[(title[2]):length(html)])
write(html, pathHtml)

}
#try(htmlMod("/Users/html/index.html",Counts = c(1,2,3,4,5),BGcolor = rep("#123456",5)))

#writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[!ECstd]),path = #paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml)

#try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = Counts,BGcolor = rep("#123456",length(Counts))))
#system("open /Users/html/index.html")