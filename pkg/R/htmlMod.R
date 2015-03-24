htmlMod <- 
function(pathHtml,Machines = c("Bibo","Kermit","Grobi","Bert","Tiffy")
,Counts = rep(NA,length(Machines)),BGcolor  = rep("#000000",length(Machines)),pdfPaths = rep("noInfo",length(Machines))
,Type = ""){
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
CountI <- paste("<a href = \"", pdfPaths[i],"\"style=\"text-decoration: none;color:#ffffff\"  target ='blank'  > ",Counts[i]," </a>",sep = "")

RowHead <- c(RowHead ,paste("<TD ALIGN=center VALIGN=bottom >", Machines[i],"</TD>",collapse = " "))
Row 		<- 	c(Row,paste("<TD ALIGN=center VALIGN=CENTER bgcolor=	",BGcolor[i],">
  ",CountI,"</TD>"))
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
 # try(writeToHtml(inputVec = sort(paste(".", StandardIDs[1],paste(Machines,".pdf",sep = ""),sep = "/")),
 # inputVec2 = sort(paste(".","all",paste(Machines,".pdf",sep = ""),sep = "/")),path = paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml,Table2 = tableHtml2 ,Table3 = tableHtml3, insertText = insertText,Machines = Machines, StandardIDs = StandardIDs))

 # try(htmlMod(pathHtml = paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = HotLink,BGcolor =as.character(HotLinkCol),pdfPaths = HotLinkPath,Type = HotLinkType))

#try(htmlMod("/Users/html/index.html",Counts = c(1,2,3,4,5),BGcolor = rep("#123456",5)))

#writeToHtml(sort(pdfFiles[ECstd]),sort(pdfFiles[!ECstd]),path = #paste(finalMQQC,"index.html",sep = "/"),Table = tableHtml)

#try(htmlMod(paste(finalMQQC,"index.html",sep = "/"),Machines = Machines,Counts = Counts,BGcolor = rep("#123456",length(Counts))))
#system("open /Users/html/index.html")