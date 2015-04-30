ProgressTracker <-
function(folders){

Delete <-list.files(folders,pattern = "DeleteTag",full.name = T)
Enter <-list.files(folders,pattern = "MQEnterTag",full.name = T)
Start <-list.files(folders,pattern = "MQStartTag",full.name = T)


Pending <- setdiff(dirname(Enter),c(dirname(Delete),dirname(Start))) # Pending FIles
Running <- setdiff(dirname(Start),c(dirname(Delete)))	# Running Files
Finished <- dirname(Delete)# Finished Files

Pending <- paste(Pending, "MQEnterTag" ,sep = "/")
Running <- paste(Running,"MQStartTag" ,sep = "/")
Finished <- paste(Finished,"DeleteTag" ,sep = "/")


All <- list(Pending,Running,Finished)

InfoFile <- lapply(All,function(x){
	xi <- file.info(x)
	addInfo <- sapply(dirname(x),function(x){
	  inf <- file.info(list.files(paste(x,"/combined/proc/",sep = ""),full.name = T))
	  inf <- inf[grep("runningTimes.txt",rownames(inf),invert = T) ,]
	  out <- paste(gsub(".txt$","",basename(rownames(inf)[inf$mtime == max(inf$mtime)])),sep = "",collapse = ";")
	})
	addInfo[addInfo == ""] <- "unknown"
	x <- cbind(gsub("raw_folder$","",basename(dirname(x))), as.character(xi$mtime))
  try(x <- cbind(x,addInfo))
	try(colnames(x) <- c("File","Time","Status")[1:dim(x)[2]])
  if(!all(is.na(x[,2] ))){
  if(is.vector(x)){ x <- t(as.matrix(t))}
  x <- x[order(x[,2],decreasing = T),]
  if(is.vector(x)){x <- t(as.matrix(x))}
  }
	 try(x <-HtmlTable(x, tableDesign = "table-design4",rules = "blank",cellpadding = 3))

	
})



HtmlHeader <- paste("
<!-- $Id: example.html,v 1.4 2006/03/27 02:44:36 pat Exp $ -->
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<title>MQQC</title>

<body link=\"#cccccc\" alink=\"#0fff00\" vlink=\"#df5320\">

<div id=\"container\" style=\"width:100%\">

<div id=\"header\" style=\"background-color:#426787\">
<h1 style=\"margin-bottom:0;color:#ffffff\">MQQC - MaxQuant Quality Control ",packageVersion("mqqc"),"</h1></div>")

Tables <-  c(	"<div id=\"header\" style=\"background-color: #009ec2\"> <h3 style=\"margin-bottom:0;color:#ffffff\">  MQ Queue </h3> </div>", InfoFile[[1]],
						"<div id=\"header\" style=\"background-color: #009ec2\"> <h3 style=\"margin-bottom:0;color:#ffffff\">  MQ analysis started  </h3> </div>", InfoFile[[2]],
						"<div id=\"header\" style=\"background-color: #009ec2\"> <h3 style=\"margin-bottom:0;color:#ffffff\">  MQQC analysis finished </h3></div>", InfoFile[[3]])

endHtml <- "</div>

</body>
</div>

</div>

<div id=\"footer\" style=\"background-color:#426787;height:5px;clear:both;text-align:center;\">
<a href = #> top </a>
</div>
</html>"

write(c(HtmlHeader,Tables, endHtml),paste(htmloutPath,"MqqcRunningFileInfo.html",sep = "/"))
#system(paste("open ",paste(htmloutPath,"MqqcRunningFileInfo.html",sep = "/")))
}
#ProgressTracker(folders)

