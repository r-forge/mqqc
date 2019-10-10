ProgressTracker <-
function(folders,htmloutPath= "~/"){

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
  x <<- x
  print(x)
  MSFres <- "No Data"
  
	FI <- file.info(list.files(dirname(x),pattern = "MQStartTag",full.names = T))$ctime

	
	MSFraggerLinks <- sapply(x,function(y){
	  namevec <- ""
	  try({namevec <- readLines(list.files(dirname(y),pattern = "RawFileName",full.names = T))},silent = T)
	  
	  MSF <- list.files(dirname(y),pattern = "MSfragger_Result.pdf",full.names = T)
	  
	  if(length(MSF) > 0){
	    MSF <- MSF
	    namevec <- namevec
	    namevec <- gsub(".raw$","_MSF.pdf",namevec)
	    htmloutPath <<- htmloutPath
	    try({MSFres <- read.table(paste(dirname(MSF),"MSF_results.txt",sep = "/"),skip = 1)[1,1]},silent = T)
	    MSFres <- as.character(MSFres)
	    
	    if(all(list.files(dirname(MSF)) != "msfragger_copied") & any(list.files(dirname(MSF)) ==("MSF_Finished"))){
	      MSF <<- MSF
	      file.copy(MSF,paste(htmloutPath,"files",namevec,sep = "/"),overwrite = T)
	      write("",paste(dirname(MSF),"msfragger_copied",sep = "/"))
	    }
	  }else{namevec <- ""}
	    
	  return(c(namevec,MSFres))
	})
	MSFraggerLinks <<- MSFraggerLinks
	if(any(MSFraggerLinks[1,]!="")){
	  MSFraggerLinks[1,MSFraggerLinks[1,]!=""] <- paste("<a href ='./files/",MSFraggerLinks[1,],"' target ='_blank' >",MSFraggerLinks[2,],"</a>",sep = "")[MSFraggerLinks[1,]!=""]
	  
	}
	MSFraggerLinks[1,MSFraggerLinks[1,]==""] <- "No Data"
	MSFraggerLinks <- MSFraggerLinks[1,]

	addInfo <- sapply(dirname(x),function(x){
	  inf <- file.info(list.files(paste(x,"/combined/proc/",sep = ""),full.name = T))
	  inf <- inf[grep("runningTimes.txt",rownames(inf),invert = T) ,]
	  out <- paste(gsub(".txt$","",basename(rownames(inf)[inf$mtime == max(inf$mtime)])),sep = "",collapse = ";")
	  timevec <- inf$mtime[inf$mtime == max(inf$mtime)]
	  return(c(out,as.character(timevec)[1]))
	})
	if(is.matrix(addInfo)){
	  addTime <- (addInfo[2,])
	  addTime[is.na(addTime)] <- ""
	  addInfo <- addInfo[1,]
	}

	addInfo[addInfo == ""] <- "unknown"
	unit <- ""
	
	if(length(addTime) != length(addInfo)){
	  FI <- ""
	  dif <- ""
	}else{
	  dif <- ""
	  addTime <- addTime
	  FI <- FI
	  # HOMPS <<- difftime(as.POSIXct(addTime),FI,units = "mins")
	  try(dif <- (difftime(as.POSIXct(addTime),FI,units = "mins")),silent = T)
	  try(unit <- rep("min",length(dif)),silent = T)
	  try({
	    
	  
	  dif <- abs(as.numeric(dif))
	  if(any(dif> 60)){
	    print(dif)
	    print(unit)
	    sel <- dif> 60
	    dif[sel] <- dif[sel]/60
	    unit[sel] <- "h"
	  }
	  })
	  
	}
	
	
	x <- cbind(gsub("raw_folder$","",basename(dirname(x))),as.character(FI), addTime,paste(round(as.numeric(dif),1),unit),MSFraggerLinks)
	
  try(x <- cbind(x,addInfo))
	try(colnames(x) <- c("File","MQ Start","Step Time","Duration","MSFragger","Status")[1:dim(x)[2]])
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
# 
# # 
# # htmloutPath <- "~/"
# hotFolder <- "/Volumes/mqqc"
# # hotFolder <-"/Users/henno/temp/MQQC"
# folders <- listFolders(hotFolder)
# folders <- folders[grep("^_RmqqcFile",basename(folders),invert = T)] # Exclude _RmqqcFile_ Folders
# test <- ProgressTracker(folders,htmloutPath = "/Users/henno/Documents/vbox/MQQC/_RmqqcFile_html/")
# # # # # # # #ProgressTracker(folders)
# # # # # # # MqqcRunningFileInfo.html
# system("open /Users/henno/Documents/vbox/MQQC/_RmqqcFile_html/MqqcRunningFileInfo.html")
