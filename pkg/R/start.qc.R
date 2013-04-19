start.qc <-
function(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F)
{
require(tcltk)	
#tk_choose.files(multi = F,caption = "select your evidence.txt",filters = matrix(c("Text",".txt","All files","*"),2,2,byrow = T))
cat("\rLoading data",rep(" ",100))
if(!is.null(DataEvidence)){
	if(is.vector(DataEvidence)){
		.path <- dirname(DataEvidence)
		class(try(DataEvidence <- read.csv(DataEvidence,sep = "\t",stringsAsFactors = F)))
	}else{
		.path <- getwd()
	}	
	}else{

library(tcltk)
	evidence.path <- file.choose()
	class(try(DataEvidence <- read.csv(evidence.path,sep = "\t",stringsAsFactors = F)))
	.path <- dirname(evidence.path)
}
cat("\rData loaded",rep(" ",100))


	
raw.files 		<- grep("raw.file",tolower(colnames(DataEvidence)),)
raw.files.str 	<- unique(DataEvidence[,raw.files])
rep.v <- raw.files.str


if(!is.na(n)){
	if(is.numeric(n)){
		rep.v <- raw.files.str[n]
	}
	if(is.character(n)){
		rep.v <- n
	}
}

setwd(.path)
dir.create(.folder <- paste("mqqc",Sys.Date(),sep = "_"))
setwd(.folder)

list.collect <- list(length=length(rep.v))
a <- 1
for(i in rep.v){

temp.DataEvidence <- DataEvidence[as.character(DataEvidence[,raw.files]) ==as.character(i),]	
cat("\rstarting qc.prepare",rep(" ",100))
qc.prepare.data <- qc.prepare(temp.DataEvidence)

plot.scores(temp.DataEvidence,qc.prepare.data,i, open.doc = F)

list.collect[a] <- qc.prepare.data
a <- a+1
}

names(list.collect) <- rep.v
if(show.path){

	"Close tkmessage to finish process"
	try(tkmessageBox(message = paste("Finished QC-Analysis. Result in path:\n",getwd(),sep = "")))

}
setwd(.path)

	try(return(list(qc = qc.prepare.data)))
	try(system("open ."))

}
