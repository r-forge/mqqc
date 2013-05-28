start.qc <-
function(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F,pdfOut = T, SpeciesTable = T,placeholder = "PLACEHOLDER",templateFasta="PLACEHOLDER")
{
require(tcltk)	
#tk_choose.files(multi = F,caption = "select your evidence.txt",filters = matrix(c("Text",".txt","All files","*"),2,2,byrow = T))
cat("\rLoading data",rep(" ",100))
if(!is.null(DataEvidence)){
	if(is.vector(DataEvidence)){
		.path <- dirname(DataEvidence)
		.name <- basename(DataEvidence)
		class(try(DataEvidence <- read.csv(DataEvidence,sep = "\t",stringsAsFactors = F)))
	}else{
		.path <- getwd()
		.name <- "unknown"
	}	
	}else{

library(tcltk)
	evidence.path <- file.choose()
	class(try(DataEvidence <- read.csv(evidence.path,sep = "\t",stringsAsFactors = F)))
	.path <- dirname(evidence.path)
	.name <- basename(evidence.path)
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
qc.prepare.data <- qc.prepare(temp.DataEvidence, SpeciesTable,placeholder = placeholder,templateFasta =templateFasta)

export 	<- unlist(qc.prepare.data$sd)

add.vec <- c(rep.v[a],as.numeric(Sys.time()),make.names(Sys.time()))
names(add.vec) <- c("Name","System.Time.s","System.Time")
export <- t(as.matrix(c(add.vec ,export)))
try(write.csv(export,paste(rep.v[a],".csv",sep = ""),quote = F,row.names = F))
plot.scores(temp.DataEvidence,qc.prepare.data,i, open.doc = F,pdfOut = pdfOut)

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
#start.qc()
#system(paste("open ",list.files(pattern = ".pdf",recursive = T,full.name = T)))