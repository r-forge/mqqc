start.qc <-
function(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F,pdfOut = T, SpeciesTable = T,placeholder = "PLACEHOLDER",templateFasta="PLACEHOLDER",SendMail = T, exitPath = NULL)
{
#DataEvidence <- NULL
require(tcltk)	
#tk_choose.files(multi = F,caption = "select your evidence.txt",filters = matrix(c("Text",".txt","All files","*"),2,2,byrow = T))50  
 
 
cat("\rLoading data",rep(" ",100))

if(!is.null(DataEvidence)){
	if(is.vector(DataEvidence)){
		SourceTime <- file.info(DataEvidence)$ctime
		.path <- dirname(DataEvidence)
		.name <- basename(DataEvidence)
		tryError <- class(try(DataEvidence <- read.csv(DataEvidence,sep = "\t",stringsAsFactors = F)))
	}else{
		.path <- getwd()
		.name <- "unknown"
	}	
	}else{

library(tcltk)
	evidence.path <- file.choose()
	SourceTime <- file.info(evidence.path)$ctime

	tryError <- class(try(DataEvidence <- read.csv(evidence.path,sep = "\t",stringsAsFactors = F)))
	.path <- dirname(evidence.path)
	.name <- basename(evidence.path)
}
cat("\rData loaded",rep(" ",100))

if(tryError == "try-error"){
	  	          write("",paste(.path,"mqqcProcessed",sep = "/"))
}

	
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
####
# Subset evidence
####
temp.DataEvidence <- DataEvidence[as.character(DataEvidence[,raw.files]) ==as.character(i),]	
cat("\rstarting qc.prepare",rep(" ",100))

####
# Calculation of Scores
####

tryError1 <- class(try(qc.prepare.data <- qc.prepare(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta =templateFasta,path = .path,filename = i)))
export 	<- unlist(qc.prepare.data$sd)

add.vec <- c(rep.v[a],as.numeric(Sys.time()),make.names(Sys.time()))
names(add.vec) <- c("Name","System.Time.s","System.Time")
export <- t(as.matrix(c(add.vec ,export)))
 
####
# BSACheck
####
BSACheck <- gsub(placeholder,"BSA", templateFasta,fixed = T)
if(length(grep(BSACheck,i)) > 0){
	
	BSACheck <- T
}else{BSACheck <- F}


####
# Plotting
####
tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))
TotalScoreRes <<- TotalScoreRes
ASCIIplot <- NULL
try(ASCIIplot <- readLines(list.files(pattern = "ASCIIplot.txt",full.name = T)))
if(length(ASCIIplot) > 0){
	ASCIIplot <- paste("\n\n#########\n","# Plots #\n","#########\n\nFor a correct view, please set your font to Menlo regular.\n\n", paste(ASCIIplot,collapse = "\n"),"\n",sep = "")
}else{
	ASCIIplot <- ""
}


if(tryError == "try-error"){
	TotalScoreRes <- list(TotalScore = "?",TotalScoreColor = "#666666")
}
tempScoreList <- t(as.matrix(unlist(TotalScoreRes[1:2])))
export <- cbind(export, tempScoreList)

# ScoreAdd <- c(TotalScoreRes$TotalScore,qc.prepare.data$sc[grep("combi",names(qc.prepare.data$sc))])
# names(ScoreAdd)[1] <- "Total"
# names(ScoreAdd) <- paste(names(ScoreAdd),"Score",sep = "_")
# export <- cbind(export,t(as.matrix(ScoreAdd)))

if(length(exitPath) > 0){
	exitPath  <-  paste(exitPath,paste(Sys.Date(),gsub(".raw$","raw",rep.v[a]),"folder",sep = "_"),paste(rep.v[a],".csv",sep = ""),sep = "/")
	names(exitPath) <- "filePath"
	SourceTime <- as.numeric(SourceTime)
	names(SourceTime) <- "SourceFileTime"
	Status <- "fresh"
	names(Status) <- "Status"
	if(length(SourceTime) == 0){SourceTime <- "error"}
	export <- cbind(export,exitPath, SourceTime, Status)
	
	
}

try(write.csv(export,paste(rep.v[a],".csv",sep = ""),quote = F,row.names = F))
try(save(qc.prepare.data, TotalScoreRes ,file = paste(rep.v[a],".Rdata",sep = ""),quote = F,row.names = F))


flatFile <- t(export)
flatFile <- paste(rownames(flatFile),flatFile[,1],sep = "\t\t")
flatFile <- paste(flatFile,collapse = "\n")
flatFile <- paste("################\n# MQQC Message #\n################\nYour MQQC Analysis of ",data.frame(export)$Name," has finished.\nYou can check your file under http://selbachsrv.mdc-berlin.net/mqqc/index.html.\n\n\n\n#####################\n#  Selected Values  #\n#####################\nNumber of identified peptides: "
                  ,data.frame(export)$msms.count,"\n",
                  "Coverage (median): ",data.frame(export)$Coverage,"\n",
                  "Score (median): ",data.frame(export)$score.50.,"\n",
                  "Median Fragments/identified MSMS: ",data.frame(export)$msmsMassCount.50.,"\n",
                  "MSMS log10 Intensity (median): ",log10(as.numeric(as.character(data.frame(export)$msmsQuantile.50.))), ASCIIplot,
                  "\n\n##########\n#  List  #\n##########\n"
                  
                  
                  ,flatFile,sep = "")

if(!exists("export")){
	export <- "No Info"
}
if(any(c(tryError2, tryError1) == "try-error")){
	export <- cbind(export,"An unexpected error occured")
	
}

test 		<- HtmlTable(t(export))
export 	<- data.frame(export)

MailList <- list.files(path.package("mqqc"),pattern = "MailList.txt",recursive=T,full.name = T)
if(length(MailList) > 0&SendMail){
	MailList  <- read.table(MailList,sep = "\t",colClasses = "character",stringsAsFactors = F)
	MailList 	<- apply(MailList,2,as.character)
	
	Mail <- MailList[MailList[,1] == unlist(strsplit(as.character(export$Name),"_"))[3],2]
	Mail <- Mail[1]
	PrepareMail(Title = paste("MQQC",data.frame(export)$Name,data.frame(export)$msms.count,"Peptides"),Message = flatFile,recipient=gsub("@","\\@",as.character(Mail),fixed = T))
}
list.collect[a]     <- qc.prepare.data
a <- a+1
}

names(list.collect) <- rep.v
if(show.path){

	"Close tkmessage to finish process"
	try(tkmessageBox(message = paste("Finished QC-Analysis. Result in path:\n",getwd(),sep = "")))

}
setwd(.path)

	#try(return(list(qc = qc.prepare.data)))
	#try(system("open ."))

}
#start.qc()
#system(paste("open ",list.files(pattern = ".pdf",recursive = T,full.name = T)))