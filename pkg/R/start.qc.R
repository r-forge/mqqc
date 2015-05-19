start.qc <-
function(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F,pdfOut = T, SpeciesTable = T,placeholder = "PLACEHOLDER", RESettings =list(REpar = "PLACEHOLDER"),SendMail = T, exitPath = NULL, BSAID = "P02769")
{
#DataEvidence <- NULL
  require(tcltk)	
  #tk_choose.files(multi = F,caption = "select your evidence.txt",filters = matrix(c("Text",".txt","All files","*"),2,2,byrow = T))50  
   
   #reading data 
  # Peptides
  
  
  cat("\rLoading data",rep(" ",100))
  
  if(!is.null(DataEvidence)){
  	if(is.vector(DataEvidence)){
  		SourceTime <- file.info(DataEvidence)$ctime
  		.path <- dirname(DataEvidence)
  		.name <- basename(DataEvidence)
  		evidence.path <- DataEvidence
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
  
  setwd(.path)
  dir.create(.folder <- paste("mqqc",Sys.Date(),sep = "_"))
  setwd(.folder)
  
  if(tryError == "try-error"){
                  Export <- "Name,System.Time.s,System.Time,msms.count,uniPepCount,ret.peak.shape.0%,ret.peak.shape.25%,ret.peak.shape.50%,ret.peak.shape.75%,ret.peak.shape.100%,ret.width.0%,ret.width.25%,ret.width.50%,ret.width.75%,ret.width.100%,ret.max,total.msms.min,quan.msms.min,mass.error.cal.0%,mass.error.cal.25%,mass.error.cal.50%,mass.error.cal.75%,mass.error.cal.100%,mass.error.uncal.0%,mass.error.uncal.25%,mass.error.uncal.50%,mass.error.uncal.75%,mass.error.uncal.100%,quan.duplicates,quan.duplicates.msms,score.0%,score.25%,score.50%,score.75%,score.100%,quanRetRSD,quanRetSlope.x,RatioIQuan.75%,quanRet50ratio.50%,Intensity.0%,Intensity.25%,Intensity.50%,Intensity.75%,Intensity.100%,missed.cleavages.percent,msmsQuantile.0%,msmsQuantile.25%,msmsQuantile.50%,msmsQuantile.75%,msmsQuantile.100%,msmsMassCount.0%,msmsMassCount.25%,msmsMassCount.50%,msmsMassCount.75%,msmsMassCount.100%,Coverage,msmsEff,TotalScore.MS,TotalScore.MSMS,TotalScore.LC,TotalScore.Total,TotalScoreColor.MS,TotalScoreColor.MSMS,TotalScoreColor.LC,TotalScoreColor.Total,exitPath,SourceTime,Status"
                  Export <- unlist(strsplit(Export,","))
                  TempInfo <- t(as.matrix(rep(NA,length(Export))))
                  colnames(TempInfo) <- Export
                  TempInfo[1,grep("exitPath",colnames(TempInfo))] <- paste(unlist(dirname(dirname(.path))),.name,sep = "/",collapse = "/")
                  TempInfo[1,grep("^Name$",colnames(TempInfo))] <- basename(dirname(dirname(.path)))
                  TempInfo[1,grep("^SourceTime$",colnames(TempInfo))] <- as.numeric(Sys.time())
                  TempInfo[1,grep("^System.Time.s$",colnames(TempInfo))] <- as.numeric(Sys.time())
                  TempInfo[1,grep("^System.Time$",colnames(TempInfo))] <- as.numeric(Sys.time())
                  
                  try(write.csv(TempInfo,paste(basename(dirname(dirname(.path))),".csv",sep = ""),quote = F,row.names = F))
                  return(NULL)
                  
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
  
  
  
  list.collect <- list(length=length(rep.v))
  a <- 1
  #### Test Preread Tables, should fasten readout
  print(.path)
  
  check <- file.exists(peppath<- paste(.path,"peptides.txt",sep = "/"))
  if(check){
    cat("\rLoading peptides",rep(" ",100))
    Peptides <- read.csv(peppath,sep = "\t")
  }else{Peptides <- NULL}
  # AllPeptides 
  check <- file.exists(peppath<- paste(.path,"allPeptides.txt",sep = "/"))
  if(check){    
    cat("\rLoading AllPeptides",rep(" ",100))
    AllPeptides <- read.csv(peppath,sep = "\t",stringsAsFactors = F)
  }else{AllPeptides = NULL}
  # MSMS 
  check <- file.exists(peppath<- paste(.path,"msms.txt",sep = "/"))
  if(check){
    
#     ReadBigSubset <- function(x,select = NULL){
#     
#     ##read in data one line at a time
#     
#     source <- file(x, "r")
#       
#     header = readLines(x,n = 1)
#     header = unlist(strsplit(header,"\t"))
#     header = make.names(header)
#     if(length(select > 0)){
#      headerGrep <- sapply(select,function(x){grep(paste("^",x,"$",sep = ""),header)})
#     }else{headerGrep <- 1:length(header)}
#     #when reading from a connection, readLines jumps to a second line/block after each execution
#     readOut <- c()
#     while (length(line <- readLines(source, n=1, warn=FALSE)) > 0){
#       line<- unlist(strsplit(line,"\t"))[headerGrep]
#       readOut <- rbind2(readOut,line)
#     }
#     close(source)
#     return(readOut)
#     }
#     
#     ReadBigSubset <- function(x,select = NULL){
#       temp <- readLines(x)
#       header = unlist(strsplit(temp[1],"\t"))
#       header = make.names(header)
#       if(length(select > 0)){
#         headerGrep <- sapply(select,function(x){grep(paste("^",x,"$",sep = ""),header)})
#       }else{headerGrep <- 1:length(header)}
#       retVal <- lapply(temp,function(x){
#         unlist(strsplit(x,"\t"))[headerGrep]
#       })
#       
#     }
    MSMS <- read.table(peppath,colClasses = "character",sep = "\t",comment.char = "",header = T)
    # subset
    #raw.file intensities
    #MSMS <- subset(MSMS,select = c("Raw.file","Intensities"))
    
  }else{
    MSMS <- NULL
  }

cat("MSMS",dim(MSMS),"ALLP",dim(AllPeptides),"Peptides",dim(Peptides))
  
  
i = rep.v
for(i in rep.v){
####
# Subset evidence
####
  
temp.DataEvidence <- DataEvidence[as.character(DataEvidence[,raw.files]) ==as.character(i),]	

temp.DataEvidence <- temp.DataEvidence
cat("\rstarting qc.prepare",rep(" ",100))

try(tempAllPeptides <- AllPeptides[AllPeptides$Raw.file  == i,])
try(tempMSMS <- MSMS[MSMS$Raw.file  == i,])

####funfin
# Calculation of Scores
####

#LoadSettings(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =tempAllPeptides,MSMS = tempMSMS)
tryError1 <- class(try(qc.prepare.data <- qc.prepare(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =tempAllPeptides,MSMS = tempMSMS)))
export 	<- unlist(qc.prepare.data$sd)

if(length(qc.prepare.data$IdentifiedProteins) > 0& length(AllPeptides) > 0){
  if(nchar(as.character(qc.prepare.data$IdentifiedProteins)) > 0){
    try(ChrPath <- WriteChromatogram(tempAllPeptides,filename = i,BSAID =as.character(qc.prepare.data$IdentifiedProteins) ,jitfac = 0))
  }else{
    try(ChrPath <- WriteChromatogram(tempAllPeptides,filename = i,BSAID =NULL,jitfac = 0))
  }
}

add.vec <- c(rep.v[a],as.numeric(Sys.time()),make.names(Sys.time()))
names(add.vec) <- c("Name","System.Time.s","System.Time")
export <- t(as.matrix(c(add.vec ,export)))
 
####
# BSACheck
####
BSACheck <- gsub(placeholder,"BSA", RESettings$REpar,fixed = T)
if(length(grep(BSACheck,i)) > 0){
	
	BSACheck <- T
}else{BSACheck <- F}


####  AllPepsData
# Plotting
####
tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,data.list = qc.prepare.data,pdf.name = i, open.doc = F,pdfOut = pdfOut, BSACheck = BSACheck)))
if(exists("ChrPath")){
  try(ASCIIprofileplot(TotalScoreRes$plotData,AllPepsData = ChrPath$all))
  
}else{
  try(ASCIIprofileplot(TotalScoreRes$plotData))
  
}

TotalScoreRes <- TotalScoreRes
ASCIIplot <- NULL
try(ASCIIplot <- readLines(list.files(pattern = "ASCIIplot.txt",full.name = T)))
if(length(ASCIIplot) > 0){
	ASCIIplot <- paste("\n\n#########\n","# Plots #\n","#########\n\nFor a correct view, please set your font to Menlo regular.\n\n", paste(ASCIIplot,collapse = "\n"),"\n",sep = "")
}else{
	ASCIIplot <- ""
}


if(tryError == "try-error"){
	TotalScoreRes <- list(TotalScore = rep("?",4),TotalScoreColor = rep("#666666",4))
}
tempScoreList <- t(as.matrix(unlist(TotalScoreRes[1:2])))
export <- cbind(export, tempScoreList)
colnames(export)[colnames(export) == "TotalScoreColor.DS"] <- "TotalScoreColor"
colnames(export)[colnames(export) == "TotalScore.DS"] <- "TotalScore"

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
                  "Dependent Peptides:",data.frame(export)$DependentPeptides,"\n",
                  "MSMS log10 Intensity (median): ",log10(as.numeric(as.character(data.frame(export)$msmsQuantile.50.))), ASCIIplot#,
#                   "\n\n##########\n#  List  #\n##########\n"
#                   
#                   
#                   ,flatFile
                  ,sep = "")

if(!exists("export")){
	export <- "No Info"
}
if(any(c(tryError2, tryError1) == "try-error")){
	export <- cbind(export,"An unexpected error occured")
	
}

#test 		<- HtmlTable(t(export))
export 	<- data.frame(export)

MailList <- list.files(path.package("mqqc"),pattern = "MailList.txt",recursive=T,full.name = T)
if(length(MailList) > 0&SendMail){
	MailList  <- read.table(MailList,sep = "\t",colClasses = "character",stringsAsFactors = F)
	MailList 	<- apply(MailList,2,as.character)
	MailPatterns <- sapply(MailList[,1],function(x){gsub(placeholder,x,RESettings$REmail)})
  if(!all(sapply(MailPatterns,length) == 0)){
  MailID <- sapply(MailPatterns ,function(x){
    x <- x 
    xn <- as.character(export$Name)
		try(x  <- grep(x,as.character(export$Name),value = F))
		return(length(x) > 0)
	})
	MailID <- MailList[MailID,2]
	for(Mail in MailID){
	PrepareMail(Title = paste("MQQC",data.frame(export)$Name,data.frame(export)$msms.count,"Peptides"),Message = flatFile,recipient=gsub("@","\\@",as.character(Mail),fixed = T))
	}
  }
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
#LoadSettings(DataEvidence = NULL,RawBased = T,n=NA, show.path = F,open.doc = F,pdfOut = T, SpeciesTable = T,placeholder = "PLACEHOLDER", RESettings =list(REpar = "PLACEHOLDER"),SendMail = T, exitPath = NULL, BSAID = "P02769")
