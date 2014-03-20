qc.prepare <- 
function(Data, SpeciesTable,placeholder,templateFasta,path = "./"){
score <- list()

sumDat <- function(){
	temp <- list.files(pattern = "summary")
	if(length(temp) > 0){
	temp <- read.csv(temp[1],sep = "\t")
	colnames(temp) <- tolower(colnames(temp))
	return(cbind(as.character(temp$raw.file),temp$ms.ms.identified....))
	}else{return(NA)}
}



get.env2 <- environment() 
ls.null <- function(get.env = get.env2){
	ls.temp <- ls(envir = get.env)
	for(i in 1:length(ls.temp)){
	temp.i <- get(ls.temp[i])
	if(length(temp.i) == 0){
	assign(ls.temp[i],0,envir = get.env)
		
	}
	}
}

#### Define thresholds
# needs to be modular
####
thresholds <- list()
mass.error 		<- c(0.5,5)
thresholds$msms.count 		<- 4000
thresholds$ret.peak.shape <- c(0.5,3)
thresholds$ret.width 		  <- c(0.3,1)
thresholds$total.msms.min	<- 150
thresholds$quan.msms.min	<- 200
thresholds$mass.error.cal <- c(0.5,5)
thresholds$quan.duplicates.msms <- 0.05
thresholds$score 			    <- 100
thresholds$msmsEff 			  <- 60
thresholds$quanRetRSD 		<- 0.05
thresholds$quanRetSlope 	<- 0.02
thresholds$quanRet50ratio	<- 1.2
thresholds$msmsQuantile	 <-  c(4.5,5) # log10 Int
thresholds$msmsCounts 	<- c(30,40)
thresholds$Intensities 			<- c(50000000,1000000000)


if(SpeciesTable){
		colnames(Data) <- tolower(colnames(Data))

	      species 	<- read.csv(paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"))
	      RawFile <- unique(Data$raw.file)[1]
	      regEx 	<- sapply(species$Abbreviation,function(x){gsub(placeholder,x, 	templateFasta,fixed = T)})	
	      temp   	<-as.logical(sapply(regEx,grep, x = RawFile))
		temp[is.na(temp)] <- FALSE

		if(any(temp)){
		speciesUsed <- species[temp,]
		thresholds	<- as.list(speciesUsed[1,])
		thresholds <- lapply(thresholds,function(x){
			x <- as.character(x)
			x <- unlist(strsplit(x," "))
			x <- as.numeric(x)
			
		})
		
		}
}
#.cols <- colnames(Data)
#Data <- apply(Data,2,function(x){as.numeric(as.character(x))})
#colnames(Data) <- .cols
Data <- as.data.frame(Data)
grep.col <- function(string,Data){x <- grep(string,colnames(Data),fixed = T)
if(length(x) == 0){
	 x <- 0
}
if(length(x) > 1){
	 cat("\rwarning, more than one match found",rep(" ",100))
}
return(x)	
	}

summary.Data <- list()
# read.Data
#Data <- temp.i
colnames(Data) <- tolower(colnames(Data))


raw.files <- grep.col("raw.file",Data)

Data.i <- Data[unique(Data[,raw.files])[1] == Data[,raw.files],]

Data.i <<- Data.i
RawFilesUsed <- unique(Data.i$raw.file)

# MSMS

type.ident 	<- Data.i[,grep.col("type", Data.i)]
msms.count <- length(grep("MSMS", type.ident))
summary.Data$msms.count <- msms.count


seq 					<- Data.i$sequence[grep("MSMS",type.ident)]
uniPepCount 	<- length(unique(seq))
summary.Data$uniPepCount <- uniPepCount


# Ret.time
reten.ident 	<- Data.i[,grep.col("retention.time", Data.i)]
reten.start 		<- reten.ident[,grep.col("start",reten.ident)]
reten.stop 		<- reten.ident[,grep.col("finish", reten.ident)]
reten.mid 		<- reten.ident[,colnames(reten.ident) == "calibrated.retention.time"]


ls.null()


ret.peak.ratio 	<- (reten.stop-reten.mid)/(reten.mid-reten.start)
ret.total		<- reten.stop - reten.start

summary.Data$ret.peak.shape <- quantile(ret.peak.ratio,na.rm = T)
summary.Data$ret.width 		<- quantile(ret.total,na.rm = T)
summary.Data$ret.max		<- max(reten.ident,na.rm = T)
quant.range <- quantile(reten.mid,na.rm = T)[2:4]

ident.peps 	<- reten.mid > min(quant.range,na.rm = T) & reten.mid < max(quant.range,na.rm = T)
Data.i.quant <- Data.i[ident.peps,] # subset with innerquantile range

# MSMS
summary.Data$total.msms.min <-  summary.Data$msms.count/summary.Data$ret.max
summary.Data$quan.msms.min 	<-  length(grep("MSMS",Data.i.quant$type))/(max(quant.range,na.rm = T) - min( quant.range,na.rm = T))

# mass error 

mass.error 		<- Data.i[,grep.col("mass.error",Data.i)]
mass.error.uncal 	<- mass.error[,grep.col("uncalibrated",mass.error)]
mass.error 		<- mass.error[,-grep.col("uncalibrated",mass.error)]
summary.Data$mass.error.cal 	<- quantile(as.numeric(mass.error),na.rm = T)
summary.Data$mass.error.uncal 	<- quantile(as.numeric(mass.error.uncal),na.rm = T)
# duplicates
pep.identifier 			<- paste(round(Data.i.quant$m.z),round(Data.i.quant$charge), Data.i.quant$modified.sequence)
double 					<- length(pep.identifier)-length(unique(pep.identifier))
summary.Data$quan.duplicates 		<- double
summary.Data$quan.duplicates.msms 	<- double/length(grep("MSMS",Data.i.quant$type))
summary.Data$score<- quantile(Data.i$score,na.rm = T)
# sd interquantile

temp 		<- density(Data$calibrated.retention.time)
tempQuan 	<- quantile(Data$calibrated.retention.time) 


x <- temp$x
y <- temp$y

ySel <- y[x > tempQuan[2]&x< tempQuan[4]]
xSel <- x[x > tempQuan[2]&x< tempQuan[4]]
slope <- NA
y<<- y
x<<- x
try(slope <- coefficients(lm(scale(y)~x))[2])
rSDquanRet				<- sd(ySel)/median(ySel)
summary.Data$quanRetRSD <- rSDquanRet
summary.Data$quanRetSlope <- slope
summary.Data$quanRet50ratio <- diff(tempQuan[c(1,3)])/diff(tempQuan[c(3,5)])
score$quanRetRSD 		<- 	thresholds$quanRetRSD/summary.Data$quanRetRSD
score$quanRetSlope 		<-	thresholds$quanRetSlope /abs(summary.Data$quanRetSlope)
if(abs(summary.Data$quanRet50ratio) > 1){
score$quanRet50ratio 	<- 	thresholds$quanRet50ratio/abs(summary.Data$quanRet50ratio)}else{
score$quanRet50ratio 	<- 	abs(summary.Data$quanRet50ratio)	/thresholds$quanRet50ratio
	
}
# Intensity Value
vals <- grep("^intensity",colnames(Data),ignore.case = T)
if(length(vals) > 1){
ValExclude <- grep("^intensity$",colnames(Data),ignore.case = T)
vals <- setdiff(vals,ValExclude)
}	
Int <- Data[, vals]
Int <- as.vector(Int)
Int <- Int[Int != 0]
IntQuan <- quantile(Int,na.rm = T)
summary.Data$Intensity <- IntQuan
tempScoreInt <- log10(IntQuan[3])/log10(thresholds$Intensities)[1]
score$Intensity <- tempScoreInt

# check MSMS
try(msmsInfo <- msmsPlot(path = path, RawFilesUsed=  RawFilesUsed))
if(!exists("msmsInfo")){
	msmsInfo <- rep(0,5)
}
if(all(msmsInfo$MSMSint == 0)){
	
	summary.Data$msmsQuantile  <- c(0,0,0,0)
	summary.Data$msmsMassCount <- c(0,0,0,0)
	score$msmsQuantile <- 0
	score$msmsCount <- 0
}else{
summary.Data$msmsQuantile <- msmsInfo$MSMSint
summary.Data$msmsMassCount <- msmsInfo$MSMSn

score$msmsQuantile <- 	(log10(msmsInfo$MSMSint[3])/thresholds$msmsQuantile[1]*0.3)^1.25 + 
										(log10(msmsInfo$MSMSint[4])/thresholds$msmsQuantile[2]*0.3)^1.25 + 
										(log10(msmsInfo$MSMSint[5])/(thresholds$msmsQuantile[2]*1.2)*0.1)^1.25+ 
										(log10(msmsInfo$MSMSint[1])/(thresholds$msmsQuantile[1]*0.7)*0.1)^1.25 + 
										(log10(msmsInfo$MSMSint[2])/(thresholds$msmsQuantile[1]*0.9)*0.2)^1.25

score$msmsCount 	<-  msmsInfo$MSMSn[3]/thresholds$msmsCount[1]*0.5 + msmsInfo$MSMSn[2]/(thresholds$msmsCount[2] - diff(thresholds$msmsCount))*0.25 + msmsInfo$MSMSn[4]/(thresholds$msmsCount[2])*0.25


}

# Check Summary 
summaryPath <- list.files(path,pattern = "proteinGroups.txt",full.name = T)
if(length(summaryPath) > 0){
	summaryFile <- read.csv(summaryPath,sep = "\t")
	BSA <- summaryFile[grep("P02769",summaryFile$Majority.protein.IDs),]
	if(is.data.frame(BSA)){
		Coverage 	<- 	BSA$Sequence.coverage....
	}else{
		Coverage 	<- 	paste(quantile(summaryFile$Sequence.coverage....,na.rm = T),collapse = " # ")
	}
	
	
	

	
}else{Coverage <- NA}

summary.Data$Coverage <- Coverage
if(length(thresholds$ProteinCoverage) == 0){thresholds$ProteinCoverage <- 50}
score$ProteinCoverage <- summary.Data$Coverage/thresholds$ProteinCoverage

#####
#Combined Scores
#####

# 1. nLC shape Combi
nLCvec <- c(score$quanRetRSD,score$quanRet50ratio,score$quanRetSlope)
nLCvec[nLCvec > 1] <- 1
score$nLCcombi <- mean(nLCvec)



msmsEff <- NA
try(msmsEff <- length(Data.i.quant$ms.ms.ids[!is.na(Data.i.quant$ms.ms.ids)])/(max(as.numeric(Data.i.quant$ms.ms.ids),na.rm = T)-min(as.numeric(Data.i.quant$ms.ms.ids),na.rm = T))*100)
# }else{msmsEff <- 0}

# }else{
	# msmsEff <- as.numeric(msmsEff[dim(msmsEff)[1],2])/thresholds$msmsEff
# }
summary.Data$msmsEff 	<- msmsEff
# if(is.vector(msmsEff)){msmsEff <- as.matrix(msmsEff)}
score$msmsEff 			<- msmsEff/thresholds$msmsEff
summary.Data <<- summary.Data
thresholds <<- thresholds
# scores 
score$msms 			<-  summary.Data$quan.msms.min/thresholds$quan.msms.min 
score$mass.error 	<-  thresholds$mass.error.cal[1]/max(abs(summary.Data$mass.error.cal[c(2,4)]))*0.5+ thresholds$mass.error.cal[1]/max(abs(summary.Data$mass.error.cal[c(3)]))*0.5
score$score <- summary.Data$score[3]/thresholds$score
# score nlc
score$peak.shape 	<- thresholds$ret.peak.shape[1]/max(abs(log2((summary.Data$ret.peak.shape[c(2,4)]))))

score$ret.width 	<- thresholds$ret.width[1]/(summary.Data$ret.width[c(3)])

score$quan.duplicates.msms 	<- thresholds$quan.duplicates.msms[1]/((((summary.Data$quan.duplicates.msms))))


# 2. MS combi score
MSvec <- c(score$Intensity,score$mass.error,score$msms)
MSvec[MSvec > 1] <- 1
score$combiMS <- mean(MSvec)
# 3. MSMS combi score
MSvec <- c(score$msmsCount,score$msmsQuantile,score$msms)
MSvec[MSvec > 1] <- 1
score$combiMSMS <- mean(MSvec)
# 4. nLC combi score
nLCvec <- c(score$nLCcombi,score$peak.shape,score$ret.width)
nLCvec[nLCvec > 1] <- 1
score$LCcombi <- mean(nLCvec)


# efficiency 
# msmsEff <- sumDat()
# if(length(msmsEff) == 1){
# if(any(is.na(msmsEff))){


return(list(th = thresholds,sc = score,sd = summary.Data,diq = Data.i.quant))
}
#qc.prepare.data <- qc.prepare(temp.DataEvidence, SpeciesTable,placeholder = placeholder,templateFasta =templateFasta,path = .path)
#print(qc.prepare.data$sc$msmsQuantile)

#Data.list <- qc.prepare(Data)
#Data <- temp.DataEvidence
# qc.prepare.data <- qc.prepare(temp.DataEvidence, SpeciesTable,placeholder = placeholder,templateFasta =templateFasta,path = .path)
# print(qc.prepare.data$sc)
# print(qc.prepare.data$sd$mass.error.cal)
# tryError <- class(try(TotalScoreRes  <- plot.scores(temp.DataEvidence,qc.prepare.data,i, open.doc = T,pdfOut = pdfOut)))
