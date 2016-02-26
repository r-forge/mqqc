qc.prepare <- 
function(Data, SpeciesTable,placeholder,templateFasta,path = "./", filename = NULL,BSAID = "P02769",selectedFile = 1,RESettings = RESettings,Peptides = NULL,AllPeptides = NULL,MSMS = NULL){
score <- list()

sumDat <- function(){
	temp <- list.files(path,pattern = "summary",full.name = T)
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
mass.error 		<- c(0,-3,3)
thresholds$mass.error 		<- c(0,-3,3)
thresholds$mass.error.cal 		<- c(0,-3,3)
thresholds$msms.count 		<- 4000
thresholds$ret.peak.shape <- c(0,-1,1)
thresholds$ret.width 		  <- c(0.3,0,1)
thresholds$total.msms.min	<- 150
thresholds$quan.msms.min	<- 200
thresholds$quan.duplicates.msms <- 0.05
thresholds$score 			    <- 100
thresholds$msmsEff 			  <- 30
thresholds$quanRetRSD 		<- 0.1
thresholds$quanRet50ratio	<- 1.2
thresholds$quanRetSlope 	<- 1

thresholds$msmsQuantile	 <-  c(4.5,4,5) # log10 Int
thresholds$msmsCounts 	<- c(25,15,35)
thresholds$Intensities 			<- c(8,6.5,9.5) # log10 int
thresholds$ProteinCoverage <- 30
thresholdsBA <- thresholds
thresholds$MSID.min <- 2000
if(SpeciesTable){
		colnames(Data) <- tolower(colnames(Data))

	      species 	<- read.csv(paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"))
        if(length(species) == 1){
        species 	<- read.csv2(paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"))
        }
	      RawFile <- unique(Data$raw.file)[1]
	      regEx 	<- sapply(species$Abbreviation,function(x){gsub(placeholder,x, 	templateFasta,fixed = T)})			
	      temp   	<- as.logical(sapply(regEx,grep, x = RawFile))
		    temp[is.na(temp)] <- FALSE
        
		if(any(temp)){
		speciesUsed <- species[temp,]
		thresholds	<- as.list(speciesUsed[1,])
		thresholds <- lapply(thresholds,function(x){
			x <- as.character(x)
			x <- unlist(strsplit(x," "))
			x <- as.numeric(x)
			
		})
		
		}else{
		  speciesUsed <- species[species$Abbreviation == "default",]
		  thresholds	<- as.list(speciesUsed[1,])
		  thresholds <- lapply(thresholds,function(x){
		    x <- as.character(x)
		    x <- unlist(strsplit(x," "))
		    x <- as.numeric(x)
		    
		  })
		  if(dim(speciesUsed)[1] == 0){
		    thresholds <- thresholdsBA
		    cat("\n\nWARNING! Wether a parameter ID nor a default method was detected. Jumping to preset thresholds. Scoring might be deficient\n\n")
		  }
		  
		  
		  
		}
}

# pdf("SpeciesStatistics.pdf",width = 10)
try(SpecStat <- SpeciesStatistic(Data$leading.razor.protein,F))
# graphics.off()
# system("open SpeciesStatistics.pdf")
if(exists("SpecStat")){
  MostProperSpecies = SpecStat
  
}else{MostProperSpecies = NA;  SpecStat = NA}
# if(all(is.na(SpecStat))){
#   unlink("SpeciesStatistics.pdf")
# }

#.cols <- colnames(Data)
#Data <- apply(Data,2,function(x){as.numeric(as.character(x))})
#colnames(Data) <- .cols
Data <- as.data.frame(Data)
grep.col <- function(string,Data,fixed =  T){
  x <- grep(string,colnames(Data),fixed = fixed)
if(length(x) == 0){
	 x <- 0
}
if(length(x) > 1){
	 cat("\rwarning, more than one match found",rep(" ",100))
}
return(x)	

}


# Initiate output list
summary.Data <- list()
# read.Data
#Data <- temp.i
colnames(Data) <- tolower(colnames(Data))



raw.files <- grep.col("raw.file",Data)

# to make sure that only one file is processed
Data$reverse[is.na(Data$reverse)] <- ""
Data.i  <- Data[unique(Data[,raw.files])[selectedFile] == Data[,raw.files],]
Data.i  <- Data.i[Data.i$reverse != "+",]
#Data.i <<- Data.i
RawFilesUsed <- unique(Data.i$raw.file)


# include Dependent Peptides Module
summary.Data$DependentPeptides <- NULL
if(all(1)){
  #print("Dependent")

  if(file.exists(DPfile <- paste(path,"allPeptides.txt",sep = "/"))|length(AllPeptides) > 0){
    DepPepFun<- function(x,filename = "DPpie",NormPep = NULL,unknowns = T,cbPalette = rainbow(10),maxShow = 20){ 
      if(!is.data.frame(x)){
        DPlines     <- read.csv(x,sep = "\t",stringsAsFactors = F)}else{DPlines <- x}
      DPlinesSig  <- DPlines[as.numeric(DPlines$"DP.PEP") < 0.01,]
      DPlinesSig$DP.Modification[DPlinesSig$DP.Modification == ""] <- "unknown"
      if(dim(DPlinesSig)[1] > 0){
        
        Val <- aggregate(DPlinesSig$DP.Mass.Difference,list(DPlinesSig$DP.Modification),function(x){c(length(x),median(x,na.rm = T))})
        ValR <- Val[[2]]
        rownames(ValR) <- Val[,1] 
        if(!unknowns){
          ValR <- ValR[rownames(ValR) != "unknown",]
        }
        ValRsum   <- sum(ValR[,1],na.rm = T)
        ValRrest  <- ValR[ ExVec<- ValR[,1]/sum(ValR[,1]) < 0.01,]
        ValRrest <- sum(ValRrest[,1])
        ValR <- ValR[!ExVec,]
        ValR <- rbind(ValR,c(ValRrest,NA))
        rownames(ValR)[dim(ValR)[1]] <- "Other"
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","tomato3")
        ValR <- ValR[order(ValR[,1],decreasing = T),]
      }else{
        ValRsum = 0
        ValR = matrix(0)
        rownames(ValR) <- "NA"
      }
      if(length(NormPep) > 0){
        width = 18  
      }else{width = 8}
      if(dim(ValR)[1] > maxShow){
        ValR <- ValR[1:maxShow,]
      }
      if(ValRsum != 0){
        pdf(pdfname <- paste(paste("DepPepPie",filename,sep = "_"),".pdf",sep = ""),width =width)
        
        
        #tempy <- Hui$counts[peaksf]
        #peaksf <<- peaksf[tempy/max(tempy)]
        
        
        if(length(NormPep) > 0){
          par(mfrow = c(1,2))
          pie(c(NormPep,ValRsum),labels = paste(c("Identified","Dependent Peptides"),"n:",c(NormPep,ValRsum)),border = "transparent",col = c("red","grey"),main = "All Peptides",angle = 30,density = c(NA,20))
        }
        if(ValRsum == 0){
          empty.plot()
          legend("top",legend = "No dependent peptides detected.",xpd = NA,bty = "n")
        }else{
          pie(ValR[,1],labels = paste(rownames(ValR),"n:",round(ValR[,1],2)),border = "transparent",col = colorRampPalette((cbPalette))(dim(ValR)[1]),main = "Dependent Peptides")    
        }
        par(mfrow = c(1,1))
        
        PlotDPHist <- function(x){
          Hui <- hist(x,breaks = 100000,plot = F)
          
          plot(Hui$breaks[-1][Hui$counts > 0],Hui$counts[Hui$counts > 0],xlim = c(-50,50),type = "h",frame = F,xlab = "Mass Difference [Da]",ylab = "n",main = "")
          abline(h = 0)
          if(length(grep("pracma",library())) > 0){
            try(library(pracma))
            try(peaksf <-findpeaks(Hui$counts))
            peaksf <- peaksf[,2]
            
          }
          if(!exists("peaksf")){
            peaksf <- (1:length(Hui$counts))[Hui$counts >= sort(Hui$counts,decreasing = T)[15]]
          }
          text(Hui$breaks[peaksf],Hui$counts[peaksf],round(as.numeric(as.character(Hui$breaks[peaksf])),2),col = 2,type = "h",pos = 3,srt = 90,xpd = NA,cex = 0.8)
          
          
        }
        try(PlotDPHist(DPlinesSig$DP.Mass.Difference))
        
        
        dev.off()
        #system(paste("open",pdfname))
        return( paste(apply(cbind(rownames(ValR),ValR),1,paste,collapse = "##"),sep = " ",collapse = "_#_")) 
        
      }else{return("No DP Peptides found.")}
    }
summary.Data$DependentPeptides  <- "No DP Peptides found."
if(length(AllPeptides) > 0){
  if(is.matrix(AllPeptides)|is.data.frame(AllPeptides)){
    DPfile <- AllPeptides
  }
}

try(summary.Data$DependentPeptides <-  DepPepFun(DPfile,NormPep = dim(Data.i)[1],cbPalette = cbPalette,filename = filename))
  }
}


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

quant.range <- quantile(reten.mid,na.rm = T)[2:4]
ident.peps 	<- reten.mid > min(quant.range,na.rm = T) & reten.mid < max(quant.range,na.rm = T)

summary.Data$ret.peak.shape <- quantile(ret.peak.ratio[ident.peps],na.rm = T) # filtered for inner elution time quantile
summary.Data$ret.width 		<- quantile(ret.total[ident.peps],na.rm = T)#filtered for inner elution time quantile
summary.Data$ret.max		<- max(reten.mid[ident.peps],na.rm = T)#filtered for inner elution time quantile
summary.Data$mz <- quantile(Data.i[,grep.col("^m.z$", Data.i,FALSE)],na.rm = T) # filtered for inner elution time quantile

Data.i.quant <- Data.i[ident.peps,] # subset with innerj range

# MSMS
summary.Data$total.msms.min <-  summary.Data$msms.count/summary.Data$ret.max
summary.Data$quan.msms.min 	<-  length(grep("MSMS",Data.i.quant$type))/(max(quant.range,na.rm = T) - min( quant.range,na.rm = T))

# mass error 

mass.error 		<- Data.i[,grep.col("mass.error..ppm",Data.i)]
mass.error.uncal 	<- mass.error[,grep.col("uncalibrated",mass.error)]
mass.error 		<- mass.error[,-grep.col("uncalibrated",mass.error)]
summary.Data$mass.error.cal 	<- quantile(as.numeric(mass.error),na.rm = T)
summary.Data$mass.error.uncal 	<- quantile(as.numeric(mass.error.uncal),na.rm = T)
# duplicates
pep.identifier 			        <- paste(round(Data.i.quant$m.z),round(Data.i.quant$charge), Data.i.quant$modified.sequence)
double 					            <- length(pep.identifier)-length(unique(pep.identifier))
summary.Data$quan.duplicates 		<- double
summary.Data$quan.duplicates.msms 	<- double/length(grep("MSMS",Data.i.quant$type))
summary.Data$score<- quantile(Data.i$score,na.rm = T)
# sd interquantile
# print("HUI")

if(length(Data.i$calibrated.retention.time[!is.na(Data.i$calibrated.retention.time)])> 1){
  # print("HUI")
  tempQuan   <- quantile(Data.i$calibrated.retention.time,na.rm = T)
  
# try(temp2 <- density(sl));temp2$y <- temp2$y*temp2$n/sum(temp2$y)
# #NF <- length(Data.i$calibrated.retention.time)/length(AllPeptides$Retention.time[!is.na(AllPeptides$Retention.time)])
# 
#   #temp$y <- temp$y * NF
# plot(temp2)
# points(temp,col = 2,type = "l")
# 
# sl <- AllPeptides$Retention.time[AllPeptides$Intensity >quantile(AllPeptides$Intensity)[2]]
# 
# 
# length(sl)
# hist(sl,breaks = 1000)
# hist(Data.i$retention.time,add = T,border = 2,breaks = 100)
if(length(AllPeptides) > 0){
tempL <- 1:2
}else{tempL <- 1}
for(deci in tempL){
if(deci == 1){
try(temp   	  <- density(Data.i$calibrated.retention.time))#;temp$y <- temp$y*temp$n/sum(temp$y)
}
if(deci == 2){
  try(temp   	  <- density(AllPeptides$Retention.time))#;temp$y <- temp$y*temp$n/sum(temp$y)
}
x <- temp$x
y <- temp$y

ySel <- y[x > tempQuan[2]&x< tempQuan[4]]
xSel <- x[x > tempQuan[2]&x< tempQuan[4]]
y<- y
x<- x
ySel <- ySel/mean(ySel,na.rm = T)
xSel <- xSel-min(xSel,na.rm = T)
xSel <- xSel/max(xSel,na.rm = T) 
if(deci == 1){
slope <- NA
rSDquanRet <- NA
try(rSDquanRet				<- sd(ySel,na.rm = T)/mean(ySel,na.rm = T))
try(slope <- coefficients(lm(scale(ySel)~xSel))[2])
}else{
  slopeALL <- NA
  rSDquanRetALL <- NA
  try(  rSDquanRetALL				<- sd(ySel,na.rm = T)/mean(ySel,na.rm = T))
  try(slopeALL <- coefficients(lm(scale(ySel)~xSel))[2]) 
}
}

}else{
  rSDquanRet <- NA
  slope <- NA
}

summary.Data$quanRetRSD <- rSDquanRet
summary.Data$quanRetSlope <- slope
#tempQuan <- tempQuan

RatioIQuan <- diff(tempQuan[c(2,4)])/diff(tempQuan[c(1,5)]) # Ratio between inner and outer quantile distance of retention time, the bigger the better
summary.Data$RatioIQuan <- RatioIQuan 
summary.Data$quanRet50ratio <- log2( diff(tempQuan[c(2,3)])/diff(tempQuan[c(3,4)]))



score$quanRetRSD 		  <- 	ThreshCompare(1/summary.Data$quanRetRSD,1/thresholds$quanRetRSD,type = "single")
s <- summary.Data$quanRetRSD
r <- thresholds$quanRetRSD
score$quanRetSlope 		<-	ThreshCompare(1/abs(summary.Data$quanRetSlope),1/thresholds$quanRetSlope,type = "single")
score$quanRet50ratio 	<- 	ThreshCompare(1/abs(summary.Data$quanRet50ratio),1/abs(log2(thresholds$quanRet50ratio)),type = "single")
# if(abs(summary.Data$quanRet50ratio) > 1){
# score$quanRet50ratio 	<- 	ThreshCompare(abs(summary.Data$quanRet50ratio),abs(thresholds$quanRet50ratio))}else{
# score$quanRet50ratio 	<- 	ThreshCompare(thresholds$quanRet50ratio,abs(summary.Data$quanRet50ratio))#?
# }
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

IntQuan <- IntQuan 
thresholds <- thresholds

score$Intensity <- ThreshCompare(log10(signif(IntQuan[3])),thresholds$Intensities,type = "quantile",cat = "high")

# check MSMS
if(file.exists(peppath<- paste(path,"peptides.txt",sep = "/"))|length(Peptides) > 0){
  if(length(Peptides) > 0){
    if(is.data.frame(Peptides)){
      tempPep <- Peptides
    }else{    
      tempPep <- read.csv(peppath,sep = "\t")
    }
  }else{
	  tempPep <- read.csv(peppath,sep = "\t")
  }
	tempPep$Missed.cleavages[tempPep$Missed.cleavages > 0] <- 1
	TempMis <- aggregate(tempPep$Missed.cleavages,list(tempPep$Missed.cleavages),length)
	val1 <- TempMis[TempMis[,1] == 0,2]
	val2 <- TempMis[TempMis[,1] > 0,2]
	if(length(val1) == 0){val1 = 0}
	if(length(val2) == 0){val2 = 0}
	rat = val2 /(val1+ val2)*100
	
}else{rat = "not available"}

summary.Data$missed.cleavages.percent =as.character(rat)

try(msmsInfo <- msmsPlot(path = path, RawFilesUsed=  RawFilesUsed,quant.range = range(quant.range),MSMS.Import = MSMS))
if(!exists("msmsInfo")){
	msmsInfo <- rep(0,5)
	msmsInfo <-	list(MSMSint = "nodata",MSMSn = "nodata")
}else{
	if(!is.list(msmsInfo)){
			msmsInfo <-	list(MSMSint = "nodata",MSMSn = "nodata")
	}
}
msmsInfo$MSMSint[is.na(msmsInfo$MSMSint)] <- 0
if(all(msmsInfo$MSMSint == 0)|all(is.character(unlist(msmsInfo)))){
	
	summary.Data$msmsQuantile  <- c(0,0,NA,0,0)
	summary.Data$msmsMassCount <- c(0,0,NA,0,0)
	score$msmsQuantile <- NA
	score$msmsCount <- NA
}else{
summary.Data$msmsQuantile <- msmsInfo$MSMSint
summary.Data$msmsMassCount <- msmsInfo$MSMSn
score$msmsQuantile <-  ThreshCompare(log10(summary.Data$msmsQuantile)[3],thresholds$msmsQuantile,type = "quantile",cat = "high")
###
score$msmsCount 	<-  ThreshCompare((summary.Data$msmsMassCount)[3],thresholds$msmsCount,type = "quantile",cat = "high")#ThreshCompare(msmsInfo$MSMSn[3], thresholds$msmsCount) #(sig - lo)/(ref[1]-lo)

}
# Check Summary 
summaryPath <- list.files(path,pattern = "proteinGroups.txt",full.name = T)
# include Protein entry, in case older sepcies table is used
noCoverage <- F
if(length(speciesUsed$Protein)==0){
  if(dim(speciesUsed)[1] == 0){
    speciesUsed <- rbind(speciesUsed,"")
    noCoverage <- T
  }
  speciesUsed$Protein <- ""
}
if(nchar(as.character(speciesUsed$Protein)) > 0){
	summaryFile   <- read.csv(summaryPath,sep = "\t")
	BSA           <- summaryFile[grep(speciesUsed$Protein,summaryFile$Majority.protein.IDs),]
	if(is.data.frame(BSA)){
		Coverage 	<- 	BSA$Sequence.coverage....
	}else{
		Coverage 	<- 	paste(quantile(summaryFile$Sequence.coverage....,na.rm = T),collapse = " # ")
		speciesUsed$Protein <- ""
	}
  
  }else{
  if(file.exists(summaryPath)&!noCoverage){
  summaryFile   <- read.csv(summaryPath,sep = "\t")
  Coverage 	    <- 	median(summaryFile$Sequence.coverage....,na.rm = T)#paste(quantile(summaryFile$Sequence.coverage....,na.rm = T),collapse = " # ")  
  }else{Coverage <- NA}
}
	
	
	

	

summary.Data$Coverage <- Coverage
if(length(thresholds$ProteinCoverage) == 0){thresholds$ProteinCoverage <- 50}
score$ProteinCoverage <- ThreshCompare(summary.Data$Coverage,thresholds$ProteinCoverage)

#####
#Combined Scores
#####

# 1. nLC shape Combi
nLCvec <- c(score$quanRetRSD,score$quanRet50ratio,score$quanRetSlope)
nLCvec[nLCvec > 1] <- 1
score$nLCcombi <- mean(nLCvec)
# print(score$nLCcombi)

#AllPeptides
msmsEff <- msmsInfo$MSMSEff*100
#msmsEff <- sumDat() # gets msms effecency info from summary table

 if(length(msmsEff) == 0){
   
 #	try(msmsEff <- length(Data.i.quant$ms.ms.ids[!is.na(Data.i.quant$ms.ms.ids)])/(max(as.numeric(Data.i.quant$ms.ms.ids),na.rm = T)-min(as.numeric(Data.i.quant$ms.ms.ids),na.rm = T))*100)
 # try(msmsEff <- length(Data.i.quant$ms.ms.ids[!is.na(Data.i.quant$ms.ms.ids)])/(max(as.numeric(Data.i.quant$ms.ms.ids),na.rm = T)-min(as.numeric(Data.i.quant$ms.ms.ids),na.rm = T))*100)
    msmsEff <- NA

    
   
 }
MSID.min = NA
if(length(AllPeptides) > 0){
  allMSMS <- (AllPeptides$Sequence != " ")
  allMSMS <- allMSMS[allMSMS]
  msmsEffAll <-  length(Data.i$reverse[Data.i$reverse != "+"])/dim(AllPeptides)[1]*100
  
  sel       <- AllPeptides$Retention.time > min(quant.range) & AllPeptides$Retention.time < max(quant.range)
  allMSMS   <- (AllPeptides$Sequence[sel] != " ")
  Features  <-length(sel[sel])
  #allMSMST  <- allMSMS[allMSMS]
  #allMSMSF  <- allMSMS[!allMSMS]
  
  msmsEffQuantile <-  length(Data.i.quant$reverse[Data.i.quant$reverse != "+"])/length(allMSMS)*100
  msmsEff <- msmsEffQuantile
  tempMSIDMIN <- unlist(Features/abs(diff(quant.range[c(1,3)])))
  names(tempMSIDMIN) <- NULL
  summary.Data$MSID.min <- tempMSIDMIN 
  score$MSID.min <-   ThreshCompare(summary.Data$MSID.min,thresholds$MSID.min )
  
}else{score$MSID.min <- NA;summary.Data$MSID.min <- NA}


#else{
# 	if(dim(msmsEff)[1] == 2){
# 		msmsEff <- msmsEff[1,2]
# 	}else{
# 		if(dim(msmsEff)[1] == 2& filename != 0){
# 			msmsEff <- msmsEff[msmsEff[,1] == gsub(".raw$","",filename),2]
# 		}else{msmsEff <- 0}
# 
# 	}
# 
# }
msmsEff <- as.numeric(msmsEff)

# }else{msmsEff <- 0}

# }else{
	# msmsEff <- as.numeric(msmsEff[dim(msmsEff)[1],2])/thresholds$msmsEff
# }
summary.Data$msmsEff 	<- msmsEff
summary.Data$msmsEffall 	<- msmsEffAll
# if(is.vector(msmsEff)){msmsEff <- as.matrix(msmsEff)}
score$msmsEff 			<- ThreshCompare(msmsEff,thresholds$msmsEff)
summary.Data <- summary.Data
thresholds <- thresholds
# scores 
score$msms 			  <-  ThreshCompare(summary.Data$quan.msms.min,thresholds$quan.msms.min )
score$mass.error 	<-  ThreshCompare(abs(summary.Data$mass.error.uncal[3]),(thresholds$mass.error.cal),type = "quantile",cat = "fixed")
#thresholds$mass.error.cal[1]/max(abs(summary.Data$mass.error.uncal[c(2,4)]))*0.5+ thresholds$mass.error.cal[1]/max(abs(summary.Data$mass.error.uncal[c(3)]))*0.5
score$score <- ThreshCompare(summary.Data$score[3],thresholds$score)
# score nlc
score$peak.shape 	<- ThreshCompare(log2(summary.Data$ret.peak.shape[3]),thresholds$ret.peak.shape,type = "quantile",cat = "fixed")

score$ret.width 	<- ThreshCompare(log2(summary.Data$ret.width[c(3)]),log2(thresholds$ret.width),type = "quantile",cat = "low")

score$quan.duplicates.msms 	<- ThreshCompare(summary.Data$quan.duplicates.msms,thresholds$quan.duplicates.msms)


# 2. MS combi score
#MSvec <- c(score$Intensity,score$mass.error,score$msms)
MSvec <- c(score$MSID.min,score$mass.error,score$Intensity)

MSvec[MSvec > 1] <- 1
score$combiMS <- mean(MSvec,na.rm = T)
# 3. MSMS combi score
MSMSvec <- c(score$msms,score$msmsQuantile,score$msmsEff)
MSMSvec[MSMSvec > 1] <- 1
score$combiMSMS <- mean(MSMSvec,na.rm = T)
# 4. nLC combi score
nLCvec <- c(score$nLCcombi,score$peak.shape,score$ret.width)
nLCvec[nLCvec > 1] <- 1
score$LCcombi <- mean(nLCvec)
summary.Data$LCcombiScore <- mean(nLCvec,na.rm = T)

# efficiency 
# if(length(msmsEff) == 1){
# if(any(is.na(msmsEff))){
parID <- speciesUsed$Abbreviation
if(length(parID) == 0){
  parID <- "not detected"
}
return(list(th = thresholds,sc = score,sd = summary.Data,diq = Data.i.quant,IdentifiedProteins = speciesUsed$Protein,parID = parID,SpecStat = MostProperSpecies))
}
#tryError1 <- class(try(qc.prepare.data <- qc.prepare(Data =  temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID)))
#tryError1 <- class(try(qc.prepare.data <- qc.prepare(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =AllPeptides,MSMS = MSMS)))
# start.qc()
#tryError1 <- class(try(qc.prepare.data <- qc.prepare(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =AllPeptides,MSMS = MSMS)))
#tryError1 <- class(try(qc.prepare.data <- qc.prepare(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =AllPeptides,MSMS = MSMS)))
