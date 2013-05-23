qc.prepare <- 
function(Data, SpeciesTable,placeholder,templateFasta){
score <- list()

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
thresholds$quan.msms.min	<- 250
thresholds$mass.error.cal <- c(0.5,5)
thresholds$quan.duplicates.msms <- 0.05
thresholds$score 			    <- 100
thresholds$msmsEff 			  <- 60
thresholds$quanRetRSD 		<- 0.05
thresholds$quanRetSlope 	<- 0.02
thresholds$quanRet50ratio	<- 1.2
if(SpeciesTable){
	
	      species 	<- read.csv(paste(path.package("mqqc"),"Data/MQQCspecies.csv",sep = "/"))
	      RawFile <- unique(Data$Raw.file)[1]
	      regEx <- sapply(species$Abbreviation,function(x){gsub(placeholder,x, 	templateFasta,fixed = T)})	
	      temp   	<-as.logical(sapply(regEx,grep, x = RawFile))
		temp[is.na(temp)] <- FALSE

		if(any(temp)){
		speciesUsed <- species[temp,]
		thresholds	<- as.list(speciesUsed[1,])
		}
}
#.cols <- colnames(Data)
#Data <- apply(Data,2,function(x){as.numeric(as.character(x))})
#colnames(Data) <- .cols
Data <- as.data.frame(Data)
grep.col <- function(string,Data){x <- grep(string,colnames(Data),fixed = T)
if(length(x) == 0){
	#alarm()
	; x <- 0
}
if(length(x) > 1){
	alarm(); cat("\rwarning, more than one match found",rep(" ",100))
}
return(x)	
	}

summary.Data <- list()
# read.Data
#Data <- temp.i
colnames(Data) <- tolower(colnames(Data))


raw.files <- grep.col("raw.file",Data)

Data.i <- Data[unique(Data[,raw.files])[1] == Data[,raw.files],]




# MSMS
type.ident <- Data.i[,grep.col("type", Data.i)]
msms.count <- length(grep("MSMS", type.ident))
summary.Data$msms.count <- msms.count

# Ret.time
reten.ident 	<- Data.i[,grep.col("retention.time", Data.i)]
reten.start 	<- reten.ident[,grep.col("start",reten.ident)]
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
try(slope <- coefficients(lm(y~x))[2])
rSDquanRet				<- sd(ySel)/median(ySel)
summary.Data$quanRetRSD <- rSDquanRet
summary.Data$quanRetSlope <- slope
summary.Data$quanRet50ratio <- diff(tempQuan[c(1,3)])/diff(tempQuan[c(3,5)])
#print("HUI")
#print(score)
score$quanRetRSD 		<- 	thresholds$quanRetRSD/summary.Data$quanRetRSD
score$quanRetSlope 		<-	thresholds$quanRetSlope /abs(summary.Data$quanRetSlope)
score$quanRet50ratio 	<- 	thresholds$quanRet50ratio/abs(summary.Data$quanRet50ratio)

# efficiency 
msmsEff <- NA
try(msmsEff <- length(Data.i.quant$ms.ms.scan.number[!is.na(Data.i.quant$ms.ms.scan.number)])/(max(Data.i.quant$ms.ms.scan.number,na.rm = T)-min(Data.i.quant$ms.ms.scan.number,na.rm = T)*0.9)*100)
summary.Data$msmsEff 	<- msmsEff
score$msmsEff 			<- msmsEff/thresholds$msmsEff

# scores 
score$msms 			<-  summary.Data$quan.msms.min/thresholds$quan.msms.min 
score$mass.error 	<-  max(thresholds$mass.error.cal[1]/abs(summary.Data$mass.error.cal[c(2,4)]))*0.7+max(abs(summary.Data$mass.error.cal[c(1,5)])/thresholds$mass.error.cal[2])*0.3
score$score <- summary.Data$score[3]/thresholds$score
# score nlc
score$peak.shape 	<- thresholds$ret.peak.shape[1]/max(abs(log2((summary.Data$ret.peak.shape[c(2,4)]))))

score$ret.width 	<- thresholds$ret.width[1]/(summary.Data$ret.width[c(3)])

score$quan.duplicates.msms 	<- thresholds$quan.duplicates.msms[1]/((((summary.Data$quan.duplicates.msms))))
#print(score)
return(list(th = thresholds,sc = score,sd = summary.Data,diq = Data.i.quant))
}
#Data.list <- qc.prepare(Data)
#print(test$sc)