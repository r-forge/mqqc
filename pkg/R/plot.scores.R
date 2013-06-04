plot.scores <-
function (data.i,data.list,pdf.name = "qc.control", open.doc = T,pdfOut = T)
{
	cat("\rplotting scores",rep(" ",100))
#initiation of important vectors 
grad.cols.vec <- c("black","blue","lightblue",colors()[50])


colnames(data.i) <- tolower(colnames(data.i))
summary.data 	<- data.list$sd
thresholds 		<- data.list$th
score		 	<- data.list$sc
data.i.quant 	<- data.list$diq

na.inf.fun <- function(x){
	x[is.na(x)] 		<- 0
	x[is.infinite(unlist(x))] 	<- 0
	return(x)
}


score     <- na.inf.fun(score)

color.blind <- list(	

		yellow = rgb(240,228,66,max = 255),
		orange = rgb(230,159,0,max = 255),
		vermillion = rgb(213,94,0,max = 255),
		bluishgreen = rgb(0,158,115,max = 255),
		reddishpurple = rgb(204,121,167,max = 255), 		skyblue = rgb(86,180,233,max = 255),
		blue = rgb(0,133,178,max = 255)

)

  if(pdfOut){
pdf(.pdf<- paste(pdf.name,".pdf",sep = ""),width = 15,height = 7.5,pointsize = 20)
# plot scores
}
library(gplots)
real.data 		<- summary.data
ref.data 		<- thresholds
score.data 	<- score
ms.col <- unlist(color.blind[c(5,6,1)])
nc.col <- unlist(color.blind[c(3,4,2)])
sum.scores <- c(sum(unlist(score.data[1:3])),sum(unlist(score.data)[4:6]))

TotalScore <- unlist(score.data) 
TotalScore[TotalScore > 1] <- 1
TotalScore <- sum(TotalScore)/(length(TotalScore)-1)

if(any(sum.scores > 3)){
	
	temp.xlim <- c(0,max(sum.scores))
	
	}else{temp.xlim <- c(0,3)
		
}
nrowVal <- 6
ncolVal <- 7
extra <- 0
totalFill 	<- nrowVal* ncolVal

profileSpace <- matrix(c(rep(1,(nrowVal-1)),2,rep(3,(nrowVal-1)),4),ncol = 2,nrow = nrowVal)
scoreSpace 	 <- matrix(c(rep(5,2*(ncolVal-3)),6,6),ncol =(ncolVal-2),nrow = 2 )
leftSpace	 	<- (nrowVal-2)*(ncolVal-2)/2
leftSpace 		<- seq(from = 7,to = (leftSpace+6))
topLeftSpace 	<- min(leftSpace):leftSpace[length(leftSpace)/2]
downLeftSpace 	<- (leftSpace[length(leftSpace)/2]+1):max(leftSpace)
leftSpace 		<- rbind(topLeftSpace, topLeftSpace, downLeftSpace, downLeftSpace)


totalSpace 		<- cbind(profileSpace,rbind(scoreSpace,leftSpace))


layout(totalSpace,width = c(2.3,0.3,0.5,0.5,0.5,0.5,0.5))
par(mai = c(0,1,0.1,0))

if(any(grep.col("calibrated.retention.time.start",data.i) == 0|grep.col("calibrated.retention.time.finish",data.i) == 0)){
	dots <- F
}else{dots <- T}

try(plot.profile(data.i,F,dots))

par(mai = c(0.5,2,0.2,0.1))



#assc#ign("score.data",score.data,envir = .GlobalEnv)
#barplot2(cbind(score.data[1:3],rep(0,3)),names.arg = c("MS-score","nLC"),ylim = temp.xlim,col = ms.col,las = 2,ylab = "score")
#temp.pos <- barplot2(cbind(rep(0,3),score.data[4:6]),ylim = temp.xlim,col = nc.col,las = 2,axes = F,add = T)

round.spec <- function(x){ 
		x<- round(x*100)
		x[x > 100] <- 100
		x[is.na(x)] <- 100
		x[x == 0] <- 1
		return(x)
		}
col.temp <- (colorRampPalette(grad.cols.vec)(100))
score.data <- score.data[names(score.data)!=""]
orderScores <- c("msms","mass.error","score","msmsEff","quan.duplicates.msms","ret.width","peak.shape","quanRetRSD","quanRetSlope","quanRet50ratio")

score.order <- merge.control(names(score.data),orderScores)
diff 		<- setdiff(1:length(names(score.data)),score.order[!is.na(score.order)])
if(length(diff) > 0){
	score.order[is.na(score.order)] <- diff
}

score.data <- score.data[rev(score.order)]



namesData <- names(score.data)

namesData[namesData=="quanRetRSD"] <- "ETime rSD"
namesData[namesData=="quanRetSlope"] <- "ETime slope"
namesData[namesData=="quanRet50ratio"] <- "ETime balance"
namesData[namesData=="msmsEff"] <- "Peptide ID/msms"
namesData[namesData=="msms"] <- "Peptide ID/min"
namesData[namesData=="mass.error"] <- "Mass error"
namesData[namesData=="score"] <- "Score"
namesData[namesData=="peak.shape"] <- "Peak Shape"
namesData[namesData=="ret.width"] <- "Peak Width"
namesData[namesData=="ret.width"] <- "Peak Width"
namesData[namesData=="quan.duplicates.msms"] <- "Peptide Duplicates"




temp.pos 	<- barplot2(unlist(score.data),beside = T,col = c(col.temp[round.spec(unlist(score.data))]),horiz = T,names.arg = namesData ,las = 2, plot.grid = T, grid.col = "grey",xlim = c(0,1.6),border = "transparent")
ColUse 		<- cbind(c(col.temp[round.spec(unlist(score.data))]),names(score.data),namesData)

#text(temp.pos,0,c("peptide ID","mass error","score","peak shape","elution time","dupl. peptide IDs"),las = 2,srt = 90,adj = c(1.1,1),xpd =NA,srt = 45)
mtext("Scores:",3,line = 0,cex = 0.6)
abline(v=1,col = "grey",lwd = 5)
abline(v=1,col = "red")

par(mai = c(1,2,0.1,0.1))

# col.vec for plot.quans 


par(mai = c(0.5,1,0.2,0.2))
init.i <- 500

plot(1,frame = F,axes=F,xlab = "",ylab = "",type = "n",ylim = c(0,init.i))
col.temp.2 <- (colorRampPalette(grad.cols.vec)(init.i))
for(col.i in 1:init.i){
	abline(h = col.i,col = col.temp.2[col.i])
}

mtext("Good",2,adj = 1,cex = 0.8)
mtext("Poor",2,adj = 0,cex = 0.8)
#mtext("Color\nCode",3,cex = 0.8)
#legend(max(temp.pos)+ temp.pos[1],max(temp.xlim),legend = c("peptide ID","mass error","score","peak shape","elution time","duplicated peptide IDs"),,fill = c(ms.col,nc.col),xpd = NA,xjust = 0,bty = "n",title = "Legend")



par(mai = c(0,0,0,0))
empty.plot <- function(){plot(1,type = "n",axes = F,xlab = "",ylab ="")}
#empty.col()
#empty.plot()

# plot real data
par(mai = c(0.2,1,0.5,0.2),bg = "lightblue")
plot.quans <- function(temp.plot,log2,xlab ="x",ylab = "y",ref.data,thresh.auto = T,bg ="lightblue",fg.col = 1 ,main = "",ylim = range(temp.plot)){
# if fg.vol is 0 
if(length(fg.col) == 0){fg.col <- 1}
	
if(log2){
	temp.plot <- log2(temp.plot)
}
temp.plot <- na.inf.fun(temp.plot)

#boxplot(temp.plot,type = "n",xlim = c(1:2),axes = F,frame = T,xlab = xlab,ylab = ylab,lwd = 5,las = 2,fg = fg.col,bty = "n",ylim = ylim, boxwex = 2)
boxplot(temp.plot,range = 0,col = "transparent",xlab = "",ylab = "",type = "n",axes = F,border = "transparent")

mtext(xlab,1,line = 1)
mtext(main,3,line = 0.3,cex = 0.7)

temp.lwd <- c(4,4,5,4,4)
temp.col <- c(4,2,1,2,4)
#for(i in 1:5){
#	lines(c(1.2, 1.8),rep(temp.plot[i],2),lwd = temp.lwd[i],col = temp.col[i])
#}

if(log2){
#abline(h=sort(c(-ref.data,ref.data,0)),col = "black", lty = "dashed",lwd = 1)	
abline(h=sort(c(-ref.data,ref.data,0)),col = temp.col,lwd = 2)	
}
if(!log2){
abline(h=sort(c(ref.data)),col = temp.col,lwd = 2)	
	
}
axis(2,las = 2,fg = 1,lwd = 2)
box(lwd = 4,fg = fg.col)
boxplot(temp.plot,range = 0,col = fg.col,xlab = xlab,ylab = ylab,type = "n",axes = F,add = T)


}

plot.stat <- function(x,thresh, name.val,rev = F,bg = "lightblue",main = "2",col.dir = NULL,xlimV = NULL){
	col.temp <- (colorRampPalette(c("blue","yellow","green"))(thresh*100))
	
		if(rev){
		col.temp <- (colorRampPalette(c("blue","yellow"))(thresh*100))
		col.temp <- rev(c(col.temp,rep("green",thresh*100)))	
		}

	x[is.na(x)] <- 0
	if(x > thresh){
		col.sel <- thresh
		x.range <- c(0,x+x*0.2)
	}else{
		col.sel <- x
		x.range <- c(0,thresh+thresh*0.2)

	}
	if(length(xlimV) == 2){
		x.range <- as.numeric(xlimV)
	}
	if(length(col.dir) == 0){col.dir <- col.temp[col.sel*100]}
	
	max(c(x,thresh))
	barplot2(x,las = 2,col =col.dir,ylab = name.val,ylim = x.range,bg = bg,angle = 45,density = 35)
	abline(h=thresh,lwd = 2,col = "grey")
	abline(h=thresh,lwd = 2,col = "red")
		box(lwd = 4,fg = col.dir)

	mtext(main,3,cex =0.7,line = 0.3)

}

#plot.stat(summary.data$msms.count,thresholds$msms.count, name.val = "MSMS counts")
##
# Peptide ID/min
##
try(plot.stat(summary.data$quan.msms.min,thresholds$quan.msms.min, name.val = "Peptide ID/min",main = "MS", col.dir = ColUse[ColUse[,2] == "msms",1]))
##
# Duplicates
##
try(plot.stat(summary.data$quan.duplicates.msms, thresholds$quan.duplicates.msms, name.val = "Duplicates/Peptide IDs in %",rev = T,main = "MS",col.dir = ColUse[ColUse[,2] == "quan.duplicates.msms",1]))

##
# Iden efficiency
##
try(plot.stat(length(data.i.quant$ms.ms.scan.number[!is.na(data.i.quant$ms.ms.scan.number)])/(max(data.i.quant$ms.ms.scan.number,na.rm = T)-min(data.i.quant$ms.ms.scan.number,na.rm = T)*0.9)*100,60,name.val = "identification efficiency in %",main = "MS",col = ColUse[ColUse[,2] == "msmsEff",1]))

par(mai = c(0.2,1,0.5,0.2),bg = "lightblue")

###
# mass error
###
try(plot.quans(summary.data$mass.error.cal,F,"","mass error in ppm",c(thresholds$mass.error.cal,-thresholds$mass.error.cal,0),fg.col = ColUse[ColUse[,2] == "mass.error",1],main = "MS"))


###
# score
###
try(plot.quans(summary.data$score,F,"","score",c(0,50,100,150,400),fg.col = ColUse[ColUse[,2] == "score",1],main = "MS")
)
##
# peak shape
##
try(plot.quans(summary.data$ret.peak.shape,T,"","log2(peak shape)",thresholds$ret.peak.shape,fg.col = ColUse[ColUse[,2] == "peak.shape",1],main = "nLC")
)
##
# peak width
## 

try(plot.quans(summary.data$ret.width,F,"","peak width in min",c(0,0.1,0.3,1,4),fg.col = ColUse[ColUse[,2] == "ret.width",1],main = "nLC",ylim = range(summary.data$ret.width[1:4]))
)

##
# ET time balance
## 

try(plot.stat(log2(data.list$sd$quanRet50ratio),thresh = c(log2(data.list$th$quanRet50ratio),-1*log2(data.list$th$quanRet50ratio)),name.val = "log2 ETime balance",main = "nLC",col = ColUse[ColUse[,2] == "quanRet50ratio",1],xlimV = c(-1,1)))

##
# ET time slope
## 

try(plot.stat((data.list$sd$quanRetSlope),thresh = c(data.list$th$quanRetSlope,-1*data.list$th$quanRetSlope),name.val = "ETime slope",main = "nLC",col = ColUse[ColUse[,2] == "quanRetSlope",1],xlimV = c(-0.05,0.05)))

##
# ET time slope
## 

try(plot.stat((data.list$sd$quanRetRSD),thresh = data.list$th$quanRetRSD,name.val = "ETime rSD",main = "nLC",col = ColUse[ColUse[,2] == "quanRetRSD",1]))


##plot.quans(summary.data$mass.error.cal,F,"mass.error","mass.error in ppm",thresholds$mass.error.cal)




if(pdfOut){
	  
	graphics.off()
}
cat("\r",getwd())

if(open.doc){
	try(        try(system(paste("open ", .pdf), intern = TRUE, 				ignore.stderr = TRUE)))
}
return(list(TotalScore = TotalScore,TotalScoreColor = col.temp[round.spec(TotalScore)]))
}
#temp <- start.qc(data)
#try(TotalScoreRes  <- plot.scores(temp.DataEvidence,qc.prepare.data,i, open.doc = T,pdfOut = pdfOut))
#system(paste("open ",list.files(pattern = ".pdf",recursive = T,full.name = T)))