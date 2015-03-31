CompareLowComplexStdFromTable <- 
function(tempListOne, TargetVec, RESettings, pdfShow = F, finalMQQC ,PDFname = "LowComplexStdComparison.pdf",main = NULL){

uniSample <- tempListOne#[grep(TargetVec, tempListOne$Name),]
test <- sub(RESettings$REmac,"", as.character(uniSample$Name))
pdfName <- paste(finalMQQC ,"TimeLines", PDFname,sep = "/")
pdf(pdfName ,width = 14,pointsize = 15)

UniMachine  <- grepRE(as.character(uniSample$Name),RESettings$REmac)
layout(matrix(c(1,1,1,2,3,4,5,6,7),3,3,byrow = T),height = c(0.2,1,1))
par(mai = rep(0,4))
empty.plot()
if(length(main ) == 0){
	main <- paste("MQQC Parameter Comparisons, Low Complex Standard (",TargetVec,")",sep = "")
}
mtext(main,line = -2)

par(mai = c(0.5,0.5,0.2,0.1))

xColumn <- "msmsQuantile.50."
yColumn <- "score.50."
PlotTwoFun <- function(tempListOne,xColumn,yColumn,xlab = xColumn,ylab = yColumn,legPos = "bottomright", plotpoints = T, axesplot = T){
a <- tempListOne[xColumn]
b <- tempListOne[yColumn]

Exclude <- is.na(a)|is.na(b)|a == 0|b == 0
plot(a[! Exclude],b[!Exclude],pch = 20,type = "n", xlab = xlab,ylab = ylab,frame = T,mgp = c(1.5,0.5,0),axes = axesplot)
rainbowCol <- rainbow(length(unique(UniMachine)),alpha = 0.4,v = 0.8) 
rainbowColFull <- rainbow(length(unique(UniMachine)),alpha = 1,v = 0.8) 
grid()
List <- list()
for(Mac in 1:length(unique(UniMachine)) ){

tempListOneMac <- tempListOne[UniMachine ==unique(UniMachine)[Mac],]
a <- tempListOneMac[xColumn]
b <- tempListOneMac[yColumn]
a <- as.numeric(unlist(a))
b <- as.numeric(unlist(b))
Exclude <- is.na(a)|is.na(b)|a == 0|b == 0
if(plotpoints){
points(a[! Exclude],b[!Exclude],pch = 21,bg = rainbowCol[Mac],col = "transparent")
}
a <<- a
b <<- b
Exclude <<- Exclude
try(List[[Mac]] <- lowess(a[! Exclude],b[! Exclude]))
#points(,type = "l",col = rainbowColFull[Mac],lwd = 3,lty = Mac)
#points(Estimate <- lowess(b[! Exclude]~a[! Exclude]),type = "l",col = rainbowColFull[Mac],lwd = 3,lty = Mac)


}
List <<- List
ittemp <<- 1

sapply(List,function(x){
	temp <- rainbowColFull[ittemp]
	temp <<- temp
	temp <- paste(substr(temp ,1,7),"60",sep = "")
	points(x$x,x$y,type = "l",col = "#FFFFFF80",lwd = 4)
	points(x$x,x$y,type = "l",col =temp ,lwd = 3.3,lty = 1)	
	points(x$x,x$y,type = "l",col = rainbowColFull[ittemp],lwd = 3,lty = ittemp)
	ittemp <<- ittemp+1 

})

legend(legPos,legend = unique(UniMachine), col = rainbowColFull,lty = 1:Mac,lwd = 2,fill  = rainbowColFull,border = "transparent",box.col = "transparent",bg = "#FFFFFF80")
}

# Score Dependecne
#PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","score.50.","MS Intensity","MQ Score")
PlotTwoFun(tempListOne = tempListOne,"msmsQuantile.50.","Coverage","Median MSMS Intensity","Coverage in %")
PlotTwoFun(tempListOne = tempListOne,"msmsMassCount.50.","Coverage","Median MSMS Fragment Counts","Coverage in %")
PlotTwoFun(tempListOne = tempListOne,"Coverage","quan.msms.min","Median Coverage","Median MSMS/min")
PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsQuantile.50.","MSMS Median Intensity","MS Median Intensity")
PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsMassCount.50.","MS Median Intensity","MSMS Median Fragment Counts")
PlotTwoFun(tempListOne = tempListOne,"System.Time.s","Coverage","Date","Coverage in %", axesplot = F)
axis(2,mgp = c(1.5,0.5,0))
hu <- axTicks(1)
time <- as.POSIXct(hu,origin = "1970-01-01",tz = "GMT")
time <- sapply(strsplit(as.character(time)," "),function(x){x[1]})
axis(1,hu,labels =time,mgp = c(1.5,0.5,0))
graphics.off()
if(pdfShow){
	system(paste("open", pdfName))
}
}

#CompareLowStdFromTable(tempListOne,StandardIDs[1],RESettings,T,finalMQQC)
# # #PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","score.50.")
# #PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsQuantile.50.")
# #PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsMassCount.50.")
# #PlotTwoFun(tempListOne = tempListOne,"ret.width.50.","uniPepCount")
# PlotTwoFun(tempListOne = tempListOne,"total.msms.min","msms.count")

# # # y <- b[!Exclude]
# # x <- a[!Exclude]

 # te <- nls(y~x^b+c,start = list(b =0.5,c = 0), algorithm = "plinear")

 # points(x,predict(te))