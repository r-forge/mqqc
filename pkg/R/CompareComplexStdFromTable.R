CompareComplexStdFromTable <- 
function(tempListOne, RESettings, pdfShow = F, finalMQQC , PDFname = "ComplexStdComparison.pdf",main = NULL, logPlot = "n", TargetVec = "ECstd",PDF = T,Machines = NULL,StandardIDs = c("ECstd","BSA")){

#TargetVec <- StandardIDs[1]


#uniSample <- tempListOne#[grep(TargetVec, tempListOne$Name),]
test <- sub(RESettings$REmac,"", as.character(tempListOne$Name))


UniMachine  <- grepRE(as.character(tempListOne$Name),RESettings$REmac)

if(length(Machines) > 0){
	test <<- test
	MachineVec <- grep(paste(Machines,collapse = "|"), UniMachine)
	if(length(MachineVec) > 0){
		tempListOne <- tempListOne[MachineVec,]
		UniMachine <- UniMachine[MachineVec]
	}
}


pdfName <- paste(finalMQQC ,"TimeLines", PDFname,sep = "/")

if(PDF){
pdf(pdfName ,width = 14,pointsize = 15)
}else{
scaleFactor <- 1.7	
	
jpeg(pdfName,width = 140*15/scaleFactor,height = 70*15/scaleFactor, quality = 90,pointsize = 10/(scaleFactor))	
}

M <- matrix(c(1,1,1,2,3,4,5,6,7),3,3,byrow = T)
M <- cbind(M,rep(max(M)+1,dim(M)[1]))
layout(M,height = c(0.1,1,1),width = c(1,1,1,0.4))
par(mai = rep(0,4),lwd = 2)
empty.plot()
if(length(main ) == 0){
	main <- paste("MQQC Parameter Comparisons, Complex Standard (",TargetVec,")",sep = "")
}
if(PDF){
mtext(main,line = -2,cex = 1.5)
	
par(mai = c(0.5,0.5,0.2,0.1))
lwdjpg <- 1
}else{
mtext(main,line = -7,cex = 6)
	
par(mai = c(0.7,0.7,0.2,0.1),lwd = 3,cex =3)
	lwdjpg <- 2/scaleFactor
}
PlotTwoFun <- function(tempListOne,xColumn,yColumn,xlab = xColumn,ylab = yColumn,legPos = "bottomright", plotpoints = T, axesplot = T, logPlot = "n",leg = T,shiftPlot = F,ShiftVal = c(0.14,0.1)){
a <- as.numeric(unlist(tempListOne[xColumn]))
b <- as.numeric( unlist(tempListOne[yColumn]))

if(logPlot == "y"){
	b <- log10(b)
	ylab <- paste("log10",ylab)
	
}
if(logPlot == "x"){
	a <- log10(a)
	xlab <- paste("log10",xlab)
}
if(logPlot == "xy"| logPlot == "yx"){
	a <- log10(a)
	b <- log10(b)
	xlab <- paste("log10",xlab)
	ylab <- paste("log10",ylab)

}
ai <- a
bi <- b

Exclude <- is.na(a)|is.na(b)|a == 0|b == 0

yrange <- range(b[!Exclude],na.rm = T)
xrange <- range(a[!Exclude],na.rm = T)
pranx <- pretty(xrange)
prany <- pretty(yrange)

if(shiftPlot){
  shifty <- abs(diff(yrange))/(1-ShiftVal[1])
  #if(yrange[1]>= 0& yrange[2]> 0){ shifty <- abs(yrange[2])/(1-ShiftVal)}
  #if(yrange[1]< 0& yrange[2]<= 0){ shifty <- abs(yrange[1])/(1-ShiftVal)}
  yline <- yrange[1]
  
  yrange[1] <- yrange[2]-abs(shifty)
  axesplot <- F
  
  shiftx <- abs(diff(xrange))/(1-ShiftVal[2])
  xline <- xrange[1]
  #if(xrange[1]>= 0& xrange[2]>  0){ shiftx <- abs(xrange[2])/(1-ShiftVal)}
  #if(xrange[1]<  0& xrange[2]<= 0){ shiftx <- abs(xrange[1])/(1-ShiftVal)}
  
  xrange[1] <- xrange[2]-abs(shiftx)
  axesplot <- F
}

plot(a[! Exclude],b[!Exclude],pch = 20,type = "n", xlab = xlab,ylab = ylab,frame = F,mgp = c(1.5,0.5,0),axes = axesplot,ylim = yrange,xlim=xrange)
#grid()

if(shiftPlot){
  axis(1,at = pranx[pranx > xline],mgp = c(1.5,0.5,0))
  axis(2,at = prany[prany > yline],mgp = c(1.5,0.5,0))
  #abline(h=yline,v=xline)
  lines( c(xline,xrange[2]*2),rep(yline,2),col = 1)
  lines( rep(xline,2),c(yline,yrange[2]*2),col = 1)
  abline(v = pranx[pranx > xline],col = "grey",lwd = 0.5,lty = "dotted")
  abline(h = prany[prany > yline],col = "grey",lwd = 0.5,lty = "dotted")
  
}

rainbowCol <- rainbow(length(unique(UniMachine)),alpha = 0.4,v = 0.8) 
rainbowColFull <- rainbow(length(unique(UniMachine)),alpha = 1,v = 0.8) 
box()
List <- list()
Listcurrent <- list()

xPoints <- c()
yPoints <- c()
colPoints <- c()

UniMachines <- unique(UniMachine)
UniMachines <- UniMachines[merge.control(UniMachines,Machines)]
UniMachines  <- UniMachines[!is.na(UniMachines)]
for(Mac in 1:length(UniMachines) ){

tempListOneMac <- tempListOne[UniMachine == UniMachines[Mac],]
Sel <- tempListOneMac$System.Time.s == max(tempListOneMac$System.Time.s)

a <- tempListOneMac[xColumn]
b <- tempListOneMac[yColumn]
a <- as.numeric(unlist(a))
b <- as.numeric(unlist(b))
if(logPlot == "y"){
	b <- log10(b)
}
if(logPlot == "x"){
	a <- log10(a)
}
if(logPlot == "xy"| logPlot == "yx"){
	a <- log10(a)
	b <- log10(b)
}

Exclude <- is.na(a)|is.na(b)|a == 0|b == 0
if(plotpoints){
#points(a[! Exclude],b[!Exclude],pch = 21,bg = rainbowCol[Mac],col = "transparent")
}
a <<- a
b <<- b
Exclude <<- Exclude
 error <- try(temp <- lowess(a[! Exclude],b[! Exclude]))
if(class(error) == "try-error"){temp <- list(x = 0,y = 0)}
TempSel <- tempListOneMac$SourceTime[!Exclude]
TempSel  <- TempSel == max(TempSel,na.rm = T)
TempSel[is.na(TempSel)] <- F
Listcurrent[[Mac]] <- list(x = a[! Exclude][TempSel], y = b[! Exclude][TempSel])

try(List[[Mac]] <- temp)

xPoints <- c(xPoints,a[!Exclude])
yPoints <- c(yPoints,b[! Exclude])
colPoints <- c(colPoints,rep(rainbowCol[Mac],length(b[! Exclude])))

#points(,type = "l",col = rainbowColFull[Mac],lwd = 3,lty = Mac)
#points(Estimate <- lowess(b[! Exclude]~a[! Exclude]),type = "l",col = rainbowColFull[Mac],lwd = 3,lty = Mac)
}
SampleVec <- sample(1:length(xPoints))
points(xPoints[SampleVec], yPoints[SampleVec],pch = 21,bg = colPoints[SampleVec],col = "transparent")



List <<- List
ittemp <<- 1

sapply(List,function(x){
	temp <- rainbowColFull[ittemp]
	temp <<- temp
	temp <- paste(substr(temp ,1,7),"60",sep = "")
	points(x$x,x$y,type = "l",col = "#FFFFFF80",lwd = 4* lwdjpg)
	
	points(x$x,x$y,type = "l",col =temp ,lwd = 3.3* lwdjpg,lty = 1)	
	points(x$x,x$y,type = "l",col = rainbowColFull[ittemp],lwd = 3* lwdjpg,lty = ittemp)
	ittemp <<- ittemp+1 
}
)
ittemp <<- 1
sapply(Listcurrent,function(x){
Sel[is.na(Sel)] <- F

points(x$x,x$y,bg = "white",col = "white",pch = 23,cex = 1.85,lwd = 0.5)
points(x$x,x$y,col = rainbowColFull[ittemp],pch = ittemp,cex = 1,lwd = 1.5)

#points(x$x[Sel],x$y[Sel],bg = rainbowColFull[ittemp],col = rainbowCol[Mac],pch = 21)
#text(x$x[Sel],b[Sel],unique(UniMachine)[ittemp])
ittemp <<- ittemp+1 

}

)




if(leg){
	
legend(legPos,legend = UniMachines, col = rainbowColFull,lty = 1:Mac,lwd = 1.5*lwdjpg,border = "transparent",box.col = "transparent",bg = "#FFFFFF80",pch =1 : Mac ,pt.cex = 1)


	
	
}
UM <- UniMachine[!Exclude]

if(shiftPlot){
  # density plot x
  shifti <- ShiftVal[1]#abs(yrange[1])/sum(abs(yrange))
  par(new = T)
  inputShift <<- ai[!Exclude]
  tempListOne <<- tempListOne
  #try(DensVec <<- density(hua <-inputShift,na.rm = T))
  #DensVec$y <- DensVec$y/max(DensVec$y)*shifti
  xir <<- xrange
  try(plot(1,col = "#44444460",xlab = "",ylab = "",main = "",axes = F,xlim =  xrange,ylim = c(0,1),type = "n"))
  for(m in UniMachines){
    dm <- density(hua <-inputShift[UM == m],na.rm = T)
    dm$y <- dm$y/max(dm$y)*shifti*0.8
    Col <- rainbowColFull[UniMachines == m]
    Col <- gsub("FF$","FF",Col)
    dm$y[dm$x < xline] <- NA
    try(points(dm,col = Col,type = "l",lty = (1:Mac)[UniMachines == m],lwd = 2))
    
  }
  if(PDF){
    axis(4,at = c(0,shifti*0.8),labels = c("",""),col = "grey")
    
    mtext("     density",4,adj = 0,line = 0.1,cex = 0.4,las = 0,col = "grey")
    
  }else{
    #mtext("   density",4,adj = 0,line = 0,cex = 2.5,las = 0,col = "grey")
  }
  
  
  #axis(2,mgp = c(1.5,0.5,0))
  hu <- axTicks(1)
  time <- as.POSIXct(hu,origin = "1970-01-01",tz = "GMT")
  time <- sapply(strsplit(as.character(time)," "),function(x){x[1]})
  #axis(1,hu,labels =time,mgp = c(1.5,0.5,0))
  # density plot y
  
  shifti <- ShiftVal[2]#abs(xrange[1])/sum(abs(xrange))
  
  par(new = T)
  inputShift <- bi[!Exclude]
  #try(DensVec <- density(hua <-inputShift,na.rm = T))
  #DensVec$y <- DensVec$y/max(DensVec$y)*shifti
  try(plot(1,col = "#44444460",xlab = "",ylab = "",main = "",axes = F,ylim =  yrange,xlim = c(0,1),type = "n"))
  for(m in UniMachines){
    dm <- density(hua <-inputShift[UM == m],na.rm = T)
    dm$y <- dm$y/max(dm$y)*shifti*0.8
    Col <- rainbowColFull[UniMachines == m]
    Col <- gsub("FF$","FF",Col)
    dm$y[dm$x < yline] <- NA
    try(points(dm$y,dm$x,col = Col,type = "l",lty = (1:Mac)[UniMachines == m],lwd = 2))
    
  }
  axis(3,at = c(0,shifti*0.8),labels = c("",""),col = "grey")
  if(PDF){
    mtext("       density",3,adj = 0,line = 0.1,cex = 0.4,las = 0,col = "grey")
  }else{
    #mtext("   density",4,adj = 0,line = 0,cex = 2.5,las = 0,col = "grey")
  }
  
  
  #axis(2,mgp = c(1.5,0.5,0))
  hu <- axTicks(1)
  time <- as.POSIXct(hu,origin = "1970-01-01",tz = "GMT")
  time <- sapply(strsplit(as.character(time)," "),function(x){x[1]})
  #axis(1,hu,labels =time,mgp = c(1.5,0.5,0))
}

return(list(Mac = UniMachines,col = rainbowColFull,lty = 1:Mac,lwd = 1.5*lwdjpg,pch =1 : Mac))
}
# Score Dependecne
#PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","score.50.","MS Intensity","MQ Score")
if(TargetVec == StandardIDs[2]){
  try(PlotTwoFun(tempListOne = tempListOne,"msmsQuantile.50.","score.50.","MSMS Median Intensity","Andromeda Median Score",logPlot = "x",leg = F,shiftPlot = T))
  try(PlotTwoFun(tempListOne = tempListOne,"msmsMassCount.50.","score.50.","MSMS Median Fragment Counts","Andromeda Median Score",leg = F,shiftPlot = T))

  #dev.off()
  #system(paste("open", pdfName))
  
  #stop()
  try( PlotTwoFun(tempListOne = tempListOne,"Coverage","score.50.","Coverage [%]","Andromeda Median Score",leg = F,shiftPlot = T))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsQuantile.50.","MS Median Intensity","MSMS Median Intensity", logPlot = "xy",leg = F,shiftPlot = T))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsMassCount.50.","MS Median Intensity","MSMS Median Fragment Counts", logPlot = "x",leg = F,shiftPlot = T))
#  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"System.Time.s","Coverage","Date","Coverage [%]", axesplot = F,leg = F))
  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","Coverage","Uncalibrated mass error [ppm]","Coverage [%]", axesplot = T,leg = F,logPlot = "n",shiftPlot = T))

}else{
  try(PlotTwoFun(tempListOne = tempListOne,"msmsQuantile.50.","score.50.","MSMS Median Intensity","Andromeda Median Score",logPlot = "x",leg = F,shiftPlot = T))
  try(PlotTwoFun(tempListOne = tempListOne,"msmsMassCount.50.","score.50.","MSMS Median Fragment Counts","Andromeda Median Score",leg = F,shiftPlot = T))
  try(PlotTwoFun(tempListOne = tempListOne,"quan.msms.min","score.50.","MSMS/min","Andromeda Median Score",leg = F,shiftPlot = T))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsQuantile.50.","MS Median Intensity","MSMS Median Intensity", logPlot = "xy",leg = F,shiftPlot = T))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsMassCount.50.","MS Median Intensity","MSMS Median Fragment Counts", logPlot = "x",leg = F,shiftPlot = T))
#LegFun <- PlotTwoFun(tempListOne = tempListOne,"System.Time.s","quan.msms.min","Date","MSMS/min", axesplot = F,leg = F)
  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","quan.msms.min","Uncalibrated mass error [ppm]","MSMS/min", axesplot = T,leg = F,logPlot = "n",shiftPlot = T))
 
}

par(mai = rep(0,4),mar = rep(0,4))
empty.plot()

legend("left"
		,legend = as.character(LegFun$Mac)	
		,col = as.character(LegFun$col)
		,lwd = LegFun$lwd,
		,lty = LegFun$lty
		,pch = LegFun$pch,
		,border = "transparent",box.col = "transparent",bg = "#FFFFFF80",xpd = NA
		)

graphics.off()
if(pdfShow){
	system(paste("open", pdfName))
}
}
#try(CompareComplexStdFromTable(tempListOne = collectList[,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "ECstd",PDF = T, Machines = Machines,StandardIDs = StandardIDs))

#try(CompareComplexStdFromTable(tempListOne = collectList[BSA,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "BSA",PDF = T, Machines = Machines))
#try(CompareComplexStdFromTable(collectList[BSA,],RESettings,T,finalMQQC, PDFname = "LowComplexStandardComparison.jpg", TargetVec = "BSA",PDF = F, Machines = Machines))

