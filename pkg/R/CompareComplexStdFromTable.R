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

# Score Dependecne
#PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","score.50.","MS Intensity","MQ Score")

if(TargetVec == StandardIDs[2]&TargetVec!= ""){
  try(PlotTwoFun(tempListOne = tempListOne,"msmsQuantile.50.","score.50.","MSMS Median Intensity","Andromeda Median Score",logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
   try(PlotTwoFun(tempListOne = tempListOne,"msmsMassCount.50.","score.50.","MSMS Median Fragment Counts","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))

  #dev.off()
  #system(paste("open", pdfName))
  
  #stop()
  try( PlotTwoFun(tempListOne = tempListOne,"Coverage","score.50.","Coverage [%]","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsQuantile.50.","MS Median Intensity","MSMS Median Intensity", logPlot = "xy",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsMassCount.50.","MS Median Intensity","MSMS Median Fragment Counts", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
#  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"System.Time.s","Coverage","Date","Coverage [%]", axesplot = F,leg = F))
  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","Coverage","Uncalibrated mass error [ppm]","Coverage [%]", axesplot = T,leg = F,logPlot = "n",shiftPlot = T,UniMachine = UniMachine, Machines = Machines))

}else{
  #print(UniMachine)
  try(PlotTwoFun(tempListOne = tempListOne,"msmsQuantile.50.","score.50.","MSMS Median Intensity","Andromeda Median Score",logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))  
  try(PlotTwoFun(tempListOne = tempListOne,"msmsMassCount.50.","score.50.","MSMS Median Fragment Counts","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
  try(PlotTwoFun(tempListOne = tempListOne,"quan.msms.min","score.50.","MSMS/min","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsQuantile.50.","MS Median Intensity","MSMS Median Intensity", logPlot = "xy",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsMassCount.50.","MS Median Intensity","MSMS Median Fragment Counts", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))
#LegFun <- PlotTwoFun(tempListOne = tempListOne,"System.Time.s","quan.msms.min","Date","MSMS/min", axesplot = F,leg = F)
  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","quan.msms.min","Uncalibrated mass error [ppm]","MSMS/min", axesplot = T,leg = F,logPlot = "n",shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF))


}





if(exists("LegFun")){
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
}
graphics.off()
if(pdfShow){
	system(paste("open", pdfName))
}


DensMatrixTemplate <<- rbind(
  #MS
  c("quan.msms.min","IDs/min",F,"MS"),
  c("Intensity.50.","log10 MS Median Intensity",T,"MS"),
  c("mass.error.uncal.50.","Mass Error in [ppm]",F,"MS"),
  c("Coverage","Coverage",F,"MS"),
  #MSMS
  c("msmsQuantile.50.","log10 MSMS Median Intensity",T,"MSMS"),
  c("msmsMassCount.50.","MSMS Median Fragment Counts",F,"MSMS"),
  c("msmsEff","identified MSMS",F,"MSMS"),
  #nlc
  c("LCcombiScore","LC profile symmetry",F,"LC"),
  c("ret.width.50.","Retention Time [min]",F,"LC"),
  c("ret.peak.shape.50.","log10 Peak shape",T,"LC"))


if(TargetVec == StandardIDs[2]&TargetVec!= ""){
  
}else{
  DensMatrixTemplate <- DensMatrixTemplate[grep("Coverage",DensMatrixTemplate[,1],invert = T),] 
}

graphics.off()

pdfName <- paste(finalMQQC ,"TimeLines", paste("DensityEstimates",PDFname,sep = "_"),sep = "/")

if(PDF){
  pdf(pdfName ,width = 12,height = 0.5*dim(DensMatrixTemplate)[1])
  par(mai = c(0.3,0.5,0.2,0.2),las = 1)
  
}else{
  scaleFactor <- 1.2
  
  jpeg(pdfName,width = 1000,height = 50*dim(DensMatrixTemplate)[1], quality = 90,pointsize = 12)	
  par(mai = c(0.3,0.5,0.2,0.2),las = 1)
  
}



#pdf(pdfname<- "MQQCDensityPlots.pdf",)
vmax <- max(table(DensMatrixTemplate[,4]))
colmax <- length(unique(DensMatrixTemplate[,4]))
LayoutM <- matrix(1:(vmax*colmax),vmax,colmax)
LayoutM <- cbind(LayoutM,LegPlot<- max(LayoutM)+1)
layout(LayoutM,width = c(rep(1,colmax),0.5))

LegFun <<- LegFun
for(i in unique(DensMatrixTemplate[,4])){
  tempDensMatrix <-   DensMatrixTemplate[DensMatrixTemplate[,4]== i,]
  for(a in 1:vmax){
  if(a > dim(tempDensMatrix)[1]){
    empty.plot()
  }else{  
  tempI <- tempListOne[,colnames(tempListOne) ==  tempDensMatrix[a,1]]
  
  if(length(tempI) > 0){
  if(tempDensMatrix[a,3] == "TRUE"){
    tempI <- log10(tempI)
  }
  tempM <<- aggregate(tempI,list(UniMachine),median,na.rm = T)
  temp <<- aggregate(tempI,list(UniMachine),function(x){
    
      tempi <- class(try(tempDens <- density(x,na.rm = T)))
      if(tempi!="try-error"){
        tempDens$y <- tempDens$y /max(tempDens$y,na.rm = T)
      }else{tempDens <- NA}
    
      return(tempDens)})
  OrderFun <- merge.control(temp[[1]],LegFun$Mac)
  xlim <- range(tempI[!is.infinite(tempI)],na.rm = T)
  if(!any(c(is.na(xlim),is.infinite(xlim)))){
  plot(1,type = "n",ylim = c(0,1),xlim =xlim ,xlab = "",main  = tempDensMatrix[a,2],ylab = "",frame = F)
  }else{
    plot(1,type = "n",ylim = c(0,1) ,xlab = "",main  = tempDensMatrix[a,2],ylab = "",frame = F)
    
  }
  itCompare <<- 0
  sapply(OrderFun,function(x){
    itCompare <<- itCompare+1
    trye <- class(try(points(temp[[2]][x,],col = LegFun$col[itCompare],lty = 1,type = "l",lwd = LegFun$lwd),silent = T))
    if(trye!="try-error"){
    hui <- temp[[2]][x,]
    xl<- median(tempM[x,2],na.rm = T)
    #abline(v = xl<- median(tempM[x,2],na.rm = T),col = LegFun$col[x],lty = "dotted")
    subvec <- abs(xl -hui$x) 
    yl <- hui$y[subvec == min(subvec,na.rm = T)][1]
    lines(c(xl,xl),c(0,yl),col = LegFun$col[itCompare],lty = "dotted")
    axis(1,at = xl,col = LegFun$col[itCompare],label = "")
    }
  })
  
  

  
}else{
  plot(1,type = "n",ylim = c(0,1),xlab = "",main  = tempDensMatrix[a,2],ylab = "",frame = F)
}
}
}
}
par(mai = rep(0,4),mar = rep(0,4))
empty.plot()
legend("left"
       ,legend = as.character(LegFun$Mac)	
       ,col = as.character(LegFun$col)
       ,lwd = LegFun$lwd,
       ,border = "transparent",box.col = "transparent",bg = "#FFFFFF80",xpd = NA
)

dev.off()
#system(paste("open",pdfName))
}
#try(CompareComplexStdFromTable(collectList[BSA,],RESettings,T,finalMQQC, PDFname = "LowComplexStandardComparison.pdf", TargetVec = StandardIDs[2],PDF = T, Machines = Machines))

#try(CompareComplexStdFromTable(tempListOne = collectList[,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "ECstd",PDF = T, Machines = Machines,StandardIDs = StandardIDs))

#try(CompareComplexStdFromTable(tempListOne = collectList,RESettings = RESettings,pdfShow = T,finalMQQC = htmloutPath, PDFname = "ComplexStandardComparison.pdf",PDF = T))



