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
}
#try(CompareComplexStdFromTable(tempListOne = collectList[ECstd,],RESettings = RESettings,pdfShow = F,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "ECstd",PDF = T, Machines = Machines,StandardIDs = StandardIDs))

#try(CompareComplexStdFromTable(tempListOne = collectList[,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "ECstd",PDF = T, Machines = Machines,StandardIDs = StandardIDs))

#try(CompareComplexStdFromTable(tempListOne = collectList[BSA,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "BSA",PDF = T, Machines = Machines))
#try(CompareComplexStdFromTable(collectList[BSA,],RESettings,T,finalMQQC, PDFname = "LowComplexStandardComparison.jpg", TargetVec = "BSA",PDF = F, Machines = Machines))

