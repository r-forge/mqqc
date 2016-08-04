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
pdf(pdfName ,width = 14,height = 11,pointsize = 15)
}else{
scaleFactor <- 1.7	
	
jpeg(pdfName,width = 140*15/scaleFactor,height = 70*15/scaleFactor, quality = 90,pointsize = 10/(scaleFactor))	
}

M <<- matrix(c(1,1,1,2,3,4,5,6,7,8,9,10),4,3,byrow = T)
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
  try(PlotTwoFun(tempListOne = tempListOne,"MSID.min","quan.msms.min","Isotopic Patterns [1/min]","", logPlot = "xy",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","quan.msms.min","MS Median Intensity","", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","quan.msms.min","uncalibrated mass error ppm","Andromeda Median Score",logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)  
  try(PlotTwoFun(tempListOne = tempListOne,"msmsQuantile.50.","quan.msms.min","MSMS Median Fragment Counts","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"msmsMassCount.50.","quan.msms.min","MSMS/min","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"LCcombiScore","quan.msms.min","MSMS/min","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"ret.width.50.","quan.msms.min","MSMS/min","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"ret.peak.shape.50.","quan.msms.min","MS Median Intensity","MSMS Median Fragment Counts", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  #LegFun <- PlotTwoFun(tempListOne = tempListOne,"System.Time.s","quan.msms.min","Date","MSMS/min", axesplot = F,leg = F)
  # try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","quan.msms.min","Uncalibrated mass error [ppm]","MSMS/min", axesplot = T,leg = F,logPlot = "n",shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  
  
  try( PlotTwoFun(tempListOne = tempListOne,"Coverage","score.50.","Coverage [%]","Andromeda Median Score",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
#   try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsQuantile.50.","MS Median Intensity","MSMS Median Intensity", logPlot = "xy",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
#   try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","msmsMassCount.50.","MS Median Intensity","MSMS Median Fragment Counts", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
#   #  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"System.Time.s","Coverage","Date","Coverage [%]", axesplot = F,leg = F))
#   # try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","Coverage","Uncalibrated mass error [ppm]","Coverage [%]", axesplot = T,leg = F,logPlot = "n",shiftPlot = T,UniMachine = UniMachine, Machines = Machines))
#   try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"MSID.min","Coverage","Features [1/min]","Coverage [%]", axesplot = T,leg = F,logPlot = "n",shiftPlot = T,UniMachine = UniMachine, Machines = Machines))
#   
   
}else{
  # MSID.min
  try(PlotTwoFun(tempListOne = tempListOne,"MSID.min","quan.msms.min","MS Isotopic Patterns [1/min]","PSMs [1/min]", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"Intensity.50.","quan.msms.min","MS Median Intensity","PSMs [1/min]", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","quan.msms.min","MSMS uncalibrated mass error ppm","PSMs [1/min]",logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)  
  try(PlotTwoFun(tempListOne = tempListOne,"msmsQuantile.50.","quan.msms.min","MSMS Median Fragment Intensity","PSMs [1/min]",logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"msmsMassCount.50.","quan.msms.min","MSMS Median Fragment Counts","PSMs [1/min]",logPlot = "",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"LCcombiScore","quan.msms.min","LC Score","PSMs [1/min]",logPlot = "",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(PlotTwoFun(tempListOne = tempListOne,"ret.width.50.","quan.msms.min","LC Peak width","PSMs [1/min]",logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"ret.peak.shape.50.","quan.msms.min","LC peak shape","PSMs [1/min]", logPlot = "x",leg = F,shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
#LegFun <- PlotTwoFun(tempListOne = tempListOne,"System.Time.s","quan.msms.min","Date","MSMS/min", axesplot = F,leg = F)
  # try(LegFun <- PlotTwoFun(tempListOne = tempListOne,"mass.error.uncal.50.","quan.msms.min","Uncalibrated mass error [ppm]","MSMS/min", axesplot = T,leg = F,logPlot = "n",shiftPlot = T,UniMachine = UniMachine, Machines = Machines,lwdjpg = lwdjpg,PDF =PDF),silent = T)
  try(empty.plot())
  

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


DensMatrixTemplate <- rbind(
  #MS
  c("Isotope.patterns.min","Features/min",F,"MS"),
  c("Intensity.50.","log10 MS Median Intensity",T,"MS"),
  c("mass.error.uncal.50.","Mass Error in [ppm]",F,"MS"),
  c("Coverage","Coverage",F,"MS"),
  #MSMS
  c("quan.msms.min","IDs/min",F,"MSMS"),
  c("msmsQuantile.50.","log10 MSMS Median Intensity",T,"MSMS"),
  c("msmsMassCount.50.","MSMS Median Fragment Counts",F,"MSMS"),
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
layout(LayoutM,width = c(rep(1,colmax),0.8))

LegFun <- LegFun
PointsListQuantiles <- list()
itMain = 0
naQuan <- c()
for(i in unique(DensMatrixTemplate[,4])){
  tempDensMatrix <-   DensMatrixTemplate[DensMatrixTemplate[,4]== i,]
  for(a in 1:vmax){
    itMain = itMain +1
    
    if(a > dim(tempDensMatrix)[1]){
      empty.plot()
    }else{  
      
      tempI <- tempListOne[,tolower(colnames(tempListOne)) ==  tolower(tempDensMatrix[a,1])]
      
      
      
      #       if(all(is.na(Current))){
      #       }
      tempDensMatrix <- tempDensMatrix
      if(length(tempI) > 0){
        if(tempDensMatrix[a,3] == "TRUE"){
          tempI <- log10(tempI)
        }
        
        CurrentL <- lapply(unique(UniMachine),function(x){
          sel <- x == UniMachine
          tempM <<- tempListOne[sel,]
          na <- tempListOne$Name[tempM$System.Time.s == max(tempM$System.Time.s,na.rm = T)]
          Current <- tempI[sel][tempM$System.Time.s == max(tempM$System.Time.s,na.rm = T)]
          return(c(Current,na))
        })
        names(CurrentL) <- unique(UniMachine)
        tempM <- aggregate(tempI,list(UniMachine),median,na.rm = T)
        temp <- aggregate(tempI,list(UniMachine),function(x){
          tempDens <- list(x = 0,y = 0)
          tempi <- class(try(tempDens <- density(x,na.rm = T),silent = T))
          tempDens$Quantile <- quantile(x,prob = c(0.05,0.25,0.5,0.75,0.95),na.rm = T)
          if(tempi!="try-error"){
            tempDens$y <- tempDens$y /max(tempDens$y,na.rm = T)
          }
          return(tempDens)})
        
        OrderFun <- merge.control(temp$Group.1,LegFun$Mac)
        xlim <- range(tempI[!is.infinite(tempI)],na.rm = T)
        if(!any(c(is.na(xlim),is.infinite(xlim)))){
          plot(1,type = "n",ylim = c(0,1),xlim =xlim ,xlab = "",main  = tempDensMatrix[a,2],ylab = "",frame = F)
        }else{
          plot(1,type = "n",ylim = c(0,1) ,xlab = "",main  = tempDensMatrix[a,2],ylab = "",frame = F)
          
        }
        itCompare <<- 0
        # pointsList <<- list()
        pointsList <- sapply(OrderFun,function(x){
          itCompare <<- itCompare+1
          if(is.matrix(temp$x)){
            subDens <- temp$x[x,]
          }
          if(exists("subDens")){
            try(subDens$y <- subDens$y /max(subDens$y ))
            trye <- class(try(points(subDens,col = LegFun$col[itCompare],lty = 1,type = "l",lwd = LegFun$lwd),silent = T))
            Current <<- CurrentL[[itCompare]]
            
            CurrentDiff <- abs(subDens$x-as.numeric(Current[1]))
            Currenty <- subDens$y[CurrentDiff == min(CurrentDiff,na.rm = T)]
            Currenty <- Currenty[!is.na(Currenty)]
            if(length(Currenty)==0){Currenty <- NA}
            pointsList<- c(as.numeric(Current[1]),Currenty,col = LegFun$col[itCompare],pch = LegFun$pch[itCompare],subDens$Quantile)
            # try(points())
            
            if(trye!="try-error"){
              try(xl<- median(tempM[x,2],na.rm = T),silent = T)
              #abline(v = xl<- median(tempM[x,2],na.rm = T),col = LegFun$col[x],lty = "dotted")
              try(subvec <- abs(xl -subDens$x) ,silent = T)
              try(yl <- subDens$y[subvec == min(subvec,na.rm = T)][1],silent = T)
              try(lines(c(xl,xl),c(0,yl),col = LegFun$col[itCompare],lty = "dotted"),silent =T)
              try(axis(1,at = xl,col = LegFun$col[itCompare],label = ""),silent = T)
            }
          }else{            pointsList<- c(Current,rep(NA,9))
          }
          return(pointsList)
        })
        apply(pointsList,2,function(x){
          try(points(x[1],x[2],bg = "white",col = "white",pch = 23,cex = 1.85,lwd = 0.5),silent = T)
          return(NULL)
        })
        apply(pointsList,2,function(x){
          try(points(x[1],x[2],col = x[3],pch = as.numeric(x[4]),cex = 1,xpd = NA),silent = F)
          return(NULL)
        })
        pointsList <- pointsList[grep("%",rownames(pointsList)),]
        try(colnames(pointsList) <- LegFun$Mac,silent = T) # check if naming is correct
        
        PointsListQuantiles[[itMain]] <- pointsList
      }else{
        plot(1,type = "n",ylim = c(0,1),xlab = "",main  = tempDensMatrix[a,2],ylab = "",frame = F)
      }
      
    }
  }
  naQuan <- c(naQuan,tempDensMatrix[,1])
}

names(PointsListQuantiles)<- naQuan



par(mai = rep(0,4),mar = rep(0,4))
empty.plot()

CL <- sapply(CurrentL,function(x){x[2]})

legend("left"
       ,legend = paste(as.character(LegFun$Mac)	,CL,sep = "\n")
       ,col = as.character(LegFun$col)
       ,lwd = LegFun$lwd,
       ,border = "transparent",box.col = "transparent",bg = "#FFFFFF80",xpd = NA
)

graphics.off()
if(pdfShow){
  
system(paste("open",pdfName))
}
 # system(paste("open",pdfName))
LoadSettings(inputframe = tempListOne,DensMatrixTemplate = DensMatrixTemplate,pdfname = paste(finalMQQC ,"TimeLines", paste("Correlations",PDFname,sep = "_"),sep = "/"),pdfshow = F)
try(PairsFun(inputframe = tempListOne,DensMatrixTemplate = DensMatrixTemplate,pdfname = paste(finalMQQC ,"TimeLines", paste("Correlations",PDFname,sep = "_"),sep = "/"),pdfshow = F),silent = T)
return(PointsListQuantiles)
}

# try(BSAquan<- CompareComplexStdFromTable(collectList[BSA,],RESettings,F,finalMQQC, PDFname = "LowComplexStandardComparison.pdf", TargetVec = StandardIDs[2],PDF = T, Machines = Machines),silent = T)

# try(ECquan <- CompareComplexStdFromTable(tempListOne = tempListOne,RESettings = RESettings,pdfShow = F,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "ECstd",PDF = T, Machines = Machines),silent = F)

# RESettings <- Param[grep("^RE",names(Param))]
# 
# finalMQQC <- "~/temp/html/"#äParam$htmloutPath
# # AllDataA <-read.csv("/Users/henno/temp/mqqc/_RmqqcFile_Processed/list_collect.csv",stringsAsFactors = F)
# AllData <- AllDataA[grep("ECstd",AllDataA$Name),]
# 
#  try(ECquan <- CompareComplexStdFromTable(tempListOne = AllData,finalMQQC = finalMQQC,RESettings = RESettings,pdfShow = T, PDFname = "ComplexStandardComparisonTest.pdf", TargetVec = "ECstd",PDF = T, Machines = c("Gladys","Statler","Waldorf","Tiffy"),StandardIDs = c("ECstd","BSA")),silent = F)

# try(BSAquan<- CompareComplexStdFromTable(collectList[BSA,],RESettings,F,finalMQQC, PDFname = "LowComplexStandardComparison.pdf", TargetVec = StandardIDs[2],PDF = T, Machines = Machines),silent = T)

   # LoadSettings(tempListOne = collectList[ECstd,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = StandardIDs[1],PDF = T, Machines = c("Gladys"),StandardIDs = c("ECstd","BSA"),lwdjpg = 2)
   # try(Res <- CompareComplexStdFromTable(tempListOne = collectList[BSA,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = StandardIDs[2],PDF = T, Machines = Machines,StandardIDs = StandardIDs),silent = F)

#try(CompareComplexStdFromTable(tempListOne = input,RESettings = RESettings,pdfShow = F,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "",PDF = T, Machines = Machines,StandardIDs = StandardIDs),silent = T)

#try(CompareComplexStdFromTable(collectList[BSA,],RESettings,T,finalMQQC, PDFname = "LowComplexStandardComparison.pdf", TargetVec = StandardIDs[2],PDF = T, Machines = Machines))

#try(CompareComplexStdFromTable(tempListOne = collectList[,],RESettings = RESettings,pdfShow = T,finalMQQC = finalMQQC, PDFname = "ComplexStandardComparison.pdf", TargetVec = "ECstd",PDF = T, Machines = Machines,StandardIDs = StandardIDs))

#try(CompareComplexStdFromTable(tempListOne = collectList,RESettings = RESettings,pdfShow = T,finalMQQC = htmloutPath, PDFname = "ComplexStandardComparison.pdf",PDF = T))



