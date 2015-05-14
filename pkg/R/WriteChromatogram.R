WriteChromatogram <- 
function(x,colvec = c("black","tomato3","steelblue"),fun = sum,log10 = F,filename= "./chromatogram.pdf"){
  if(log10){
    x$Intensity <- log10(x$Intensity)
  }	
  
  xSepS <- x[x$Sequence != " ",]
  xTimeT <- aggregate(xSepS$Intensity,list(round(xSepS$Retention.time,2)),fun)
  xSep <- x[x$Sequence == " ",]
  xTimeS <- aggregate(x$Intensity,list(round(x$Retention.time,2)),fun)
  
  
  
  
  pdf(pdfname <- paste(dirname(filename),"/","chromatogram_",basename(filename),".pdf",sep = ""),width = 15,height= 5)
  par(mai = c(1,1,0.1,0.1))
  #spfit <- spline(xTimeS[,1],log10(xTimeS[,2]))
  #points(spfit,type = "l")
  
  DPsel <- x$DP.PEP < 0.01
  DPsel[is.na(DPsel)] <- F
  plot(xTimeS[,1],(xTimeS[,2]),type = "l",col = colvec[1],frame = F,xlab = "time [min]",ylab = "Intensity",axes = F)
  
  if(length(DPsel) > 0){
    xDP    <- x[DPsel,]
    xTimeD <- aggregate(xDP$Intensity,list(round(xDP$Retention.time,2)),fun)
    legVec = c(paste("MS/MS",dim(xSepS)[1]),paste("Dependent",length(DPsel[DPsel])))
    rug(xTimeD[,1],col = densCols(xTimeD[,1],colramp = colorRampPalette(c(colvec[3],"black"),alpha = T)))
    
  }else{
    legVec = c(paste("MS/MS",dim(xSepS)[1]))
    
  }
  
  rug(xTimeT[,1],ticksize=0.02,col = densCols(xTimeT[,1],colramp = colorRampPalette(c(colvec[2],"black"),alpha = T)))
  abline(h=0,col = "grey",lwd = 0.)
  
  axis(1)
  axis(2)
  #abline(v = xTimeD[,1],col = colvec[2])
  #points(xTimeD[,1],(xTimeD[,2]),type = "h",col = colvec[2])
  
  #points(xTimeT[,1],(xTimeT[,2]),type = "h",col = colvec[3])
  legend("topleft",legend = legVec , fill = colvec[-1],bty = "n",border = "transparent",title = "Peptides:")
  dev.off()
  #system(paste("open",pdfname))
  return(pdfname)
}
#x <- read.csv("/Users/henno/temp/DPTest/txt/allPeptides.txt",stringsAsFactors = F,sep = "\t")

#WriteChromatogram(AllPeptides,fun = sum,log10= F)
