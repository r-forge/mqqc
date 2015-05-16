WriteChromatogram <- 
function(x,colvec = c("black","tomato3","steelblue","tomato3"),fun = sum,log10 = F,filename= "./chromatogram.pdf",BSAID = NULL){
  if(log10){
    x$Intensity <- log10(x$Intensity)
  }	
  if(length(BSAID) > 0){
    colvec[2] <- "darkgrey"
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
    try(xDP    <- x[DPsel,])
    try(xTimeD <- aggregate(xDP$Intensity,list(round(xDP$Retention.time,2)),fun))
    a <- paste("MS/MS",dim(xSepS)[1])
    b <-paste("Dependent",length(DPsel[DPsel]))
    try(legVec <-  c(a,b))
    #print(legVec)
    xTimeD[,1] <- jitter(xTimeD[,1])
    testcol <- densCols(xTimeD[,1],colramp = colorRampPalette(c("grey",colvec[3])))
    tsf <- log10(xTimeD[,2])
    tsf <- (tsf/max(tsf,na.rm = T))*-0.025
    for( i in 1:length(xTimeD[,1])){
      try(rug(xTimeD[i,1],ticksize = tsf[i],col =testcol[i]))
      
    }
  }else{
    legVec = c(paste("MS/MS",dim(xSepS)[1]))
    
  }
  xTimeT[,1] <- jitter(xTimeT[,1])
  
  tsf <- log10(xTimeT[,2])
  tsf <- (tsf/max(tsf,na.rm = T))*0.025
  testcol <- densCols(xTimeT[,1],colramp = colorRampPalette(c("grey",colvec[2])))
  for( i in 1:length(xTimeT[,1])){
    #print(tsf[i])
    try(  rug(jitter(xTimeT[i,1]),ticksize=as.numeric(as.character(tsf[i])),col = testcol[i]))
  
  }
  
  #Find BSA:
  if(length(BSAID) > 0){
    BSAgrep <- grep(BSAID,sapply(strsplit(x$Proteins,";"),function(x){x[1]}))
    if(length(BSAgrep) == 0){
      BSAID = 0
    }else{ 
      BSAx <- x[BSAgrep,]
      BSAxt <- aggregate(cbind(BSAx$Intensity),list(round(BSAx$Retention.time,2)),fun)
      BSAxtMass <- aggregate(cbind(BSAx$m.z),list(round(BSAx$Retention.time,2)),max)
      BSAxtMass[sort(BSAxt[,2],decreasing = T)[16] > BSAxt[,2],2]<- NA
      BSAxt[,1] <- jitter(BSAxt[,1])
      
      tsf <- log10(BSAxt[,2])
      tsf <- (tsf/max(tsf,na.rm = T))*0.025
      testcol <- densCols(BSAxt[,1],colramp = colorRampPalette(c("grey",colvec[4])))
      for( i in 1:length(BSAxt[,1])){
        #print(tsf[i])
        try(  rug((BSAxt[i,1]),ticksize=as.numeric(as.character(tsf[i])),col = testcol[i]))
        
      }
      points(BSAxt[,1],(BSAxt[,2]),type = "h",col = colvec[4],frame = F,xlab = "time [min]",ylab = "Intensity",axes = F)
      text(BSAxt[,1],(BSAxt[,2]),round(BSAxtMass[,2],2),type = "h",col = colvec[4],frame = F,xlab = "time [min]",ylab = "Intensity",axes = F,cex = 0.3,srt = 90,pos = 3)
      
      lines(range(BSAxt[,1]),c(0,0),col = colvec[4])
      
      legVec <- c(legVec,paste(BSAID,length(BSAgrep)))
      print(legVec)
      if(length(legVec) == 2){
        colvec <- colvec[-3]
      }
      
      
    }
    
  }
  
  
  
#abline(h=0,col = "grey",lwd = 0)
  
  axis(1)
  axis(2)
  #abline(v = xTimeD[,1],col = colvec[2])
  #points(xTimeD[,1],(xTimeD[,2]),type = "h",col = colvec[2])
  
  #points(xTimeT[,1],(xTimeT[,2]),type = "h",col = colvec[3])
  try(legend("topleft",legend = legVec , fill = colvec[-1],bty = "n",border = "transparent",title = "Peptides:"))
  dev.off()
  system(paste("open",pdfname))
  return(pdfname)
}

#x <- read.csv("~/temp/test/txt/allPeptides.txt",stringsAsFactors = F,sep = "\t")

#WriteChromatogram(x,fun = sum,log10= F,BSAID = BSAID)


