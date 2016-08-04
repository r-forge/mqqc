WriteChromatogram <- 
function(x,msSC = NULL,msmsSC = NULL,colvec = c("darkgrey","black","steelblue","tomato3"),fun = max,log10 = F,filename= "./chromatogram.pdf",BSAID = NULL,jitfac = 1,contcol = c("orange","pink3"),showpdf = F,ContPrec = 1){
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","tomato3")
  ForConPlotAgg = NULL
  M = NULL
  x <- x
  if(length(msmsSC) > 0){
  x <- msmsSC
  # print("HUI")
  # x$Intensity <- x$
  x$Intensity <- x$Base.peak.intensity
  # x <<- x
  
  }
  if(log10){
    x$Intensity <- log10(x$Intensity)
  }	
  if(length(BSAID) > 0){
    colvec[2] <- "black"
    colvec[1]  <- "darkgrey"
  }
  sumfun <- 2
  msSC <<- msSC
  x$Retention.time <-   as.numeric(as.character(x$Retention.time))
  x$Intensity <-   as.numeric(as.character(x$Intensity))
  x$Mass <-   as.numeric(as.character(x$Mass))
  msSC$Retention.time <-   as.numeric(as.character(msSC$Retention.time))
  msSC$Total.ion.current <-   as.numeric(as.character(msSC$Total.ion.current))
  msSC$Base.peak.intensity <-   as.numeric(as.character(msSC$Base.peak.intensity)) 
  
  if(length(msSC) > 0){
    xSepS <- x[x$Identified == "+",]
    xSep <- x[x$Identified == "-",]
  }else{
    xSepS <- x[x$Sequence != " ",]
    xSep <- x[x$Sequence == " ",]
    
  }
  xTimeT <- aggregate(xSepS$Intensity,list(round(xSepS$Retention.time,sumfun)),fun)
  xTimeS <- aggregate(xSep$Intensity,list(round(xSep$Retention.time,sumfun)),fun)
  
  s1 <- as.numeric(as.character(xTimeS[,2]))
  s2 <- as.numeric(as.character(xTimeT[,2]))
  PieInfo <- c(sum(s1,na.rm = T),sum(s2,na.rm = T))
  names(PieInfo) <- c("Unassigned","Identified")
  filename <<- filename
  pdf(pdfname <- paste(dirname(filename),"/","chromatogram_",basename(filename),".pdf",sep = ""),width = 15,height= 6)
  par(mai = c(1,1,0.5,0.3))
  #spfit <- spline(xTimeS[,1],log10(xTimeS[,2]))
  #points(spfit,type = "l")
  
  DPsel <- x$DP.PEP < 0.01
  DPsel[is.na(DPsel)] <- F
  xTimeS <- xTimeS
  
  if(length(msSC) > 0){
  par(mfrow = c(2,1),mai = c(0,1.2,0.5,0.3))
  layout(matrix(1:2,2,1),height = c(1.2,2))
  MSC <- aggregate(msSC$Base.peak.intensity,list(round(msSC$Retention.time,sumfun)),fun)
  
  plot(MSC$Group.1,MSC$x,type = "n",frame = F,xlab = "time [min]",ylab = "MS1 Basepeak Intensity",axes = F,main = unique(x$Raw.file)[1],las = 1,mgp = c(5,1,0))
  points(MSC$Group.1,MSC$x,type = "h",frame = F,xlab = "time [min]",ylab = "Basepeak Intensity",axes = F,main = unique(x$Raw.file)[1],las = 1,mgp = c(5,1,0))
  
  axis(2,las = 1)
  # xTimeS <- aggregate(xSep$Intensity,list(round(xSep$Retention.time,sumfun)),fun)
  
  # par(mai = c(1,1,0.5,0.3),mfrow = c(1,3))
  }else{
  
  plot(xTimeS[,1],(xTimeS[,2]),type = "l",col = colvec[1],frame = F,xlab = "time [min]",ylab = "Intensity",axes = F,main = "",ylim = yv <- range(c(s1,s2,na.rm = T)))
  abline(h = pretty(yv),col = "grey",lty = "dotted")
  points(xTimeT[,1],(xTimeT[,2]),type = "h",col = colvec[2])
  }
  

  
  if(length(DPsel) > 0){
    try(xDP    <- x[DPsel,])
    try(xTimeD <- aggregate(xDP$Intensity,list(round(xDP$Retention.time,sumfun)),fun))
    a <- paste("Identified",dim(xSepS)[1])
    b <-paste("Dependent",length(DPsel[DPsel]))
    try(legVec <-  c(a,b))
    xTimeD[,1] <- jitter(xTimeD[,1],factor = jitfac)
    testcol <- densCols(xTimeD[,1],colramp = colorRampPalette(c("grey",colvec[3])))
    tsf <- log10(xTimeD[,2])
    tsf <- (tsf/max(tsf,na.rm = T))*-0.025
    for( i in 1:length(xTimeD[,1])){
      try(rug(xTimeD[i,1],ticksize = tsf[i],col =testcol[i]))
      
    }
    try(PieInfo <- c(PieInfo,sum(xTimeD[,2],na.rm = T)))
    names(PieInfo)[length(PieInfo)] <- "Dependent"
  }else{
    legVec = c(paste("PSM",dim(xSepS)[1]))
    
  }
  xTimeT[,1] <- jitter(xTimeT[,1],factor = jitfac)
  tsf <- log10(xTimeT[,2])
  tsf <- (tsf/max(tsf,na.rm = T))*0.025
  if(length(xTimeT[,1]) > 1){
  testcol <- densCols(xTimeT[,1],colramp = colorRampPalette(c("grey",colvec[2])))
  }else{testcol <- colvec[2]}
  for( i in 1:length(xTimeT[,1])){
    #print(tsf[i])
    try(  rug(jitter(xTimeT[i,1],factor = jitfac),ticksize=as.numeric(as.character(tsf[i])),col = testcol[i]),silent = T)
  
  }
  if(length(msSC) > 0){
    par(mai = c(1,1.2,0.2,0.3))
    
    # abline(h = pretty(yv),col = "grey",lty = "dotted")
    plot(xTimeS[,1],(xTimeS[,2]),type = "h",col = colvec[1],frame = F,xlab = "time [min]",ylab = "MS2 Basepeak Intensity",axes = F,main = "",ylim = yv <- range(c(s1,s2,na.rm = T)),las = 1,mgp = c(5,1,0))
    points(xTimeT[,1],(xTimeT[,2]),type = "h",col = colvec[2])
    
  }
  #Find BSA:
  correctPie <- 0
  if(length(BSAID) > 0){
    BSAID <- unique(unlist(strsplit(BSAID,";")))
  # Never use this here, will evaluate each razor protein entry, will freeze the session
  #BSAID <- sapply(strsplit( x$Proteins,";"),function(x){x[1]})
   colused <- colvec[4]
    it <- 3
   if(length(BSAID) > 1){
    colvec <- colvec[-4]
   colvec <- c(colvec,colorRampPalette(cbPalette)(length(BSAID)))
   }
    
    
    for(iBSAID in BSAID){
    BSAgrep <- grep(iBSAID,x$Proteins,fixed =T) 
    it <- it+1
    if(length(BSAgrep) == 0){
      iBSAID = 0
    }else{ 
      if(length(BSAID) > 1){
        colused <- colvec[(it)]
      }

      BSAx <- x[BSAgrep,]
      #tempBSAx <- rbind(cbind(0,BSAx$Retention.time-BSAx$Retention.Length/2),cbind(BSAx$Intensity,BSAx$Retention.time),cbind(0,BSAx$Retention.time+BSAx$Retention.Length/2))
      BSAxt <- aggregate(unfactor(BSAx$Intensity),list(round(BSAx$Retention.time,sumfun)),fun)
      BSAxtMass <- aggregate(unfactor(BSAx$m.z),list(round(BSAx$Retention.time,sumfun)),max)
      try(BSAxtMass[sort(BSAxt[,2],decreasing = T)[16] > BSAxt[,2],2]<- NA,silent = T)
      BSAxt[,1] <- jitter(BSAxt[,1],factor = jitfac)
      
      tsf <- log10(BSAxt[,2])
      tsf <- (tsf/max(tsf,na.rm = T))*0.025
      testcol <- colvec[4]
      try(testcol <- densCols(BSAxt[,1],colramp = colorRampPalette(c("grey",colvec[4]))))
      for( i in 1:length(BSAxt[,1])){
        #print(tsf[i])
        try(  rug((BSAxt[i,1]),ticksize=as.numeric(as.character(tsf[i])),col = testcol[i]),silent = T)
        
      }
      points(BSAxt[,1],(BSAxt[,2]),type = "h",col = colused)
      text(BSAxt[,1],(BSAxt[,2]),round(BSAxtMass[,2],sumfun),type = "h",col = colused,frame = F,xlab = "time [min]",ylab = "Intensity",axes = F,cex = 0.3,srt = 90,pos = 3)
      
      lines(range(BSAxt[,1]),c(0,0),col = colused)
      legVec <- c(legVec,paste(iBSAID,length(unique(BSAgrep))))
      
      AddValueToString <-  function(x,addval,split = " ",...){
        tempx <- unlist(strsplit(x,split,...))
        tempx[length(tempx)] <- as.character(as.numeric(tempx[length(tempx)])+as.numeric(addval))
        return(paste(tempx,collapse = " "))
      }
      legVec[1] <-  AddValueToString(legVec[1],-length(unique(BSAgrep)))
      if(length(legVec) == 2){
        colvec <- colvec[-3]
      }
      
      try(PieInfo <- c(PieInfo,sum(BSAxt[,2],na.rm = T)))
      try(correctPie <- correctPie+sum(BSAxt[,2],na.rm = T))
      names(PieInfo)[length(PieInfo)] <- iBSAID     
    }
    }
   
  }
  try(PieInfo[2] <- PieInfo[2]-correctPie)
  axis(1)
  axis(2,las = 2)
  mtext("time [min]",1,line = 2.5)
  
  # axis(4)
  
  #abline(v = xTimeD[,1],col = colvec[2])
  #points(xTimeD[,1],(xTimeD[,2]),type = "h",col = colvec[2])
  
  #points(xTimeT[,1],(xTimeT[,2]),type = "h",col = colvec[3])
  
  
  # add contaminant Information:
  try(Contaminants <- read.csv(paste(path.package("mqqc"),"data/contaminants.csv",sep = "/"),skip = 6,sep = ";"),silent = T)
  Contaminants$Possible.origin.and.other.comments <- sapply(Contaminants$Possible.origin.and.other.comments,function(x){
    gsub("^.",toupper(substr(x,start = 1,stop = 1)),x)
    
  })
  if(exists("Contaminants")){
    xSep <- x[x$Sequence == " ",]
    xCon <- round(Contaminants$Monoisotopic.ion.mass..singly.charged.,ContPrec)
    xSepMass <- round(xSep$Mass,ContPrec)
    ConSum <- aggregate(xSep$Intensity,list(xSepMass),sum,na.rm = T)
    
    M <- merge(ConSum,cbind(xCon,Contaminants),by = 1)
    
    ForConPlot <- merge.control(round(xSep$Mass,ContPrec),M[,1])
    ForConPlotAgg <- aggregate(xSep$Intensity[ForConPlot],list(xSep$Retention.time[ForConPlot]),sum,na.rm = T)
    ForConPlotAggM <- aggregate(xSep$Mass[ForConPlot],list(xSep$Retention.time[ForConPlot]),function(x){
      x <- table(x)
      ret <- names(x)[x == max(x)]
      if(length(ret) > 1){
        xret <- x[x == max(x)]
        ret <- ret[xret == max(xret)]
      }
      return(ret)
    })
    #contcol
    
    
    
    
    
    MPIE <- aggregate(M[,2],list(M$Compound.ID.or.species),function(x){c(sum(x,na.rm = T),length(x))})
    MPIELaterPlot <- aggregate(M[,2],list(paste(M$Possible.origin.and.other.comments,M$Compound.ID.or.species,sep = "##")),function(x){c(sum(x,na.rm = T),length(x))})
    
    colnames(MPIE$x) <- c("SummedIntensities","nPeaks")
    write.csv(MPIE, paste(dirname(filename),"/","unidentified_contaminants_",basename(filename),".csv",sep = ""))
    MPIEVec <- MPIE$x[,1]
    names(MPIEVec ) <- MPIE[,1]
    #MPIEVec <- MPIEVec[names(MPIEVec) != "Peptide"]
    tempAGGvec <- rep("Non Peptide Contaminants",length(MPIEVec))
    tempAGGvec[names(MPIEVec) =="Peptide"] <- "Peptide Contaminants"
    MPIEVecSplit <- aggregate(MPIEVec,list(tempAGGvec),sum,na.rm = T)
    MPIEVecSplitvec <- MPIEVecSplit[,2]
    names(MPIEVecSplitvec) <- MPIEVecSplit[,1]
    tempAGGvec
    PieInfo[1] <- PieInfo[1] - sum(MPIEVec,na.rm = T)

    Frac <- sum(MPIEVec,na.rm =T)/sum(PieInfo,sum = T)
    if(is.na(Frac)){Frac <- 1}
    if(Frac> 0.01){
    try(tempFEfa <- round(as.numeric(unlist(ForConPlotAggM$x)),1))
    try(tempFEfa <- round(as.numeric(unlist(ForConPlotAggM$x)),1))
      
    try(sel <- as.numeric(ForConPlotAgg[,2])/max(xTimeS[,2]) > 0.05)
    Tys <- M$Compound.ID.or.species[match(round(tempFEfa,1),round(M$Monoisotopic.ion.mass..singly.charged.,1))]
    TysC <- c(contcol)[as.numeric(grepl("Peptide",Tys))+1]
    try(    points(as.numeric(ForConPlotAgg[,1])[!sel],as.numeric(ForConPlotAgg[,2])[!sel],col = TysC[!sel],lwd = 0.5,type = "h"))
    try(    points(as.numeric(ForConPlotAgg[,1])[sel],as.numeric(ForConPlotAgg[,2])[sel],col = TysC[sel],lwd = 0.5,type = "h"))
    
    try(    text(jitter(as.numeric(ForConPlotAgg[,1])[sel]),as.numeric(ForConPlotAgg[,2])[sel],paste(tempFEfa[sel]),col = TysC[sel],cex = 0.25,srt = 90,pos = 3))
    
    }
    
    if(Frac< 0.01|1){
      MPIEVec <- sum(MPIEVec)
      names(MPIEVec) <- "contaminants"
    }else{
      tempFrac <- MPIEVec/sum(PieInfo)
      if(any(tempFrac > 0.01)){
        MPIEVecRest <- sum(MPIEVec[tempFrac <0.01])
        MPIEVec <- c(MPIEVec[tempFrac >=0.01],MPIEVecRest)
      }else{
        MPIEVec <- sum(MPIEVec)
        names(MPIEVec) <- "contaminants"
      }
    }
    PieInfo <- c(PieInfo,MPIEVec)
    # try(legend("topleft",legend = c(legVec,paste("Contaminants by MS1")) , fill = c(colvec[-1][1:length(legVec)],contcol),bty = "n",border = "transparent",title = "Peptides:"))
    
  }else{
    # try(legend("topleft",legend = c(legVec) , fill = c(colvec[-1][1:length(legVec)]),bty = "n",border = "transparent",title = "Peptides:"))
    
  }
  


  PieInfo[is.na(PieInfo)] <- 0
  colvec <- colvec
  PieInfo <<-  PieInfo
 
  replacePoint  <- grep("contaminants",names(PieInfo))
  if(length(replacePoint) > 0){
    
    MPIEVecSplitvec <- MPIEVecSplitvec
    if(length(PieInfo) == replacePoint){
    PieInfo <- c(PieInfo[1:(replacePoint-1)],MPIEVecSplitvec)
    }else{
      PieInfo <- c(PieInfo[1:(replacePoint-1)],MPIEVecSplitvec,PieInfo[(replacePoint+1):length(PieInfo)]  )
    }
  }
  
  PieInfo[is.na(PieInfo)] <- 0
  colch <-c(colvec[1:length(PieInfo[ grepl("Contaminants",names(PieInfo))])],contcol[1:length(MPIEVecSplitvec)])
  legend("topleft",legend = names(PieInfo),fill = colch,cex = 0.8,border = "transparent",title = "cumulative Intensities:",box.col = "transparent",bg = "transparent")
  
  par(new = T)
  par(mai = c(2,0,0.8+0.4*length(BSAID),11.5),lwd = 0.1)
  try(pie(PieInfo,border = "transparent",radius = 0.4,col=colch,cex = 0.7,lwd = 0.5,main = "",labels = NA))
  # try(pie(PieInfo,border = "white",radius = 0.4,col=c(colvec[1:length(PieInfo[names(PieInfo) != "contaminants"])],contcol),cex = 0.5,labels = NA,lwd = 0.5))
  par(mai = c(0,0,0.5,0))
  layout(matrix(1:3,1,3),width = c(1.5,2,2))
  try(pie(PieInfo,border = "transparent",radius = 0.4,col=colch ,cex = 0.7,lwd = 0.5,main = "All Peaks\nCumulative Intensities"))

  if(exists("MPIE")){
    PieCut <- function(x,cutT = 0.05){
      if(is.vector(x)){
        x <- t(as.matrix(x))
      }
      xnum <- as.numeric(x[,2])
      xnum <- xnum/sum(xnum,na.rm = T)
      xreal <- x[xnum >= cutT,]
      xadd <-  x[xnum < cutT,]
      if(is.vector(xadd)){xadd <- t(as.matrix(xadd))}
      xreal <- rbind(xreal,c("Other",sum(as.numeric(xadd[,2])),sum(as.numeric(xadd[,3]))))
      xreal[,1] <- sapply(strsplit(xreal[,1],"##"),function(x){x[1]})
      MPIEoutLabelOther <-t(as.matrix(MPIEoutLabelOther))
      
      return(xreal)
    }
    MPIEout <- as.matrix(MPIELaterPlot)
    MPIEout <- MPIEout[order(as.numeric(MPIEout[,2]),decreasing = T),]
    MPIEout <- MPIEout[as.numeric(MPIEout[,3]) > 1,]
    MPIEoutLabel <- t(sapply(strsplit(MPIEout[,1],"##"),unlist))
    if(is.vector(MPIEoutLabel)){
      MPIEoutLabel <- t(as.matrix(MPIEoutLabel))
    }
    try(MPIEoutLabelPeps <- MPIEout[MPIEoutLabel[,2] == "Peptide",])
    try(MPIEoutLabelOther <- MPIEout[MPIEoutLabel[,2] != "Peptide",])

    try(MPIEoutLabelPeps <- PieCut(MPIEoutLabelPeps))
    try(MPIEoutLabelOther <- PieCut(MPIEoutLabelOther))
    
    MPIEoutLabelPeps <- MPIEoutLabelPeps[!is.na(MPIEoutLabelPeps[,2]),]
    MPIEoutLabelOther <- MPIEoutLabelOther[!is.na(MPIEoutLabelOther[,2]),]
    if(is.vector(MPIEoutLabelOther)){
      MPIEoutLabelOther <- t(as.matrix(MPIEoutLabelOther))
    }
    
    if(dim(MPIEoutLabelPeps)[1] > 0){
    try(pie(as.numeric(MPIEoutLabelPeps[,2]),labels =paste(gsub("##Peptide","",MPIEoutLabelPeps[,1]),"n:",as.numeric(MPIEoutLabelPeps[,3])),radius = 0.4,cex = 0.7,lwd = 0.5,col = colorRampPalette(cbPalette)(dim(MPIEoutLabelPeps)[1]),border = "white",main = "Potential Protein contaminants by MS1\nCumulative Intensities"),silent = T)
    try(symbols(0,0,circles = 0.4,add = T,ylim = c(0,1),xlim = c(0,1),inches = F,fg = contcol[2],lwd = 3))
    try(pie(as.numeric(MPIEoutLabelOther[,2]),labels =paste(MPIEoutLabelOther[,1],"n:",as.numeric(MPIEoutLabelOther[,3])),radius = 0.4,cex = 0.7,lwd = 0.5,col = colorRampPalette(cbPalette)(dim(MPIEoutLabelOther)[1]),border = "white",main = "Non Peptide Contaminants by MS1\nCumulative Intensities"),silent = T)
    try(symbols(0,0,circles = 0.4,add = T,ylim = c(0,1),xlim = c(0,1),inches = F,fg = contcol[1],lwd = 3))
    }
  }
#abline(h=0,col = "grey",lwd = 0)

    dev.off()
if(showpdf){
  system(paste("open",pdfname))
}
sumall <- sum(as.numeric(unlist(PieInfo)),na.rm = T)
  return(list(all = xTimeS,identified = xTimeT,contaminantsProfile = ForConPlotAgg,contaminants = M,Int = PieInfo,IntPerc=sapply(PieInfo,function(x){as.numeric(x)/sumall*100})))
}

# try(ChrPath <- WriteChromatogram(tempAllPeptides,msSC = msScans,msmsSC = msmsScans,filename = "test",BSAID =as.character(qc.prepare.data$IdentifiedProteins) ,jitfac = 0))

# try(ChrPath <- WriteChromatogram(tempAllPeptides,msSC = msScans,msmsSC = msmsScans,filename = i,BSAID =as.character(qc.prepare.data$IdentifiedProteins) ,jitfac = 0))

# try(ChrPath <- WriteChromatogram(x = tempAllPeptides,filename = i,jitfac = 0,showpdf = T))


 # temp <- read.csv("/Users/henno/temp/txtSILACFUN/msScans.txt",sep = "\t")
# sel <- temp$Identified == "-"
# plot(temp$Retention.time[sel],temp$Intens.Comp.Factor[sel],type = "h",col = "grey")
# sel <- temp$Identified == "+"
# points(temp$Retention.time[sel],temp$Intens.Comp.Factor[sel],type = "h")
# 
# SIAP <- read.csv("/Users/henno/temp/txtSILACFUN/allPeptides.txt",sep = "\t")
# LFAP <- read.csv("/Users/henno/temp/txtLF/allPeptides.txt",sep = "\t")
# LFID <- apply(subset(LFAP,select = c("Raw.file","MSMS.Scan.Numbers")),1,function(x){
#   x2 <- unlist(strsplit(x[2],";"))
#   x2 <- paste(x[1],x2)
#   return(x2)
# })
# SIID <- apply(subset(SIAP,select = c("Raw.file","MSMS.Scan.Numbers")),1,function(x){
#   x2 <- unlist(strsplit(x[2],";"))
#   x2 <- paste(x[1],x2)
#   return(x2)
# })
# SIIDUN <- unlist(SIID)
# FU <- sapply(LFID,function(x){
#   any(x!=SIIDUN)
# })
# 
# LFAPID <- paste(LFAP$Raw.file,LFAP$MSMS.Scan.Numbers)

# dim(SIAP)
# dim(LFAP)
# 
# 
# 
# # temp1 <- read.csv("/Users/henno/temp/txtSILACFUN/allPeptides.txt",sep = "\t")
# temp2 <- read.csv("/Users/henno/temp/txtSILACFUN/msScans.txt",sep = "\t")
# # temp3 <- read.csv("/Users/henno/temp/txtSILACFUN/msmsScans.txt",sep = "\t")
# graphics.off()
# hu <- WriteChromatogram(x=temp1,msSC = temp2,msmsSC = temp3,showpdf = T)
# x <- 
#x <- read.csv("/Users/henno/temp/FixMQQC/txt/allPeptides.txt",stringsAsFactors = F,sep = "\t")
#x <- read.csv("/Users/henno/temp/test/KOSHHS/allPeptides.txt",stringsAsFactors = F,sep = "\t")

#try(ChrPath <- WriteChromatogram(x,filename = "fwef",BSAID =NULL ,jitfac = 0,showpdf = T))


# keep <- WriteChromatogram(x,fun = sum,log10= F,jitfac = 0,showpdf = T)
# sumall <- sum(as.numeric(unlist(keep$Int)),na.rm = T)
# 
# # try to match Contaminants
# print(sum(keep$Int)-sum(as.numeric(x$Intensity),na.rm = T))
# keep$Int
#funhu <-spline(xTimeS[,1],xTimeS[,2])
#txtplot(xTimeS[,1],xTimeS[,2],width = 150)
#hui   <- txtplot(funhu$x[funhu$y > 0],funhu$y[funhu$y > 0],width = 150,xlab = "time [min]",ylab = "Intensity")
#hui   <- txtplot(x$Retention.time,x$Intensity,width = 150,xlab = "time [min]",ylab = "Intensity")
