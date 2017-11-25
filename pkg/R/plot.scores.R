plot.scores <-
function (data.i,msScans = NULL,msmsScans = NULL,data.list,pdf.name = "qc.control", open.doc = T,pdfOut = F,BSACheck = F,colType = c("greenblue"))
{
  
  
  
  
  #TODO:   

  cat("\rplotting scores",rep(" ",100))
  #initiation of important vectors 
  if(colType == "greenblue"){
    grad.cols.vec <- c("black",colors()[132],"lightblue",colors()[50])
  }
  if(colType == "traffic"){
    grad.cols.vec <- c("black","#D55E00","yellow2",colors()[50])
  }
  if(colType == "colorblind"){
    grad.cols.vec <-  c("black","#D55E00", "#F0E442", "#009E73")
    #, , "#0072B2", "#D55E00", "#CC79A7","tomato3"
    #c("black","indianred1","yellow2",colors()[50])
  }
  
  
  MaxCol <- "blue"
  colnames(data.i) <- tolower(colnames(data.i))
  summary.data   <- data.list$sd
  thresholds   	<- data.list$th
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
    pdf(.pdf<- paste(pdf.name,".pdf",sep = ""),width = 15.5,height = 7.7,pointsize = 20)
    # plot scores
  }
  real.data 		<- summary.data
  ref.data 		<- thresholds
  score.data 	<<- score
  ms.col <- unlist(color.blind[c(5,6,1)])
  nc.col <- unlist(color.blind[c(3,4,2)])
  sum.scores <- c(sum(unlist(score.data[1:3])),sum(unlist(score.data)[4:6]))
  
  TotalScore <<- unlist(score.data) 
  TotalScore[TotalScore > 1] <- 1
  #TotalScore <- as.data.frame(t(as.data.frame(TotalScore)))
  
  matchPa <- c("msms","msmsCount.50%","msmsQuantile.50%","mass.error.50%","Intensity.50%","peak.shape.50%","MSID.min","ret.width.50%","nLCcombi")
  Temp <- merge.control(names(TotalScore),matchPa)
  
  finalAna <- TotalScore[Temp]
  finalAna[is.na(finalAna)] <- 0		
  TotalScore <- as.data.frame(t(as.data.frame(TotalScore)))
  if(BSACheck){
    finalAna[1] <- TotalScore$ProteinCoverage
  }
  
  TotalScore <- sum(as.numeric(as.character(finalAna)))/(length(finalAna))
  if(TotalScore > finalAna[1]){TotalScore <- finalAna[1]}
  
  if(any(sum.scores > 3)){
    
    temp.xlim <- c(0,max(sum.scores))
    
  }else{temp.xlim <- c(0,3)
        
  }
  nrowVal <- 6
  ncolVal <- 8
  extra <- 0
  totalFill 	<- nrowVal* ncolVal
  
  # profileSpace <- matrix(c(rep(1,(nrowVal-1)),2,rep(3,(nrowVal-1)),4),ncol = 2,nrow = nrowVal)
  profileSpace <- matrix(c(rep(1,(nrowVal-1)),2,rep(3,(nrowVal-1)),4),ncol = 2,nrow = nrowVal)
  profileSpace <- matrix(c(rep(1,(nrowVal-2)),2,5,rep(3,(nrowVal-2)),4,4),ncol = 2,nrow = nrowVal)
  
  
  #any(summary.data$msmsQuantile != 0)& 
  if(  1==0){
    scoreSpace 	 <- matrix(c(6,6,rep(5,(2*(ncolVal-3)-2)),7,7),ncol =(ncolVal-2),nrow = 2 )	
    addNum <- 1
  }else{
    mf <- max(profileSpace)
    scoreSpace 	 <- matrix(c(rep(mf+1,(ncolVal-2)),rep(mf+2,(ncolVal-2))),ncol =(ncolVal-2),nrow = 2 )	
    addNum <- 0
    
  }
  
  byRow <- F
  if(byRow){
    leftSpace	 	<- (nrowVal-2)*(ncolVal-2)/2
    leftSpace 		<- seq(from = 7,to = (leftSpace+6))
    topLeftSpace 	<- min(leftSpace):leftSpace[length(leftSpace)/2]
    downLeftSpace 	<- (leftSpace[length(leftSpace)/2]+1):max(leftSpace)
    leftSpace 		<- rbind(topLeftSpace, topLeftSpace, downLeftSpace, downLeftSpace)
    leftSpace <- leftSpace + addNum
  }else{
    leftSpace	 	<- (nrowVal-2)*(ncolVal-2)/2
    leftSpace 		<- seq(from = max(scoreSpace)+1,to = (leftSpace+max(scoreSpace)))
    n <- length(leftSpace) / (ncolVal-2)
    to <- (ncolVal-2)
    leftSpaceM <- c()
    init <- 1
    for(b in 1:n){
      tempB <- rbind(leftSpace[init:(to*b)], leftSpace[init:(to*b)])
      leftSpaceM <- rbind(leftSpaceM, tempB)
      init <- init + to
    }
    leftSpace <- leftSpaceM
    #leftSpace 		<- as.vector(sapply(leftSpace,function(x){x <- c(x,x)}))
    #leftSpaceM <- matrix(leftSpace[1:],nrow = (nrowVal-2-2),ncol = (ncolVal-2))
    
  }
  
  
  
  totalSpace 		<- cbind(profileSpace,rbind(scoreSpace,leftSpace))
  
  hi = 1.3
  ho = 2
  layout(totalSpace,width = c(2,0.3,0.5,0.5,0.5,0.5,0.5,0.5),heights = c(hi,ho,hi,ho,hi,ho))
  par(mai = c(0,1,0.1,0))
  
  if(any(grep.col("calibrated.retention.time.start",data.i) == 0|grep.col("calibrated.retention.time.finish",data.i) == 0)){
    dots <- F
  }else{dots <- T}
  
  # plotMsFun <- function(xl = c(-1,1),type ="MS1",...){
  #   ret <- as.numeric(msScans$Retention.time)
  #   fun = max
  #   sel <- ret > min(xl) & ret < max(xl)
  #   MS1 <- aggregate(unfactor(msScans$Base.peak.intensity[sel]),list(createWindows(ret)[sel]),fun)
  #   
  #   ret2 <- unfactor(msmsScans$Retention.time)
  #   sel <- ret2 > min(xl) & ret2 < max(xl)
  #   
  #   MS2 <- aggregate(unfactor(msmsScans$Base.peak.intensity[sel]),list(createWindows(ret2[sel]),msmsScans$Identified[sel]),fun)
  #   
  #   MS1$x <- MS1$x/max(MS1$x)
  #   MS2$x <- MS2$x/max(MS2$x)
  #   if(type != "all"){
  #     fac = 1
  #     yl <- c(0,1)
  #   }else{
  #     fac = -1
  #     yl <- c(-1,1)
  #   }
  #   plot(MS1$Group.1,MS1$x,type = "n",ylim = yl,ylab = "relative Intensity",xlim = xl,frame = F,axes = F)
  #   if(type == "MS1"|type == "all"){
  #     plotPOL(MS1$Group.1,MS1$x,col = "grey40",border = "transparent")
  #     
  #   }
  #   if(type == "MS2"|type == "all"){
  #     plotPOL(MS2$Group.1[MS2$Group.2 == "-"],MS2$x[MS2$Group.2 == "-"]*fac,col = "grey40",border = "transparent",fun = max) 
  #     plotPOL(MS2$Group.1[MS2$Group.2 == "+"],MS2$x[MS2$Group.2 == "+"]*fac,col = "red",border = "transparent",fun = max) 
  #     
  #   }
  #   
  # }
  
  
  try(plotData<- plot.profile(data.i=data.i,layout = F,linePlot = dots,BSACheck= BSACheck,plot.legend = F,funfun = plotMsFun,msScans = msScans,msmsScans = msmsScans))


  
    

  
  #lines(x = rep(8.5,2),y = c(-5000,1000),xpd = NA,lwd = 3,col = "grey")
  
  par(mai = c(0.4,2,0.2,0.1))
  #assc#ign("score.data",score.data,envir = .GlobalEnv)
  #barplot(cbind(score.data[1:3],rep(0,3)),names.arg = c("MS-score","nLC"),ylim = temp.xlim,col = ms.col,las = 2,ylab = "score")
  #temp.pos <- barplot(cbind(rep(0,3),score.data[4:6]),ylim = temp.xlim,col = nc.col,las = 2,axes = F,add = T)
  
  round.spec <- function(x){ 
    x<- round(abs(x)*100)
    x[x > 100] <- 100
    x[is.na(x)] <- 100
    x[x == 0] <- 1
    return(x)
  }
  col.temp <- (colorRampPalette(grad.cols.vec)(100))
  score.data <- score.data[names(score.data)!=""]
  orderScores <- c("msms","mass.error","score","msmsQuantile","quan.duplicates.msms","msmsCount","msmsEff","ret.width","peak.shape","quanRetRSD","quanRetSlope","quanRet50ratio")
  if(BSACheck){
    
    orderScores[1] <- "ProteinCoverage"
  }
  
  
  
  score.order <- merge.control(names(score.data),orderScores)
  diff 		<- setdiff(1:length(names(score.data)),score.order[!is.na(score.order)])
  if(length(diff) > 0){
    score.order[is.na(score.order)] <- diff
  }
  
  score.data <- score.data[rev(score.order)]
  
  
  
  namesData <- names(score.data)
  # Renaming should be done better
  namesData[namesData=="quanRetRSD"] <- "ETime rSD"
  namesData[namesData=="quanRetSlope"] <- "ETime slope"
  namesData[namesData=="quanRet50ratio"] <- "ETime balance"
  namesData[namesData=="quan.duplicates.msms"] <- "Duplicates/Peptide IDs"
  namesData[namesData=="msms"] <- "Peptide ID/min"
  namesData[namesData=="ProteinCoverage"] <- "BSA Protein Coverage [%]"
  
  namesData[namesData=="mass.error"]  <- "Mass error in ppm"
  namesData[namesData=="score"] <- "Andromeda Score"
  namesData[namesData=="peak.shape"]   <- "Peak Shape"
  namesData[namesData=="ret.width"]     <- "Peak Width"
  namesData[namesData=="msmsQuantile"] <- "MSMS Intensities"
  namesData[namesData=="msmsEff"]   <- "Efficiency MSMS"
  namesData[namesData=="msmsCount"] <- "Assigned Fragments / Peptide"
  
  par(mai = c(0,0,0,0))
  empty.plot()
  
  LegString <- plotData$name.file
  tempLeg <- LegString[1]
  Thresh <- 40
  if(nchar(tempLeg)> Thresh){
    Out <- c()
    for(i in 1:ceiling(nchar(tempLeg)/Thresh)){
      Out <- c(Out,substr(tempLeg,1,Thresh))
      tempLeg <- substr(tempLeg, (Thresh+1),nchar(tempLeg))
    }
    tempLeg <- Out
    
    tempLeg[1:(length(tempLeg)-1)] <- paste(tempLeg[1:(length(tempLeg)-1)],"-",sep = "")
  }
  
  LegString <-
    c("File:",tempLeg,"","Details:", LegString[-1])
  
  if(length(summary.data$missed.cleavages.percent) > 0){
    if(summary.data$missed.cleavages.percent != "not available"&all(is.na(data.list$SpecStat))){
      mc <- round(as.numeric(summary.data$missed.cleavages),2)
      LegString = c(LegString,paste("missed cleavages:",mc,"%"))	
    }else{
      if(!all(is.na(data.list$SpecStat))){
      SPEC <<- data.list$SpecStat
      SPEC <- SPEC[order(SPEC[,3],decreasing = T),]
      SPEC$Enrichment.p.BH[is.na(SPEC$Enrichment.p.BH)] <- 1
      SPECs <- SPEC[SPEC$Enrichment.p.BH == min(SPEC$Enrichment.p.BH,na.rm = T) ,]
      SpecRes <- apply(SPECs[,c(1,3,4)],1,function(x){
          
          try(data(NameCounts) )
        if(exists("FaNa")){
          xn <- NameAlternative[match(x[1],names(FaNa))]
          if(is.na(xn)){
            xn <- x[1]
          }
          if(any(grepl("#",xn))){
          xn <- unlist(strsplit(as.character(xn),"#"))
          idi =0
          dupli <- T
          while(dupli){
            idi = 1+idi
            xns <- substr(xn,1,idi)
            if(any(duplicated(xns))){
              dupli <- T
            }else{
              dupli <- F
            }
          }
          }else{
            xns <- xn
          }
          
          x[1] <- paste(xns,collapse = "#")
        }
          x <- paste(c(" s: "," c: "," p: "),as.character(x),sep = "")
          x <- paste(x,collapse ="")
        return(x)
      })
      
      LegString = c(LegString,paste("Species Detected:",SpecRes[1]))	
      }
    }
  } 
  
  
  if(length(data.list$UniRef) > 0){
    LegString = c(LegString,paste("Mycoplasma Proteins:",length(unique(data.list$UniRef$id))))	
    
  }
  data.list <<- data.list
  
  if(length(summary.data$DependentPeptides) > 0){
    depString <- unlist(strsplit(summary.data$DependentPeptides,"_#_"))
    depString2 <- sapply(depString,function(x){
      rest <- unlist(strsplit(as.character(x),"##"))
      return(paste(rest[1],"n:",rest[2]))
    })
    depStringFin <- paste(depString2,collapse  = "; ")
    Thresh = 80
    if(nchar(depStringFin)> Thresh){
      Out <- c()
      for(i in 1:ceiling(nchar(depStringFin)/Thresh)){
        Out <- c(Out,substr(depStringFin,1,Thresh))
        depStringFin <- substr(depStringFin, (Thresh+1),nchar(depStringFin))
      }
      depStringFin <- Out
      
      depStringFin[1:(length(depStringFin)-1)] <- paste(depStringFin[1:(length(depStringFin)-1)],"-",sep = "")
      
    }
    legend("bottomleft",paste("   ",c("Modifications:",depStringFin)),bty = "n",cex = 0.5)
    
  }
  legend("top",legend ="",bty = "n",title = "MQQC Result")
  LegString <<- LegString
  legend("left",legend =LegString,bty = "n",cex =0.8)
  
  
  
#----------------
# Frame Lines
  xVal <- c(-0.02,1.04,2.1)
  yVal <- c(-2.18,-0.1)
  # vertical lines
  lines(x = rep( xVal[1],2),y = yVal,xpd = NA,lwd = 3,col = "grey")
  lines(x = rep(xVal[2],2),y =yVal,xpd = NA,lwd = 3,col = "grey")
  lines(x = rep( xVal[3],2),y = yVal,xpd = NA,lwd = 3,col = "grey")
  # horiz lines
  lines(x = range(xVal),y = rep(yVal[1],2),xpd = NA,lwd = 3,col = "grey")
  lines(x = range(xVal),y = rep(yVal[2],2),xpd = NA,lwd = 3,col = "grey")
  lines(x = range(xVal),y = rep(mean(yVal),2),xpd = NA,lwd = 3,col = "grey")
  
  # lines(x = c(xVal,xVal*7),y = rep(yVal*0.57,2),xpd = NA,lwd = 3,col = "grey")
  # lines(x = c(xVal,xVal*7),y = rep(0.59,2),xpd = NA,lwd = 3,col = "grey")
  
  
  par(mai = c(0.5,1,0.3,0.5))
  
  CombiScores <- score[grep("combi",names(score))]
  if(length(CombiScores) > 0){
    CombiScores <- CombiScores[grep("nLC",names(CombiScores),invert = T)]
    CombiScores <- unlist(CombiScores)
    CombiScores <- CombiScores[order(names(CombiScores))]
    
    
    namesData <- c("MS","MSMS","LC","Total")
    
    TotalScore <- mean(CombiScores,na.rm = T)# switch dependent Score to mean Score
    ColScore <- c(col.temp[round.spec(c(CombiScores, TotalScore))])
    SCVecs <- c(CombiScores, TotalScore)
    names(ColScore) <- namesData
    names(SCVecs) <- namesData
    temp.pos 	<- barplot(SCVecs,beside = T,col = ColScore ,horiz = F,names.arg = namesData ,las = 1,border = "transparent",xpd = F,ylim = c(0,1.2),ylab = "MQQC Score")
    ColUse 		<<- cbind(c(col.temp[round.spec(unlist(CombiScores))]),names(score.data),namesData)
    
    #text(temp.pos,0,c("peptide ID","mass error","score","peak shape","elution time","dupl. peptide IDs"),las = 2,srt = 90,adj = c(1.1,1),xpd =NA,srt = 45)
    mtext("System Performance",3,line = 0,cex = 0.6)
    #abline(h=1,col = "grey",lwd = 5)
    abline(h=1,col = MaxCol,lwd = 2)
  }else{
    empty.plot()
    
  }
  #par(mai = c(0.5,2,0.1,0.1))
  
  # col.vec for plot.quans 
  
  
  par(mai = c(0.17,1,0.5,0.2),bg = "lightblue")
  init.i <- 500
  ""
  #plot(1,frame = F,axes=F,xlab = "",ylab = "",type = "n",ylim = c(0,init.i))
  #empty.plot()
  
  # for(col.i in 1:init.i){
  # abline(h = col.i,col = col.temp.2[col.i],lwd = 0.5)
  # }
  
  
  # mtext("Good",2,adj = 1,cex = 0.8)
  # mtext("Poor",2,adj = 0,cex = 0.8)
  # if(BSACheck){
  # mtext("BSA",2,cex = 1.5,line = 4,las = 2,col = "darkgrey")
  # }
  
  
  #mtext("Color\nCode",3,cex = 0.8)
  #legend(max(temp.pos)+ temp.pos[1],max(temp.xlim),legend = c("peptide ID","mass error","score","peak shape","elution time","duplicated peptide IDs"),,fill = c(ms.col,nc.col),xpd = NA,xjust = 0,bty = "n",title = "Legend")
  
  par(mai = c(0,0,0,0))
  #empty.plot <- function(ylimVec = c(0,1)){plot(1,type = "n",axes = F,xlab = "",ylab ="",ylim = ylimVec)}
  #empty.col()
  #empty.plot()
  
  # plot real data
  par(mai = c(0.27,1,0.5,0.2),mgp = c(2.5,0.5,0),bg = "lightblue")
  plot.quans <- function(temp.plot,log2,xlab ="",ylab = "",ref.data,thresh.auto = T,bg ="lightblue",fg.col = 1 ,main = "",ylim = range(temp.plot),temp.col = c(1,2,2),axesOn = T){
    # if fg.vol is 0 
    if(length(fg.col) == 0){fg.col <- 1}
    
    if(log2){
      temp.plot <- log2(temp.plot)
    }
    temp.plot <- na.inf.fun(temp.plot)
    
    #boxplot(temp.plot,type = "n",xlim = c(1:2),axes = F,frame = T,xlab = xlab,ylab = ylab,lwd = 5,las = 2,fg = fg.col,bty = "n",ylim = ylim, boxwex = 2)
    boxplot(temp.plot,range = 0,col = "transparent",xlab = "",ylab = "",type = "n",axes = F,border = "transparent",axes = axesOn,medcol = "grey30")
    
    mtext(xlab,1,line = 1)
    mtext(main,3,line = 0.3,cex = 0.7)
    
    temp.lwd <- c(4,4,5,4,4)
    if(length(ref.data) ==2){
      temp.col <- c("grey50",2,1,2,"grey50")
    }
    #for(i in 1:5){
    #	lines(c(1.2, 1.8),rep(temp.plot[i],2),lwd = temp.lwd[i],col = temp.col[i])
    #}
    
    if(log2){
      #abline(h=sort(c(-ref.data,ref.data,0)),col = "black", lty = "dashed",lwd = 1)	
      abline(h=ref.data,col = temp.col,lwd = 2,lty = c("solid",11,11)	)
    }
    if(!log2){
      abline(h=ref.data,col = temp.col,lwd = 2,lty = c("solid",11,11))	
      
    }
    if(axesOn){
    axis(2,las = 2,fg = 1,lwd = 2,tck = -0.2)
    }
    box(lwd = 4,fg = fg.col)
    boxplot(temp.plot,range = 0,col = fg.col,xlab = xlab,ylab = ylab,type = "n",axes = F,add = T,medcol = "white",medlwd = 2)
    
    
  }
  
  plot.stat <- function(x,thresh, name.val,rev = F,bg = "lightblue",main = "2",col.dir = NULL,xlimV = NULL,RampCols = c(MaxCol,"yellow","green")){
    col.temp <- (colorRampPalette(RampCols)(thresh*100))
    if(length(thresh) == 0){
      thresh <- 0
    }
    if(rev){
      col.temp <- (colorRampPalette(RampCols[-length(RampCols)])(thresh*100))
      col.temp <- rev(c(col.temp,rep(RampCols[length(RampCols)],thresh*100)))	
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
    barplot(x,las = 2,col =col.dir,ylab = name.val,ylim = x.range,bg = bg,angle = 45,density = 35,xpd = F,names.arg = "",tck = -0.2)
    #abline(h=thresh,lwd = 2,col = "grey")
    abline(h=thresh,lwd = 3,col = MaxCol)
    box(lwd = 4,fg = col.dir)
    
    mtext(main,3,cex =0.7,line = 0.3)
    
  }
  
  #plot.stat(summary.data$msms.count,thresholds$msms.count, name.val = "MSMS counts")
  ##
  # Peptide ID/min
  ##
  if(length(thresholds$MSID.min) == 0){
    thresholds$MSID.min <- c(500)
  }
  if(length(summary.data$Isotope.patterns.min) == 0){
  trytest <- try(plot.stat(summary.data$MSID.min,thresholds$MSID.min, name.val = "Features [1/min]",main = "MS", col.dir = col.temp[round.spec(score$MSID.min)]))
  }else{
    trytest <- try(plot.stat(summary.data$Isotope.patterns.min ,thresholds$MSID.min, name.val = "Isotope patterns [1/min]",main = "MS", col.dir = col.temp[round.spec(score$Isotope.patterns.min)]))
  }
  
  
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  ####
  # Intensity
  ####
  thresholds <<- thresholds
  logInt <<- thresholds$Intensities#[c(2,1,3)]
  #logInt <- c(min(logInt)- diff(logInt), logInt)
  #logInt <<- c(logInt,min(logInt) * 0.5,max(logInt) * 1.5)
  trytest <- try(plot.quans(log10(summary.data$Intensity),F,"","log10 Intensity",logInt,fg.col = col.temp[round.spec(score$Intensity)],main = "MS"))
  
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  
  ###
  # mass error
  ###
  trytest <- try(plot.quans(summary.data$mass.error.uncal,F,"","",thresholds$mass.error.cal,fg.col = col.temp[round.spec(score$mass.error)],main = "MS"))
  if(class(trytest) == "try-error"){
    empty.plot()
  }else{
    mtext("Uncalibrated",2,line = 3.3,cex = 0.65)
    
    #mtext("log10(Intensities)",2,line = 2,cex = 0.65)
    
    mtext("Mass error [ppm]",2,line = 2.5,cex = 0.65)
    
  }
  
  #par(mai = c(0.2,1,0.5,0.2),bg = "lightblue")
  
  
  ##
  # ET time balance
  ## 
  
  #try(plot.stat(log2(data.list$sd$quanRet50ratio),thresh = c(log2(data.list$th$quanRet50ratio),-1*log2(data.list$th$quanRet50ratio)),name.val = "log2 ETime balance",main = "nLC",col = ColUse[ColUse[,2] == "quanRet50ratio",1],xlimV = c(-1,1)))
  trytest <- try(plot.stat(data.list$sc$nLCcombi, 1,name.val = "LC profile symmetry",main = "nLC",col = c(col.temp[round.spec(data.list$sc$nLCcombi)])))
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  
  
  ##
  # peak shape
  ##
  trytest <- try(plot.quans(log2(summary.data$ret.peak.shape),F,"","log2(Peak shape)",thresholds$ret.peak.shape,fg.col = col.temp[round.spec(score$peak.shape)],main = "nLC")
  )
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  ##
  # peak width
  ## 
  
  trytest <- try(plot.quans(summary.data$ret.width,F,"","Peak width [s]",log10(thresholds$ret.width),fg.col = col.temp[round.spec(score$ret.width)],main = "nLC",ylim = range(summary.data$ret.width[1:4]),axesOn = F))
  axis(2,at = pretty(summary.data$ret.width),label = round(10^pretty(summary.data$ret.width)),las = 2,tck = -0.2)
  
  
  
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  
  
  
  
  ##
  # Iden efficiency
  ##
  if(BSACheck){
    trytest <- 	try(plot.stat(summary.data$Coverage,thresholds$ProteinCoverage, name.val = "BSA Protein Coverage in %",main = "MSMS", col.dir = col.temp[round.spec(score$ProteinCoverage)]))
    
  }else{
    trytest <- try(plot.stat(summary.data$quan.msms.min,thresholds$quan.msms.min, name.val = "PSMs [1/min]",main = "MSMS", col.dir = col.temp[round.spec(score$msms)]))
  }
  # trytest <- try(plot.stat(summary.data$msmsEff,thresholds$msmsEff,name.val = "Identification efficiency [%]",main = "MSMS",col = col.temp[round.spec(score$msmsEff)]))
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  
  
  ##
  # msmsIntensities
  ##
  #return(summary.data)
  
  #if(any(summary.data$msmsQuantile != 0)){
  trytest <- try(plot.quans(as.numeric(log10(summary.data$msmsQuantile)),F,ref.data = thresholds$msmsQuantile,main = "MSMS",xlab = "",ylab = "",fg.col = col.temp[round.spec(score$msmsQuantile)]))
  #}
  mtext("Peptide Fragments",2,line = 3,cex = 0.65)
  mtext("log10(Intensities)",2,line = 2,cex = 0.65)
  
  
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  
  #if(any(summary.data$msmsMassCount != 0)){
  
  trytest <- try(plot.quans(as.numeric(summary.data$msmsMassCount),F,ref.data = thresholds$msmsCount,main = "MSMS",xlab = "",ylab = "",fg.col = col.temp[round.spec(score$msmsCount)]))
  mtext("Assigned Fragments",2,line = 3,cex = 0.65)
  mtext(" / MSMS",2,line = 2,cex = 0.65)
  
  #}
  if(class(trytest) == "try-error"){
    empty.plot()
  }
  
  
  
  
  # ##
  # # Duplicates
  # ##
  # trytest <- try(plot.stat(summary.data$quan.duplicates.msms*100, thresholds$quan.duplicates.msms*100, name.val = "Duplicates/Peptide IDs in %",rev = T,main = "Misc",col.dir = col.temp[round.spec(score$quan.duplicates.msms)]))
  
  
  # if(class(trytest) == "try-error"){
  # empty.plot()
  # }
  
  # ###
  # # score
  # ###
  # trytest <- try(plot.quans(summary.data$score,F,"","Andromeda score",c(0,50,100,150,400),fg.col = col.temp[round.spec(score$score)],main = "Misc")
  # )
  # if(class(trytest) == "try-error"){
  # empty.plot()
  # }
  
  #empty.plot()
  col.temp.2 <- (colorRampPalette(grad.cols.vec)(init.i))
  
  #par(mai = c(0.1,0.1,0.1,0.1),bg = "transparent")
  #empty.plot()
  
  par(mai = c(0.17,1,0.5,0.2),bg = "white")
  
  hu <-barplot(0.8,las = 2,col ="grey",ylab = "Single Value Metric",ylim = c(0,1.2),bg = "transparent",angle = 45,density = 35,xpd = F,names.arg = "",tck = -0.25)
  abline(h=1,lwd = 3,col = MaxCol)
  text(hu*1.4,1,"Best",xpd = NA,col = MaxCol,pos = 4)
  box(lwd = 4,fg = "grey")
  
  par(mai = c(0.17,1,0.5,0.2),bg = "lightblue")
  
  
  
  
  empty.plot(ylimVec = c(1,5),xlim = c(0.5,1.5))
  abline(h = c(1.5,3,4.5),col = c(2,1,2),lwd = 2,lty = c(11,"solid",11))
  boxplot(c(1,2,3,4,5),add = T,col = "white",frame = F,axes = F,border = "grey",lwd = 1)
  box(lwd = 4,fg = "white")
  
  text(rep(1,5),c(1,2,3,4,5),c("0%","25%","50%","75%","100%"),pos = 4,offset = 1,xpd = NA)##
  mtext("Confidence Area",2,cex = 0.7,line = 0.5) 
  mtext("Metric's Median",2,cex = 0.7,line = 1.5) 
  mtext("LEGEND",3,col = "grey",line =  1.1,xpd = NA,cex = 0.5)
  
  
  
  par(mai = c(0.17,1.1,0.5,0.2),bg = "lightblue")
  init.i <- 500
  ""
  plot(1,frame = F,axes=F,xlab = "",ylab = "",type = "n",ylim = c(0,init.i))
  col.temp.2 <- (colorRampPalette(grad.cols.vec)(init.i))
  for(col.i in 1:init.i){
    abline(h = col.i,col = col.temp.2[col.i],lwd = 0.5)
  }
  
  
  mtext("Good",2,adj = 1,cex = 0.7,xpd = NA)
  mtext("Poor",2,adj = 0,cex = 0.7,xpd = NA)
  mtext("Score Color Code",2,cex = 0.7,xpd = NA,line = 1.3)
  
  
  # if(BSACheck){
  # mtext("MQQC\nBSA",2,cex = 1.5,line = 4,las = 2,col = "darkgrey")
  # }else{
  # mtext("MQQC",2,cex = 1.5,line = 4,las = 2,col = "darkgrey")
  
  # }
  
  
  # ET time slope
  ## 
  
  #try(plot.stat((data.list$sd$quanRetSlope),thresh = c(data.list$th$quanRetSlope,-1*data.list$th$quanRetSlope),name.val = "ETime slope",main = "nLC",col = ColUse[ColUse[,2] == "quanRetSlope",1],xlimV = c(-0.05,0.05)))
  
  ##
  # ET time slope
  ## 
  
  #try(plot.stat((data.list$sd$quanRetRSD),thresh = data.list$th$quanRetRSD,name.val = "ETime rSD",main = "nLC",col = ColUse[ColUse[,2] == "quanRetRSD",1]))
  
  
  ##plot.quans(summary.data$mass.error.cal,F,"mass.error","mass.error in ppm",thresholds$mass.error.cal)
  if(length(data.list$SpecStat) !=0){
    if(!is.na(data.list$SpecStat)){
      par(bg = "white",mai = c(1.5,2,0.5,0.2))
      layout(matrix(1:2,1,2),width = c(0.6,1))
      fr <<- data.list$SpecStat
      
      try(plot(data.list$SpecStat))
    }
  }
  
  
  
  if(pdfOut){
    
    graphics.off()
  }
  cat("\r",getwd())
  
  if(open.doc){
    try(        try(system(paste("open ", .pdf), intern = TRUE, 				ignore.stderr = TRUE)))
  }

return(list(TotalScore = SCVecs,TotalScoreColor = ColScore,plotData = plotData))
}

# tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,msScans = msScans,msmsScans= msmsScans,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))

# tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,data.list = qc.prepare.data,pdf.name = i, open.doc = F,pdfOut = pdfOut, BSACheck = BSACheck)))


#tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))
#tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))

