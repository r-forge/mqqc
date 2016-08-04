plotMsFun <- 
function(msScans = NULL,msmsScans = NULL,xl = c(-1,1),type ="MS1"){
  msScans <<- msScans
  msmsScans <<- msmsScans
  xl <<- xl
  if(length("msmsScans") == 0){
    msmsScans <- msScans
  }
  if(length(msScans) > 0){
    ret <- as.numeric(msScans$Retention.time)
    fun = max
    sel <- ret > min(xl) & ret < max(xl)
    # wifu <-createWindows(ret)
    # MS1 <- aggregate(unfactor(msScans$Base.peak.intensity[sel]),list(wifu$windowsnames[sel]),fun)
    MS1  <- createWindows(ret[sel],otherValue = msScans$Base.peak.intensity[sel],win = 600)$aggRes
    # MS1 <- MS1v[,2]
    # names(MS1) <- MS1v[,1]
      # MS1 <- match()
    # MS2 <- aggregate(unfactor(msScans$Base.peak.intensity[sel]),list(wifu$windows[sel]),fun)
    

    ret2 <- unfactor(msmsScans$Retention.time)
    sel <- ret2 > min(xl) & ret2 < max(xl)
    
    # MS2 <- aggregate(unfactor(msmsScans$Base.peak.intensity[sel]),list(createWindows(ret2)$windows[sel],msmsScans$Identified[sel]),fun)
    MS2 <- createWindows(ret2[sel],otherValue = msmsScans$Base.peak.intensity[sel],win = 600)$aggRes
    # MS2 <- MS2v[,2]
    # names(MS2) <- MS2v[,1]
    # 
    MS1$x <- MS1$x/max(MS1$x)
    MS2$x <- MS2$x/max(MS2$x)
    if(type != "all"){
      fac = 1
      yl <- c(0,1)
    }else{
      fac = -1
      yl <- c(-1,1)
    }
    
    plot(MS1$Group.1,MS1$x,type = "n",ylim = yl,ylab = "",xlab = "",xlim = xl,frame = F,axes = F)
    abline(v = pretty(MS1$Group.1),col = "darkgrey",lwd = 1.5,lty = "dotted")
    
    mtext("MS1\nbase peak",2,cex = 0.6,las = 2)
    
    if(type == "MS1"|type == "all"){
      
      # huhi <<-medwin(MS1$Group.1,MS1$x)
      
      # points(huhi$x,huhi$y,col = "grey40",type = "l")
      plotPOL(MS1$Group.1,MS1$x,col = "grey30",border = "transparent")
      # points(huhi$x,huhi$y,col = "#FFFFFF60",type = "l")
      
    }
    if(type == "MS2"|type == "all"){
      plotPOL(MS2$Group.1[MS2$Group.2 == "-"],MS2$x[MS2$Group.2 == "-"]*fac,col = "grey40",border = "transparent",fun = max)
      plotPOL(MS2$Group.1[MS2$Group.2 == "+"],MS2$x[MS2$Group.2 == "+"]*fac,col = "red",border = "transparent",fun = max)
      
    }
  }else{empty.plot()}
}

# tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,msScans = msScans,msmsScans= msmsScans,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))

