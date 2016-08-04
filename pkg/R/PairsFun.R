PairsFun <-
function(inputframe,DensMatrixTemplate = NULL,pdfshow = F,pdfname = "pairsfun.pdf",filterMQQC = T,makepdf = T,mainvec = NULL,SelectMatrix = NULL){
  
  
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    sel <- is.na(x)|is.na(y)|is.infinite(x)|is.infinite(y)
    error <- class(try(ct  <- cor.test(x[!sel], y[!sel]),silent = T))
    if(error != "try-error"){
    try(r   <- (ct$estimate))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    cexfinal <- cex.cor * abs(r)
    if(cexfinal < 0.4){cexfinal <- 0.4}
    text(0.5, 0.5, txt, cex = cexfinal,pos = 3)
    text(0.5, 0.45, paste("n =",length(sel[!sel])), cex = 1.2,pos = 1,col = "darkgrey")
    text(0.5, 0.2, paste("p =",signif(ct$p.value)), cex = 1.2,col = "darkgrey")
    }
  }
  if(makepdf){
  pdf(pdfname,width = 15,height = 15)
  }
  if(length(DensMatrixTemplate) > 0){
  cselect <- sapply(DensMatrixTemplate[,1],function(x){grep(x,colnames(inputframe))})
  nam <- DensMatrixTemplate[,2]
  d <- inputframe[,cselect]
  }else{
  d <- inputframe
  }
  if(filterMQQC){
  dl<- sapply(c("Intensity.50.","mass.error.uncal.50.","msmsQuantile.50.","ret.peak.shape.50."),function(x){grep(x,colnames(d))})
  }else{
    dl <- 1:dim(d)[2]
  }
  d[,dl] <- log10(d[,dl])
  d <- apply(d,2,function(x){
    xn <- rank(x)
    xn[is.na(x)] <- NA
    return(xn)
  })
  mnames <- ""
  try(mnames <- strsplitslot(inputframe$Name,1,"_"))
  if(length(mnames)==0){
    mnames = rep(1,dim(inputframe)[1])
  }
  for(i in unique(mnames)){
    ds <- d[mnames == i,]
    dsNArem <- apply(ds,2,function(x){all(is.na(x))})
    ds <- ds[,!dsNArem]
    if(length(mainvec) !=0){
      i <- mainvec
    }
    lp <- function(x,y,rcol = c("black","blue","green","yellow","orange","red"),...){
      #cols = densCols(x,y,colramp = colorRampPalette("#14780960","black"))
      x <- x
      y <- y
      if(length(x[!is.na(x)]) > 1){
      try(points(x,y,col = densCols(x,y,colramp = colorRampPalette(rcol)),cex = 0.8,...),silent = T)
      try(MFU <- medwin(x,y,win = 20),silent = T)
      try(points(MFU$x,MFU$y,col = "red",type = "l"),silent = T)
     try( abline(0,1,col = "grey20",lty = "dashed",lwd = 2),silent = T)
      }else{
        try(points(1,type = "n",...),silent = T)
        legend("top",legend = "not enough\ndatapoints",x.intersp = 0,bty = "n")
      }
    }
    if(length(DensMatrixTemplate) > 0){
    ren <- paste(DensMatrixTemplate[,4],DensMatrixTemplate[,2],sep = ":\nranks\n")[match(colnames(ds),DensMatrixTemplate[,1])]
    colnames(ds)[!is.na(ren)] <- ren[!is.na(ren)] 
    }
    ds <<- ds
    apply(ds,2,function(x){all(is.na(x))})
    
    try(  pairs(ds,upper.panel = panel.cor,lower.panel = lp,pch = 20,main = i),silent = T)
  }
  if(makepdf){
  dev.off()
  }
  if(pdfshow){
  system(paste("open",pdfname))
  }
}
# try(PairsFun(inputframe = tempListOne,DensMatrixTemplate = DensMatrixTemplate,pdfname = paste(finalMQQC ,"TimeLines", paste("Correlations",PDFname,sep = "_"),sep = "/"),pdfshow = T),silent = T)

# try(PairsFun(inputframe = tempListOne,DensMatrixTemplate = DensMatrixTemplate,pdfname = paste(finalMQQC ,"TimeLines", paste("Correlations",PDFname,sep = "_"),sep = "/"),pdfshow = T),silent = T)

# try(PairsFun(inputframe = tempListOne,DensMatrixTemplate = DensMatrixTemplate,pdfname = paste(finalMQQC ,"TimeLines", paste("Correlations",PDFname,sep = "_"),sep = "/"),pdfshow = T))

# graphics.off()
# PairsFun(subs,filterMQQC = F,makepdf = F)

 # try(PairsFun(tempListOne,DensMatrixTemplate = DensMatrixTemplate,pdfname = paste(finalMQQC ,"TimeLines", paste("Correlations",PDFname,sep = "_"),sep = "/"),pdfshow = T))

# PairsFun(SPD,filterMQQC = F,makepdf = F)

# PairsFun(hnm,filterMQQC = F,makepdf = F,mainvec = "Change")
