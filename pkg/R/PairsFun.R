PairsFun <- 
function(inputframe,DensMatrixTemplate = NULL,pdfshow = F,pdfname = "pairsfun.pdf",filterMQQC = T,makepdf = T,mainvec = NULL){
  
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    sel <- is.na(x)|is.na(y)|is.infinite(x)|is.infinite(y)
    r <- abs(cor(x[!sel], y[!sel]))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    cexfinal <- cex.cor * r
    if(cexfinal < 0.2){cexfinal <- 0.2}
    text(0.5, 0.5, txt, cex = cexfinal,pos = 3)
    text(0.5, 0.5, length(sel[!sel]), cex = 0.8,pos = 1,col = "darkgrey")
    
  }
  if(makepdf){
  pdf(pdfname,width = 15,height = 15)
  }
  if(length(DensMatrixTemplate) > 0){
  cselect <- sapply(DensMatrixTemplate[,1],function(x){grep(x,colnames(inputframe))})
  
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
  mnames <- ""
  try(mnames <- strsplitslot(inputframe$Name,1,"_"))
  for(i in unique(mnames)){
    ds <- d[mnames == i,]
    dsNArem <- apply(ds,2,function(x){all(is.na(x))})
    ds <- ds[,!dsNArem]
    if(length(mainvec) !=0){
      i <- mainvec
    }
    lp <- function(x,y,rcol = c("black","blue","green","yellow","orange","red"),...){
      #cols = densCols(x,y,colramp = colorRampPalette("#14780960","black"))
      points(x,y,col = densCols(x,y,colramp = colorRampPalette(rcol)),cex = 0.5,...)
      abline(0,1,col = "darkgrey",lty = "dotted")
    }
    try(  pairs(ds,upper.panel = panel.cor,lower.panel = lp,pch = 20,main = i),silent = T)
  }
  if(makepdf){
  dev.off()
  }
  if(pdfshow){
  system(paste("open",pdfname))
  }
}
# PairsFun(SPD,filterMQQC = F,makepdf = F)

# PairsFun(hnm,filterMQQC = F,makepdf = F,mainvec = "Change")
