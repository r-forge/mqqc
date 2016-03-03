medwin <- 
function(x,y,win = 8,...){
  
  ord <- order(as.numeric(as.character(x)))
  x <- x[ord]
  y <- y[ord]
  ro <- ceiling(length(y)/win)
  
  resfun<- sapply(1:length(y),function(r){
    
    st <- r-win
    if(st < 1){st = 1}
    endt = (r+win)
    if(endt > length(y)){
      endt <- length(y)
    }
    yt <- y[st:endt]
    xt <- x[r]
    ytm <- mean(yt,na.rm = T)
    yts <- sd(yt,na.rm = T)
    return(c(xt,ytm,yts))
  })
  resfun <- t(resfun)
  colnames(resfun) <- c("xmu","ymu","ystd")
  
  return(list(x=resfun[,1],y = resfun[,2],ysd=resfun[,3]))
}
