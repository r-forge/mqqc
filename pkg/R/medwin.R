medwin <- 
function(x,y,win = 8,fun = mean,type = "x",...){
  
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
    ytm <- fun(yt,...)
    yts <- sd(yt,na.rm = T)
    return(c(xt,ytm,yts))
  })
  resfun <- t(resfun)
  colnames(resfun) <- c("xmu","ymu","ystd")
  
  return(list(x=resfun[,1],y = resfun[,2],ysd=resfun[,3]))
}
# sel <- sample(1:50000,50000)
# Medi <- medwin(RT[sel],HP[sel],win = 1000,fun = median)
# plot(RT[sel],HP[sel])
# points(Medi$x,Medi$y,col = 2)
# points(loPr2$x,loPr2$fitted,type = "l",col = 3)
# 
# RTCorrected <- sapply(RT,function(x){
#   tx <- Medi$x-x
#   median(Medi$y[abs(tx) == min(abs(tx))])
# })

