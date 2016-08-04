plotPOL <- 
function(x,y,sel = 1:length(x),plotpo = T,fun = min,hl = NULL,...){
  x <- x[sel]
  y <- y[sel]
  if(length(hl) > 0){
    x <- c(min(x,na.rm = T),x,max(x,na.rm = T))
    y <- c(hl,y,hl)
    
  }
  if(plotpo){
    Hui <-polygon(c(x, rev(x)), c(y, rep(fun(y),length(x))),
                  ...)
  }
  return(list(x = x,y = y))
}


# plot(1,type = "n",xlim = range(as.numeric(names(dens.mzCA))),ylim = c(0,1))
# plotPOL(names(dens.mzCA),dens.mzCA/max(c(dens.mzA,dens.mzCA)),col = "orange",type = "l",border = "transparent")
