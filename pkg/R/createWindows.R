createWindows <- 
function(x,win = 300,otherValue = NULL,funfun = max){
  x <<- x
  win <<- win
  difi <- diff(range(x))
  xi <- difi/win
  x2 <- rep(NA,length(x))
  init = min(x)
  setit = init+xi
  fufi <- seq(from = min(range(x)),to = max(range(x)),length.out = win)
  tutis <- .bincode(x,fufi,include.lowest = T)
  if(length(otherValue) == 0){
  retfun <- aggregate(x,list(tutis),mean,na.rm = T)
  tuti <- retfun$x[match(tutis,retfun$Group.1)]
  dens.mzCA  <- table(tutis)
  
  }else{
    retfun <- aggregate(unfactor(otherValue),list(tutis),max,na.rm = T)
    tuti <- retfun$x[match(tutis,retfun$Group.1)]
    dens.mzCA  <- table(tutis)
    reti <- fufi[match(retfun$Group.1,1:length(fufi))]
    retfun$Group.1 <- reti
  }
  
  # while(any(is.na(x2)) |init < max(difi)){
  #   sel <- x >= init & x<= setit
  #   x2[sel] <-  mean(c(init,setit),na.rm = T)
  #   setit = setit+xi
  #   init = init + xi
  #   cat("\r",paste(table(is.na(x2)),collapse = " "))
  # }
  val <- dens.mzCA[match(1:length(fufi),names(dens.mzCA))]
  names(val) <- fufi
  val[is.na(val)] <- 0
  
  return(list(windows = tuti,windowsnames = tutis,allwindows = fufi,full = val,aggRes = retfun))
}
# otherValue = msScans$Base.peak.intensity#[sel]

# MS1 <- createWindows(ret,otherValue = msScans$Base.peak.intensity)




