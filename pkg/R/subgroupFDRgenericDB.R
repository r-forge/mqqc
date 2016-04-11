subgroupFDRgenericDB <- 
function(evi,ALPHA = 0.01){
# ALPHA <- 0.01
try(data(NameCounts) ) 
Sp <- strsplitslot(gsub("REV__","",evi$Leading.Razor.Protein,fixed = T),1,"_")
length(NameAlternative)
length(FaNa)
Na <- match(Sp,names(FaNa))
RealNames <- NameAlternative[Na]
Nam <- strsplit(RealNames,"#")
uniNa <- unique(unlist(Nam))
SubGroupFDRSelect <- rep(F,dim(evi)[1])
hus<-sapply(uniNa,function(x){
  x <<- x
  if(is.na(x)){
    xu <- is.na(Nam)
  }else{
    xu <- sapply(Nam,function(y){
      any(y == x)
    })
  }

  xu[is.na(xu)] <- F
  
  
  tempEvi <- evi[xu,]
  # print(dim(tempEvi))
  # print(x)
  le <- 1:dim(tempEvi)[1]
  tempEvi <- cbind(tempEvi,le)
  tempEvi <- tempEvi[order(as.numeric(as.character(tempEvi$PEP))),]
  Rev <- grep("+",tempEvi$Reverse,fixed = T)
  Rev <- sort(Rev)
  if(length(Rev) >0){ 
    re <<- 0
    le <<- 1
    print("SGFDR")
    hu <-  (1:length(Rev))/Rev
    FinFDR <- c()
    for(ih in 1:length(hu)){
      ihs <- Rev[ih]
      ihi <- Rev[ih-1]+1
      if(length(ihi) == 0){
        ihi <- 1
      }
      FinFDR <- c(FinFDR,rep(hu[ih],length(ihs:ihi)))
    }
    # stop()
    FinFDR <<- FinFDR
    tempEvi <<- tempEvi
    # stop()
    SubGroupFDRSelect[xu] <<- SubGroupFDRSelect[xu]|FinFDR[order(tempEvi$le)]< ALPHA
    
#     FDR <- sapply(1:length(Rev),function(x){
#       re <<- re+1
#       xr <- Rev[x]
#       xi <- Rev[(x-1)]
#       if(length(xi)==0){
#         xi <- 1
#       }
#       vec <- xi:xr
#       fdr <- re/xr
#       vec <- rep(fdr,length(vec))
#       return(vec)
#     })
    
    
    # stop()
  }else{
    print("NO")
    
    SubGroupFDRSelect[xu] <- SubGroupFDRSelect[xu]|F
  }
  
  # stop()
})
SubGroupFDRSelect[is.na(SubGroupFDRSelect)] <- -1
return(cbind(SubGroupFDRSelect,RealNames))
}
# evi <- read.csv("/Users/henno/temp/MQQCTEMP/evidence.txt",sep = "\t")
# load("/Users/henno/mqqc/temp/ARTIFICDB/new0.3/ApexCutsID.rda")
# load("/Users/henno/mqqc/temp/ARTIFICDB/new0.3/ArtificDB.rda")
# load("/Users/henno/mqqc/temp/ARTIFICDB/new0.3/NameCounts.rda")
# newEvi <-subgroupFDRgenericDB(evi )
