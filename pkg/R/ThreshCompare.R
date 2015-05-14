ThreshCompare <- 
function(sig,ref,cat = "high",type = "single",log = T){ # cat = "fixed"|"high"|lower
  sig <- sig
  if(length(sig) == 0){return(0)}
  if(is.na(sig)){return(0)}
  if(type == "single"){
#     sig <<- sig
#     ref <<- ref
#     cato <<- cat
#     typo <<- type
#     
    retVal <- sig/ref
    if(length(retVal) == 0){retVal <- NA}
    if(is.na(retVal)){retVal <- 0}
    return(retVal)
  }
  if(cat == "high" & type == "quantile"){
    if(log){
    signorm <- sig - ref[2]
    refnorm <- ref[1] - ref[2]
    if(signorm < 0){return(0)}
    return(signorm/refnorm)
    }
  }
if(cat == "low" & type == "quantile"){
  if(log){
    signorm <- sig - ref[3]
    refnorm <- ref[1] - ref[3]
    res <- signorm/refnorm
    if(res < 0){return(0)}
    return(res)
  }
  if(!log){
    signorm <- sig /ref[3]
    refnorm <- ref[1]/ref[3]
    if(signorm < 0){return(0)}
    return(signorm/refnorm)
  }
}
  if(cat == "fixed" & type == "quantile"){
    if(log){
      if(sig < ref[1]){
        signorm <- sig - ref[2]
        refnorm <- ref[1] - ref[2]
      }else{
        signorm <- sig - ref[3]
        refnorm <- ref[1] - ref[3]
      }
      
      rat <- signorm/refnorm
      return(abs(rat))
    }
  }
  return(0)
}
#print(ThreshCompare(1,c(0.3,0,1),type = "quantile",cat = "low",log = T)
#)
#print(score$peak.shape   <- ThreshCompare(log2(summary.Data$ret.peak.shape[3]),thresholds$ret.peak.shape,type = "quantile",cat = "fixed")
#)
