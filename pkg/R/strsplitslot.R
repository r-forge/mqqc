strsplitslot <- 
  function(x,k=1,str = ";",strreplace = ";",...){
    unlist(sapply(strsplit(as.character(x),split = str,...),function(x){paste(x[k],collapse = strreplace)}))
}
