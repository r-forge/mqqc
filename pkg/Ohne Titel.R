my.read.table2=function(path,sep = "\t",header = T,convertToMatrix = T) {
 s = file.info( path )$size 
  cat("Reading File\r")
 buf <<- readChar( path, s, useBytes=T)
   cat("Splitting File\r")
 buf <<- strsplit(buf,"\n",fixed=T,useBytes=T)[[1]]
 if(convertToMatrix){
 buf <<- strsplit(buf,sep,fixed=T,useBytes=T)
  buf <- unlist(buf)
 cols <<- length(unlist(strsplit(readLines(path,n = 1),sep,fixed = T,useBytes = T)))
 rows <<- length(buf)/cols
 buf <- matrix(buf,nrow = rows,ncol = cols,byrow = T)
 if(header){
	try( 	colnames(buf) <- buf[1,])
 	buf <- buf[-1,]
 }
 return(buf)
}else{return(buf)}
}


path <- "/Users/Selbach/erik/misMatch/allPeptides.txt"
system.time(test <- my.read.table2(path,convertToMatrix = T))
