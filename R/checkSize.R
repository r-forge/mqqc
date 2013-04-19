checkSize <-
function(x){
  if(.Platform$OS.type != "windows"){
	  temp.x <- file.info(x)$size
	  Sys.sleep(1)
	  temp.y <- file.info(x)$size
	  if(temp.y == 0){temp.y <- temp.x -1}
  }else{
    temp.x <- class(try(readLines(x,n = 1),silent = T))
    if(temp.x == "try-error"){
      temp.x <- 0
      temp.y <- 1
    }else{
      temp.x <- 1
      temp.y <- 1
    }
  }
	return(temp.y-temp.x)
}
