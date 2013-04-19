round.spec <-
function(x){ 
		x<- round(x*100)
		if(x > 100){x <- 100}
		if(is.na(x)){x <- 1}
		return(x)
		}
