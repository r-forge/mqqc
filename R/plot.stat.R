plot.stat <-
function(x,thresh, name.val,rev = F,bg = "lightblue",main = "2"){
	col.temp <- (colorRampPalette(c("blue","yellow","green"))(thresh*100))
	
		if(rev){
		col.temp <- (colorRampPalette(c("blue","yellow"))(thresh*100))
		col.temp <- rev(c(col.temp,rep("green",thresh*100)))	
		}

	if(x > thresh){
		col.sel <- thresh
		x.range <- c(0,x+x*0.2)
	}else{
		col.sel <- x
		x.range <- c(0,thresh+thresh*0.2)

	}
	max(c(x,thresh))
	barplot2(x,las = 2,col = col.temp[col.sel*100],ylab = name.val,ylim = x.range,bg = bg)
	abline(h=thresh,lty = "dashed",lwd = 2)
	mtext(main,3,cex =0.7,line = 0.3)
	
}
