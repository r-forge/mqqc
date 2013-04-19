plot.quans <-
function(temp.plot,log2,xlab ="x",ylab = "y",ref.data,thresh.auto = T,bg ="lightblue",fg.col = 1 ,main = ""){
if(log2){
	temp.plot <- log2(temp.plot)
}

plot(temp.plot,type = "n",xlim = c(1:2),axes = F,frame = T,xlab = xlab,ylab = ylab,lwd = 5,las = 2,fg = fg.col,bty = "n")
mtext(xlab,1,line = 1)
mtext(main,3,line = 0,cex = 0.6)

temp.lwd <- c(4,4,5,4,4)
temp.col <- c(4,2,1,2,4)
for(i in 1:5){
	lines(c(1.2, 1.8),rep(temp.plot[i],2),lwd = temp.lwd[i],col = temp.col[i])
}

if(log2){
#abline(h=sort(c(-ref.data,ref.data,0)),col = "black",lty = "dashed",lwd = 1)	
abline(h=sort(c(-ref.data,ref.data,0)),col = temp.col,lwd = 2,lty = "dotted")	
}
if(!log2){
abline(h=sort(c(ref.data)),col = temp.col,lty = "dotted",lwd = 2)	
	
}
axis(2,las = 2,fg = 1,lwd = 2)
box(lwd = 4,fg = fg.col)

}
