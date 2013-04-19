plot.scores <-
function (data.i,data.list,pdf.name = "qc.control", open.doc = F)
{
	cat("\rplotting scores",rep(" ",100))
#initiation of important vectors 
grad.cols.vec <- c("blue","orange","yellow","green")


colnames(data.i) <- tolower(colnames(data.i))
summary.data 	<- data.list$sd
thresholds 		<- data.list$th
score		 	<- data.list$sc
data.i.quant 	<- data.list$diq

na.inf.fun <- function(x){
	x[is.na(x)] 		<- 0
	x[is.infinite(unlist(x))] 	<- 0
	return(x)
}


score <- na.inf.fun(score)

color.blind <- list(	

		yellow = rgb(240,228,66,max = 255),
		orange = rgb(230,159,0,max = 255),
		vermillion = rgb(213,94,0,max = 255),
		bluishgreen = rgb(0,158,115,max = 255),
		reddishpurple = rgb(204,121,167,max = 255), 		skyblue = rgb(86,180,233,max = 255),
		blue = rgb(0,133,178,max = 255)

)
pdf(.pdf<- paste(pdf.name,".pdf",sep = ""),width = 15,height = 7.5,pointsize = 20)
# plot scores
library(gplots)
real.data 	<- summary.data
ref.data 	<- thresholds
score.data 	<- score
ms.col <- unlist(color.blind[c(5,6,1)])
nc.col <- unlist(color.blind[c(3,4,2)])
sum.scores <- c(sum(unlist(score.data[1:3])),sum(unlist(score.data)[4:6]))

if(any(sum.scores > 3)){temp.xlim <- c(0,max(sum.scores))}else{temp.xlim <- c(0,3)}
layout(matrix(	c(rep(1,5),2,
				rep(3,5),4,
				5,5,6,6,10,10,
				5,5,7,7,11,11,
				5,5,8,8,12,12,
				5,5,9,9,13,13
				),nrow = 6,ncol = 6,byrow = F),width = c(2.5,0.3,0.5,0.5,0.5,0.5))
par(mai = c(0,1,0.1,0))


try(plot.profile(data.i,F))

par(mai = c(1,2,0.1,0.1))
#assign("score.data",score.data,envir = .GlobalEnv)
#barplot2(cbind(score.data[1:3],rep(0,3)),names.arg = c("MS-score","nLC"),ylim = temp.xlim,col = ms.col,las = 2,ylab = "score")
#temp.pos <- barplot2(cbind(rep(0,3),score.data[4:6]),ylim = temp.xlim,col = nc.col,las = 2,axes = F,add = T)

round.spec <- function(x){ 
		x<- round(x*100)
		x[x > 100] <- 100
		x[is.na(x)] <- 100
		return(x)
		}
col.temp <- (colorRampPalette(grad.cols.vec)(100))

temp.pos <- barplot2(unlist(score.data),beside = T,col = c(col.temp[round.spec(unlist(score.data))]),horiz = T,names.arg = c("peptide ID","mass error","score","peak shape","peak width","Duplicates/Peptide IDs"),las = 2, plot.grid = T, grid.col = "grey")

#text(temp.pos,0,c("peptide ID","mass error","score","peak shape","elution time","dupl. peptide IDs"),las = 2,srt = 90,adj = c(1.1,1),xpd =NA,srt = 45)

abline(v=1,col = "grey",lwd = 5)
abline(v=1,col = "red")

par(mai = c(0,0,0.1,0))
#legend(max(temp.pos)+ temp.pos[1],max(temp.xlim),legend = c("peptide ID","mass error","score","peak shape","elution time","duplicated peptide IDs"),,fill = c(ms.col,nc.col),xpd = NA,xjust = 0,bty = "n",title = "Legend")



par(mai = c(0,0,0,0))
empty.plot <- function(){plot(1,type = "n",axes = F,xlab = "",ylab ="")}
#empty.col()
#empty.plot()

# plot real data
par(mai = c(0.2,1,0.5,0.2),bg = "lightblue")
plot.quans <- function(temp.plot,log2,xlab ="x",ylab = "y",ref.data,thresh.auto = T,bg ="lightblue",fg.col = 1 ,main = "",ylim = range(temp.plot)){
# if fg.vol is 0 
if(length(fg.col) == 0){fg.col <- 1}
	
if(log2){
	temp.plot <- log2(temp.plot)
}
temp.plot <- na.inf.fun(temp.plot)

plot(temp.plot,type = "n",xlim = c(1:2),axes = F,frame = T,xlab = xlab,ylab = ylab,lwd = 5,las = 2,fg = fg.col,bty = "n",ylim = ylim)
mtext(xlab,1,line = 1)
mtext(main,3,line = 0,cex = 0.6)

temp.lwd <- c(4,4,5,4,4)
temp.col <- c(4,2,1,2,4)
for(i in 1:5){
	lines(c(1.2, 1.8),rep(temp.plot[i],2),lwd = temp.lwd[i],col = temp.col[i])
}

if(log2){
#abline(h=sort(c(-ref.data,ref.data,0)),col = "black", lty = "dashed",lwd = 1)	
abline(h=sort(c(-ref.data,ref.data,0)),col = temp.col,lwd = 2,lty = "dotted")	
}
if(!log2){
abline(h=sort(c(ref.data)),col = temp.col,lty = "dotted",lwd = 2)	
	
}
axis(2,las = 2,fg = 1,lwd = 2)
box(lwd = 4,fg = fg.col)

}

plot.stat <- function(x,thresh, name.val,rev = F,bg = "lightblue",main = "2",col.dir = NULL){
	col.temp <- (colorRampPalette(c("blue","yellow","green"))(thresh*100))
	
		if(rev){
		col.temp <- (colorRampPalette(c("blue","yellow"))(thresh*100))
		col.temp <- rev(c(col.temp,rep("green",thresh*100)))	
		}

	x[is.na(x)] <- 0
	if(x > thresh){
		col.sel <- thresh
		x.range <- c(0,x+x*0.2)
	}else{
		col.sel <- x
		x.range <- c(0,thresh+thresh*0.2)

	}
	if(length(col.dir) == 0){col.dir <- col.temp[col.sel*100]}
	
	max(c(x,thresh))
	barplot2(x,las = 2,col =col.dir,ylab = name.val,ylim = x.range,bg = bg)
	abline(h=thresh,lty = "dashed",lwd = 2)
	mtext(main,3,cex =0.7,line = 0.3)
	
}

#plot.stat(summary.data$msms.count,thresholds$msms.count, name.val = "MSMS counts")
try(plot.stat(summary.data$quan.msms.min,thresholds$quan.msms.min, name.val = "Peptide ID/min",main = "MS"))

try(plot.stat(summary.data$quan.duplicates.msms, thresholds$quan.duplicates.msms, name.val = "Duplicates/Peptide IDs in %",rev = T,main = "MS",col.dir = col.grad(data.list$sc$quan.duplicates.msms,best = 1,bad = 0)$col)
)
col.grad(data.list$sc$quan.duplicates.msms,best = 1,bad = 0)
try(plot.stat(length(data.i.quant$ms.ms.scan.number[!is.na(data.i.quant$ms.ms.scan.number)])/(max(data.i.quant$ms.ms.scan.number,na.rm = T)-min(data.i.quant$ms.ms.scan.number,na.rm = T)*0.9)*100,60,name.val = "identification efficiency in %",main = "MS")
)
# col.vec for plot.quans 


par(mai = c(0.2,1,0.5,0.2))
init.i <- 500

plot(1,frame = F,axes=F,xlab = "",ylab = "",type = "n",ylim = c(0,init.i))
col.temp.2 <- (colorRampPalette(grad.cols.vec)(init.i))
for(col.i in 1:init.i){
	abline(h = col.i,col = col.temp.2[col.i])
}

mtext("Good",2,adj = 1,cex = 0.8)
mtext("Poor",2,adj = 0,cex = 0.8)
mtext("Color\nCode",3,cex = 0.8)




par(mai = c(0.2,1,0.5,0.2),bg = "lightblue")

try(plot.quans(summary.data$mass.error.cal,F,"","mass.error in ppm",c(thresholds$mass.error.cal,-thresholds$mass.error.cal,0),fg.col = col.temp[round.spec(score.data$mass.error)],main = "MS")
)
try(plot.quans(summary.data$score,F,"","score",c(0,50,100,150,400),fg.col = col.temp[round.spec(score.data$score)],main = "MS")
)

try(plot.quans(summary.data$ret.peak.shape,T,"","log2(peak.shape)",thresholds$ret.peak.shape,fg.col = col.temp[round.spec(score.data$peak.shape)],main = "nLC")
)
try(plot.quans(summary.data$ret.width,F,"","peak width in min",c(0,0.1,0.3,1,4),fg.col = col.temp[round.spec(score.data$ret.width)],main = "nLC",ylim = range(summary.data$ret.width[1:4]))
)



##plot.quans(summary.data$mass.error.cal,F,"mass.error","mass.error in ppm",thresholds$mass.error.cal)





graphics.off()
cat("\r",getwd())
if(open.doc){
	try(        try(system(paste("open ", .pdf), intern = TRUE, ignore.stderr = TRUE)))
}
}
#temp <- start.qc(data)