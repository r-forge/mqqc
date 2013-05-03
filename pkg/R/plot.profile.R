plot.profile <-
function(data.i,layout = T){	
	if(layout){
			layout(matrix(c(1,2,3,4),ncol = 2,nrow = 2),height = c(5,1.5),width = 	c(5,1))

	}



name.file <- unique(data.i$raw.file)#"Elutionprofile"
	######
	# Chromatography test
	######

	# modify intensity

	Ramp.col <- colorRampPalette(c("dodgerblue2","blue","darkblue","navyblue"))(101)
	intensity <- data.i$intensity
	intensity <- intensity/max(intensity,na.rm = T)*100
	Ramp.col <- Ramp.col[(round(intensity)+1)]
	
	intensity <- intensity/100*5
	intensity[is.na(intensity)] <- 0
	intensity <- sqrt(intensity)
	intensity <- round(intensity,1)*1.5
	
	
	par(mai=c(0,1,0.1,0))

	col.intensity <- grep("intensity",tolower(colnames(data.i)))
		print("hui")

	plot(data.i$retention.time,data.i$m.z,pch = 20,cex = intensity,col = 	Ramp.col,type = "n" ,ylab = "m/z",xlim = range(data.i$retention.time,na.rm = T),axes = F,ylim = range(data.i$m.z,na.rm = T),frame = T)
	axis(2)
	axis(1,labels = F)
	axis(4,xpd = NA,labels = F,padj = 0.5)
	uniqueSeq 	<- length(unique(data.i$sequence))
	intens 		<- quantile(data.i$intensity[!is.na(data.i$intensity)])
	intens 		<- paste("Top 50%:",intens[3],intens[5])
	name.file <- c(name.file, paste("unique peptides:",uniqueSeq),intens)
	grid(col = "darkgrey",lwd = 1.5)
	legend("topleft",legend = name.file,bg = "white",border = "transparent")
linePlot <- T
		for(i in 1:length(unique(Ramp.col))){
			mFac	<- diff(yrange<- range(data.i$m.z,na.rm = T))/250

			temp.i.sel 	<- Ramp.col == unique(Ramp.col)[i]
			x 			<- data.i$retention.time[temp.i.sel]
			y			<- data.i$m.z[temp.i.sel]
			Start	 	<- data.i$calibrated.retention.time.start[temp.i.sel]
			Finish		<- data.i$calibrated.retention.time.finish[temp.i.sel]
			
			.cols <- unique(Ramp.col)[i]
			print(.cols)
			if(linePlot){
				tempM <- cbind(Start ,Finish,x,y)
			apply(tempM,1,function(x){
				xPoly <- c(x[1],x[3],x[2],x[3],x[1] )
yPoly <- c(x[4],x[4]+mFac,x[4],x[4]-mFac,x[4])
				polygon(xPoly,yPoly,col = .cols,border = "white",lwd = 0.5)
				
			})
				
				
			}else{
				
				points(x,y,pch = 20,cex = intensity[temp.i.sel],col = Ramp.col[temp.i.sel] )

			}
		
		}
		
	par(mai=c(0.6,1,0,0))
	
	dens.crt <- class(try(temp <- density(data.i$retention.time)))
	if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}
	
		plot(temp$x,temp$y,main = "",axes = F,frame = T,xlim = range(data.i	$retention.time,na.rm = T),type = "l",xlab = "",ylab = "Density")
		mtext("time in min",1,line = 1.9,cex = 0.6)
		
		
		axis(2,xpd = NA,las = 2)
		axis(1)
	dens.crt <- class(try(	temp <-density(data.i$m.z)))
	if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}	
		grid(col = "darkgrey",lwd = 1.5)

	abline(v=median(data.i$retention.time),col = "red")
	quantiles <- quantile(data.i$retention.time)
	abline(v=	quantiles[c(2,4)],col = "blue")


	par(mai=c(0,0,0.1,0.1))
	
	plot(temp$y,temp$x,type = "l",axes = F,frame = T,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "")
	grid(col = "darkgrey",lwd = 1.5)
		axis(1,xpd = NA,las = 2)
		mtext("Density",1,line = 4,cex = 0.6)

	plot(1,type = "n",frame = F,axes = F)
}
#plot.profile(data.i)
