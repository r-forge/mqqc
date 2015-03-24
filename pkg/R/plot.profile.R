plot.profile <-
function(data.i,layout = T,linePlot =F,BSACheck = F, plot.legend = T){	
AllDensCol <- colors()[235]
IntQuanDensCol <- colors()[57]	#632
plotData <- list()
if(layout){
			layout(matrix(c(1,2,3,4),ncol = 2,nrow = 2),height = c(5,1.5),width = 	c(5,1))

	}
colnames(data.i) <- tolower(colnames(data.i))
if(BSACheck){
	
	BSAEVI <- data.i[BSAgrep <- grep("P02769",data.i$proteins),]
}

name.file <- unique(data.i$raw.file)#"Elutionprofile"
	######
	# Chromatography test
	######

	# modify intensity

	Ramp.col <- colorRampPalette(c("dodgerblue2","blue","darkblue","navyblue"))(101)
	BSA.col <- colorRampPalette(c("orange","red","darkred","violet"))(101)

	intensity <- data.i$intensity
	intensity <- intensity/max(intensity,na.rm = T)*100
	intensity[is.na(intensity)] <- 0
	if(BSACheck){
		Ramp.col.BSA <- BSA.col[(round(intensity)+1)[BSAgrep]]
	}
		Ramp.col <- Ramp.col[(round(intensity,1)+1)]
	if(BSACheck){
		Ramp.col[BSAgrep] <- Ramp.col.BSA
	}
	
	
	intensity <- intensity/100*5
	intensity[is.na(intensity)] <- 0
	intensity <- sqrt(intensity)
	intensity <- round(intensity,1)*1.5
	
	Ramp.col[is.na(Ramp.col)] <- "grey"
	par(mai=c(0,1,0.1,0))

	col.intensity <- grep("intensity",tolower(colnames(data.i)))
	plotData$profile <- cbind(data.i$retention.time,data.i$m.z)
	plot(data.i$retention.time,data.i$m.z,pch = 20,cex = intensity,col = 	Ramp.col,type = "n" ,ylab = "m/z",xlim = range(data.i$retention.time,na.rm = T),axes = F,ylim = range(data.i$m.z,na.rm = T),frame = F)
	abline(v=median(data.i$retention.time),col = "#00000060",lwd = 1)
	quantiles <- quantile(data.i$retention.time)
	abline(v=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)
	abline(h=median(data.i$m.z),col = "#00000060",lwd = 1)
	quantiles <- quantile(data.i$m.z)
	abline(h=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)

	
	
	axis(2)
	axis(1,labels = F)
	axis(4,xpd = NA,labels = F,padj = 0.5)
	uniqueSeq 	<- length(unique(data.i$sequence))
	intens 		<- quantile(data.i$intensity[!is.na(data.i$intensity)])
	intens 		<- paste("Q 0.5-1 Intensities:",format(intens[3],digits = 3, scientific = T),format(intens[5],digits =3, scientific = T))
	
  if(BSACheck){
    name.file <- c(as.character(name.file), paste("BSA peptides:",length(grep("MSMS",BSAEVI$type)),", all peptides:",length(grep("MSMS",data.i$type))),intens)
    
  }else{
    name.file <- c(as.character(name.file), paste("proteins:",length(unique(data.i$proteins))),paste("peptides, all:",length(grep("MSMS",data.i$type)),", unique:",uniqueSeq),intens)
  }
	grid(col = "darkgrey",lwd = 1.5)
	
	if(plot.legend){
	legend("topleft",legend = name.file,bg = "white",box.col = "transparent")
	}
	tempBorder <- densCols(data.i$retention.time,data.i$m.z,colramp = colorRampPalette(c("white","darkgrey")))
	
		for(i in 1:length(unique(Ramp.col))){
			mFac	<- diff(yrange<- range(data.i$m.z,na.rm = T))/250

			temp.i.sel 	<- Ramp.col == unique(Ramp.col)[i]
			x 			<- data.i$retention.time[temp.i.sel]
			y			<- data.i$m.z[temp.i.sel]
			Start	 	<- data.i$calibrated.retention.time.start[temp.i.sel]
			Finish		<- data.i$calibrated.retention.time.finish[temp.i.sel]
			tempBorderi <- tempBorder[temp.i.sel]
			
			x <<- x
			y <<- y
			if(any(length(Start)== 0|length(Finish) == 0)){
				Start <- y -1
				Finish <- y +1
				.border <- "grey"
			}else{
				.border <- "lightgrey"
			}	
			.border = tempBorderi
			.cols <- unique(Ramp.col)[i]
			if(linePlot){
				tempM <- cbind(Start ,Finish,x,y)
			itm <<- 1
			apply(tempM,1,function(x){
				xPoly <- c(x[1],x[3],x[2],x[3],x[1] )
yPoly <- c(x[4],x[4]+mFac,x[4],x[4]-mFac,x[4])
				polygon(xPoly,yPoly,col = .cols,border = .border[itm],lwd = 0.3)
							itm <<- itm+1
 
				
			})
				
				
			}else{
				
				points(x,y,pch = 20,cex = intensity[temp.i.sel],col = Ramp.col[temp.i.sel] )

			}
		
		}
		
		if(BSACheck){
			BSAx <- data.i$retention.time[BSAgrep]
			BSAy <- data.i$m.z[BSAgrep]
			BSAmz <- round(BSAy,1)
			text(BSAx,BSAy,BSAmz,pos = 3,cex = 0.4)
		}
		
	par(mai=c(0.6,1,0,0))
	
	dens.crt <- class(try(temp <- density(data.i$retention.time)))
	if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}
	plotData$retentionTime <- cbind(temp$x,temp$y)
		plot(temp$x,temp$y,main = "",axes = F,frame = F,xlim = range(data.i$retention.time,na.rm = T),type = "n",xlab = "",ylab = "",lwd = 1)
		grid(col = "darkgrey",lwd = 1.5)
	quantiles <- quantile(data.i$retention.time)
	abline(v=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)
	
		#points(temp$x,temp$y,main = "",axes = F,frame = F,xlim = range(data.i$retention.time,na.rm = T),type = "l",xlab = "",ylab = "",lwd = 2,col = "grey30")
	polygon(temp$x,temp$y,col = AllDensCol,border = AllDensCol)
	Inner <- temp$x <= quantiles[4] & temp$x >= quantiles[2]
	pX <- temp$x[Inner]
	pY <- temp$y[Inner]
	polygon(c(min(pX),pX,max(pX)),c(0,pY,0),col = IntQuanDensCol,border = IntQuanDensCol)
	abline(v=median(data.i$retention.time),col = "white",lwd = 2)

		
		mtext("time in min",1,line = 1.9,cex = 0.6)
		
		
		#axis(2,xpd = NA,las = 2)
		axis(1)

	if(BSACheck){
		BSAdensFactor <- length(BSAgrep)/dim(data.i)[1]
		
		
			dens.crt <- class(try(temp <- density(data.i$retention.time[BSAgrep])))
	if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}
		plotData$BSAretentionTime <- cbind(temp$x,temp$y)

		points(temp$x,temp$y* BSAdensFactor,type = "l",col = "white",lwd = 4)

		points(temp$x,temp$y* BSAdensFactor,main = "",axes = F,frame = F,xlim = range(data.i$retention.time[BSAgrep],na.rm = T),type = "l",xlab = "",ylab = "Density",col = "orange",lwd = 3)
		mtext("time in min",1,line = 1.9,cex = 0.6)
				legend("topright",legend = c("all","BSA"),col = c(AllDensCol,"orange"),lwd = 3,bty = "n")

	}
	dens.crt <- class(try(	temp <-density(data.i$m.z)))
	
	if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}	




	par(mai=c(0,0,0.1,0.1))
	plotData$mz <- cbind(temp$x,temp$y)

	plot(temp$y,temp$x,type = "n",axes = F,frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",lwd = 1,col = "white")
	grid(col = "darkgrey",lwd = 1.5)
	
	quantiles <- quantile(data.i$m.z)
	#abline(h=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)

	#points(temp$y,temp$x,type = "l",axes = F,frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",lwd = 1,col = "grey30")
	polygon(temp$y,temp$x,col = AllDensCol,border = AllDensCol)
Inner <- temp$x <= quantiles[4] & temp$x >= quantiles[2]
	pX <- temp$x[Inner]
	pY <- temp$y[Inner]
	polygon(c(0,pY,0),c(min(pX),pX,max(pX)),col = IntQuanDensCol,border = IntQuanDensCol)
	abline(h=median(data.i$m.z),col = "white",lwd = 2)

		#axis(1,xpd = NA,las = 2)
		#mtext("Density",1,line = 4,cex = 0.6)
	
	if(BSACheck){
	dens.crt <- class(try(	temp <-density(data.i$m.z[BSAgrep])))
		if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}	
	par(mai=c(0,0,0.1,0.1))
	points(temp$y*BSAdensFactor,temp$x,type = "l",axes = F,frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",col = "white",lwd = 4)

	points(temp$y*BSAdensFactor,temp$x,type = "l",axes = F,frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",col = "orange",lwd = 3)

		legend("topright",legend = c("all","BSA"),col = c(AllDensCol,"orange"),lwd = 3,bty = "n")
	}
		plot(1,type = "n",frame = F,axes = F)
	plotData$name.file <- name.file
	return(plotData)
	
}
#tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))
#par(mfrow = c(2,2))
#try(plotData<- plot.profile(data.i,F,F,BSACheck= T))
#plot.profile(Data)