plot.profile <-
function(data.i,layout = T,linePlot =F,BSACheck = F, plot.legend = T,ProtFDR = 0.01,funfun = NULL,msScans = NULL,msmsScans = NULL){	
AllDensCol <- colors()[235]
IntQuanDensCol <- "lightblue"#colors()[57]	#632
plotData <- list()
if(layout){
			layout(matrix(c(1,2,3,4),ncol = 2,nrow = 2),height = c(5,1.5),width = 	c(5,1))

	}
colnames(data.i) <- tolower(colnames(data.i))
data.i$m.z[data.i$Reverse == "+"] <- NA
if(BSACheck){
	
	BSAEVI <- data.i[BSAgrep <- grep("P02769",data.i$proteins),]
}

name.file <- unique(data.i$raw.file)#"Elutionprofile"
	######
	# Chromatography test
	######

	# modify intensity

	Ramp.col <- colorRampPalette(c("dodgerblue2","blue","darkblue","navyblue"))(101)
	BSA.col <- colorRampPalette(c("#fca502","red","darkred","violet"))(101)
	CON.col <- colorRampPalette(c("#fca502","red","darkred","violet"))(101)
	
	intensity <- data.i$intensity
	intensity <- intensity/max(intensity,na.rm = T)*100
	intensity[is.na(intensity)] <- 0
	if(BSACheck){
		Ramp.col.BSA <- BSA.col[(round(intensity)+1)[BSAgrep]]
	}
		Ramp.col <- Ramp.col[(round(intensity,1)+1)]
		Ramp.col[data.i$potential.contaminant == "+"] <- "#fca502"
	if(BSACheck){
		Ramp.col[BSAgrep] <- Ramp.col.BSA
	}else{
	  Ramp.col[data.i$potential.contaminant == "+"] <- CON.col[(round(intensity)+1)[data.i$potential.contaminant == "+"]]
	  
	}
	
	
	intensity <- intensity/100*5
	intensity[is.na(intensity)] <- 0
	intensity <- sqrt(intensity)
	intensity <- round(intensity,1)*1.5
	
	Ramp.col[is.na(Ramp.col)] <- "grey"
	par(mai=c(0,1,0.1,0))

	col.intensity <- grep("intensity",tolower(colnames(data.i)))
	plotData$profile <- cbind(data.i$retention.time,data.i$m.z)
	plot(data.i$retention.time,data.i$m.z,pch = 20,cex = intensity,col = 	Ramp.col,type = "n" ,ylab = "m/z",xlim = xl <- range(data.i$retention.time,na.rm = T),axes = F,ylim = range(data.i$m.z,na.rm = T),frame = F)
	abline(v=median(data.i$retention.time),col = "#00000060",lwd = 1)
	quantiles <- quantile(data.i$retention.time)
	abline(v=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)
	abline(h=median(data.i$m.z),col = "#00000060",lwd = 1)
	quantiles <- quantile(data.i$m.z)
	abline(h=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)

	# mtext("MS1\nintensity",1,cex = 0.6,las = 2)
	

	axis(2,tck = -0.015)
	axis(1,labels = F,tck = -0.015)
	uniqueSeq 	<- length(unique(data.i$sequence))
	intens 		<- quantile(as.numeric(data.i$intensity[!is.na(data.i$intensity)]))
	intens 		<- paste("Intensities Q 0.5-1 :",format(intens[3],digits = 3, scientific = T),format(intens[5],digits =3, scientific = T))
	
  if(BSACheck){
    name.file <- c(as.character(name.file), paste("BSA peptides:",length(grep("MSMS",BSAEVI$type)),", all peptides:",length(grep("MSMS",data.i$type))),intens)
    
  }else{
    sel <- data.i$q.value < 0.01
    if(length(sel) == 0){
      sel <- rep(T,dim(data.i)[1])
    }
    pepPu<- aggregate(data.i$pep,list(data.i$leading.razor.protein,data.i$sequence),min,na.rm = T)
    pepPr <- aggregate(pepPu$x,list(pepPu$Group.1),prod)
    pepPr <- pepPr[order(pepPr[,2]),]
    Rev <- grep("^REV",pepPr[,1])
    RevP <- (1:length(Rev))/Rev
    hum <- Rev-c(0,Rev[-length(Rev)])
    # hum[length(hum)] <- 0
    fu <- unlist(apply(cbind(hum,RevP),1,function(x){return(rep(x[2],x[1]))}))
    PL <- length(fu[fu < ProtFDR])
    name.file <- c(as.character(name.file), paste("proteins:",PL),paste("peptides, all:",length(grep("MSMS",data.i$type)),", unique:",uniqueSeq),intens)
  }
	grid(col = "darkgrey",lwd = 1.5)
	
	if(plot.legend){
	legend("topleft",legend = name.file,bg = "white",box.col = "transparent")
	}
	tempBorder <- densCols(data.i$retention.time,data.i$m.z,colramp = colorRampPalette(c("white","darkgrey")))
	# tempBorder[data.i$Potential.contaminant == "+"] <- "#fca502"
		for(i in 1:length(unique(Ramp.col))){
			mFac	<- diff(yrange<- range(data.i$m.z,na.rm = T))/250

			temp.i.sel 	<- Ramp.col == unique(Ramp.col)[i]
			x 			<- data.i$retention.time[temp.i.sel]
			y			<- data.i$m.z[temp.i.sel]
			Start	 	<- data.i$calibrated.retention.time.start[temp.i.sel]
			Finish		<- data.i$calibrated.retention.time.finish[temp.i.sel]
			tempBorderi <- tempBorder[temp.i.sel]
			
			# x <<- x
			# y <<- y
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
  try(	polygon(xPoly,yPoly,col = .cols,border = .border[itm],lwd = 0.3))
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
	if(length(funfun) == 0&exists("msScans")){
	  
	  par(mai=c(0.6,1,0.1,0))
	}else{
	  par(mai=c(0.1,1,0.1,0))
	  
	}
  data.i$potential.contaminant[is.na(data.i$potential.contaminant)] <- ""
  dens.conA  <- createWindows(data.i$retention.time[data.i$potential.contaminant == "+"])$full
  dens.crtA  <- createWindows(data.i$retention.time[data.i$potential.contaminant != "+"])$full
  dens.crtA0 <- dens.crtA[dens.crtA !=0]
  nofa <- max(medwin(as.numeric(names(dens.crtA0)),dens.crtA0/max(dens.crtA0))$y)
  densityRescale <- T
  if(!densityRescale){
    nofa <- 1
  }
  
	dens.crt <- class(try(temp <- density(DAT <- data.i$retention.time,na.rm = T)))
	tempCON <- NULL
  dens.con <- class(try(tempCON <- density(CON <- data.i$retention.time[data.i$potential.contaminant == "+"],na.rm = T)))
  temp$y <- temp$y/max(temp$y,na.rm = T)*nofa
  tempCON$y <- tempCON$y/max(tempCON$y,na.rm = T)
 
try(ConFAC <- length(CON)/length(DAT))
try(tempCON$y <- tempCON$y * ConFAC)

if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}
	  plotData$retentionTime <- cbind(temp$x,temp$y)
	  
		plot(temp$x,temp$y,main = "",axes = F,frame = F,xlim = range(data.i$retention.time,na.rm = T),type = "n",xlab = "",ylab = "",lwd = 1,ylim = c(0,1))
		# grid(col = "darkgrey",lwd = 1.5)
		abline(v = pretty(temp$x),col = "darkgrey",lwd = 1.5,lty = "dotted")
		
	quantiles <- quantile(data.i$retention.time)
	abline(v=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)
	
		#points(temp$x,temp$y,main = "",axes = F,frame = F,xlim = range(data.i$retention.time,na.rm = T),type = "l",xlab = "",ylab = "",lwd = 2,col = "grey30")
	polygon(temp$x,temp$y,col = AllDensCol,border = AllDensCol)
	Inner <- temp$x <= quantiles[4] & temp$x >= quantiles[2]
	pX <- temp$x[Inner]
	pY <- temp$y[Inner]
	polygon(c(min(pX),pX,max(pX)),c(0,pY,0),col = IntQuanDensCol,border = IntQuanDensCol)
	abline(v=median(data.i$retention.time),col = "white",lwd = 2)

	if(length(funfun) == 0){
	  mtext("time in min",1,line = 1.9,cex = 0.6)
	  
	  
	  #axis(2,xpd = NA,las = 2)
	  axis(1)
	}
		
	mtext("identified\nMS2\ncounts",2,cex = 0.6,las = 2)
	
	
	if(!(length(funfun) > 0 & exists("msScans"))){
	  mtext("time in min",1,line = 1.9,cex = 0.6)
	  
	#axis(2,xpd = NA,las = 2)
	axis(1)
	abline(v=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)
	}
	if(BSACheck){
		BSAdensFactor <- length(BSAgrep)/dim(data.i)[1]
		
		
			dens.crt <- class(try(temp <- density(data.i$retention.time[BSAgrep])))
			
	if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}
		plotData$BSAretentionTime <- cbind(temp$x,temp$y)

		# points(temp$x,temp$y* BSAdensFactor,type = "l",col = "white",lwd = 4)

		# points(temp$x,temp$y* BSAdensFactor,main = "",xlim = range(data.i$retention.time[BSAgrep],na.rm = T),type = "l",xlab = "",ylab = "Density",col = "#fca502",lwd = 3)
		# mtext("time in min",1,line = 1.9,cex = 0.6)
		# try(legend("topright",legend = c("all","BSA"),col = c(AllDensCol,"#fca502"),bg = c(AllDensCol,"#fca502"),pch = 17,bty = "n",cex = 0.5))
		plotPOL(names(dens.conA),dens.conA/max(dens.crtA),col = "#fca50280",type = "l",border = "transparent")
				
	}else{
    # print(ConFAC)
    # ConFAC <<- ConFAC
    if(log2(ConFAC) > -4){
	  # try(points(tempCON$x,tempCON$y,type = "l",col = "#fca502",lwd = 2,lty = "dotted"))
      plotPOL(names(dens.conA),dens.conA/max(dens.crtA),col = "#fca50280",type = "l",border = "transparent")
      # try(legend("topright",legend = c("all","contaminants"),col = c(AllDensCol,"#fca502"),bg = c(AllDensCol,"#fca502"),pch = 17,bty = "n",cex = 0.5))
      
    
	  #points(tempCON$x,tempCON$y,main = "",axes = F,frame = F,na.rm = T,type = "l",xlab = "",ylab = "Density",col = "dark#fca502",lwd = 3)
	  # try(legend("topright",legend = c("contaminants"),col = c("grey"),lty = "dotted",lwd = 3,bty = "n"))
    }
    
	}
	plotPOL(names(dens.crtA),dens.crtA/max(dens.crtA),col = "#99999990",type = "l",border = "transparent")
	
	
	
	dens.mzCA  <- (createWindows(data.i$m.z[data.i$potential.contaminant == "+"])$full)
	dens.mzA  <- (createWindows(data.i$m.z[data.i$potential.contaminant != "+"])$full)
	
	dens.crt <- class(try(	temp <-density(data.i$m.z)))
	dens.mzA0 <- dens.mzA[dens.mzA !=0]
	nofa <- max(medwin(as.numeric(names(dens.mzA0)),dens.mzA0/max(dens.mzA0))$y)
	densityRescale <- T
	if(!densityRescale){
	  nofa <- 1
	}
	temp$y <- temp$y/max(temp$y)*nofa
	
	
	if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}	

#	
	par(mai=c(0,0,0.1,0.1))
	plotData$mz <- cbind(temp$x,temp$y)

	plot(temp$y,temp$x,type = "n",axes = F,frame = F,ylim = range(c(data.i$m.z),na.rm = T),xlim = c(0,1),xlab = "Density",ylab = "",lwd = 1,col = "white")
	# grid(col = "darkgrey",lwd = 1.5)
	abline(h = pretty(temp$y),col = "darkgrey",lwd = 1.5,lty = "dotted")
	quantiles <- quantile(data.i$m.z)
	#abline(h=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)

	#points(temp$y,temp$x,type = "l",axes = F,frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",lwd = 1,col = "grey30")
	polygon(temp$y,temp$x,col = AllDensCol,border = AllDensCol)
Inner <- temp$x <= quantiles[4] & temp$x >= quantiles[2]
	pX <- temp$x[Inner]
	pY <- temp$y[Inner]
	try(polygon(c(0,pY,0),c(min(pX),pX,max(pX)),col = IntQuanDensCol,border = IntQuanDensCol))
	abline(h=median(data.i$m.z),col = "white",lwd = 2)

		#axis(1,xpd = NA,las = 2)
		#mtext("Density",1,line = 4,cex = 0.6)

	if(BSACheck){
	dens.crt <- class(try(	temp <-density(data.i$m.z[BSAgrep])))
		if(dens.crt  == "try-error"){temp <- list(x=0,y=0)}	
	# par(mai=c(0,0,0.1,0.1))
  # try(  points(temp$y*BSAdensFactor,temp$x,type = "l",frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",col = "white",lwd = 4))
  # try(  points(temp$y*BSAdensFactor,temp$x,type = "l",frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",col = "#fca502",lwd = 3))
		# try(legend("topright",legend = c("all","BSA"),col = c(AllDensCol,"#fca502"),bg = c(AllDensCol,"#fca502"),pch = 17,bty = "n",cex = 0.5))
	}else{
	  # try(legend("topright",legend = c("all","con-\ntami-\nnants"),col = c(AllDensCol,"#fca502"),bg = c(AllDensCol,"#fca502"),pch = 17,bty = "n",cex = 0.5))

	}
	
	
	if(log2(ConFAC) > -4){
	  # try(points(tempCON$x,tempCON$y,type = "l",col = "#fca502",lwd = 2,lty = "dotted"))
	  try(plotPOL(a <- unfactor(dens.mzCA/max(c(dens.mzA,dens.mzCA))),b <- as.numeric(names(dens.mzCA)),col = "#fca50280",type = "p",border = "transparent"))
	  # points(dens.mzA/max(c(dens.mzA,dens.mzCA)),as.numeric(names(dens.mzA)),col = "#99999990",type = "h",border = "transparent")
	  
	  
	  #points(tempCON$x,tempCON$y,main = "",axes = F,frame = F,na.rm = T,type = "l",xlab = "",ylab = "Density",col = "dark#fca502",lwd = 3)
	  # try(legend("topright",legend = c("contaminants"),col = c("grey"),lty = "dotted",lwd = 3,bty = "n"))
	}
	
	try(plotPOL(a <- unfactor(dens.mzA/max(c(dens.mzA,dens.mzCA))),b <- as.numeric(names(dens.mzA)),col = "#99999990",type = "p",border = "transparent"))
	
	# axis(2,xpd = NA,labels = F,padj = 0.5,tck = 0.04)
	
	
	
	par(mai = c(0,0,0,0))
		plot(1,type = "n",frame = F,axes = F)
		
		if(BSACheck){
		  # try(  points(temp$y*BSAdensFactor,temp$x,type = "l",frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",col = "white",lwd = 4))
		  # try(  points(temp$y*BSAdensFactor,temp$x,type = "l",frame = F,ylim = range(data.i$m.z,na.rm = T),xlab = "Density",ylab = "",col = "#fca502",lwd = 3))
		  try(legend("topleft",legend = c("other","BSA"),title = "Peptides",col = c(AllDensCol,"#fca502"),bg = c(AllDensCol,"#fca502"),pch = 17,bty = "n",cex = 0.5))
		}else{
		  try(legend("topleft",legend = c("other","contaminants"),title = "Peptides",col = c(AllDensCol,"#fca502"),bg = c(AllDensCol,"#fca502"),pch = 17,bty = "n",cex = 0.5))
		  
		}
		
		
	plotData$name.file <- name.file
	if(length(funfun) > 0 & exists("msScans")){
	  
	par(mai=c(0.8,1,0,0))
	
	
	try(funfun(msScans = msScans,msmsScans = msmsScans,xl = xl))

	  mtext("time in min",1,line = 1.9,cex = 0.6)
	  
	  
	  #axis(2,xpd = NA,las = 2)
	  axis(1)
	  abline(v=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)
	  
	}else{
	  mtext("time in min",1,line = 1.9,cex = 0.6)
	  
	  
	  #axis(2,xpd = NA,las = 2)
	  axis(1)
	  abline(v=	quantiles[c(2,4)],col = IntQuanDensCol,lty = c("dashed","solid"),lwd = 1)
	  
	  empty.plot()
	}
return(plotData)
	
}
# tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,msScans = msScans,msmsScans= msmsScans,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))

# plotMsFun <- NULL

# tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,msScans = NULL,msmsScans= NULL,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))

# plot.profile(data.i,linePlot = T)
#try(plotData<- plot.profile(data.i,T,dots,BSACheck= BSACheck,plot.legend = F))

#plot.profile(temp.DataEvidence)
#tryError2 <- class(try(TotalScoreRes  <- plot.scores(data.i = temp.DataEvidence,data.list = qc.prepare.data,pdf.name = i, open.doc = T,pdfOut = pdfOut, BSACheck = BSACheck)))
#par(mfrow = c(2,2))
#try(plotData<- plot.profile(data.i,F,F,BSACheck= T))
#plot.profile(Data)