plot.SpeciesStatistics<- function(MSet,alpha = 0.01,plotstuff = T,adjustp = "BH",...){
  
try(data(NameCounts) ) 
  
cbPalette <- rev(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","tomato3"))
  
MSet <- MSet[!is.na(MSet[,3]),]  
AllSet <- grep("_all$",MSet$ID,invert = F)
if(exists("NameAlternative")){
  NameAlternative <- gsub("-","",NameAlternative,fixed = T)
  NewN <- NameAlternative[match(MSet[,1],names(FaNa))]
  MSet[!is.na(NewN),1] <- NewN[!is.na(NewN)]

  ListReport <- toupper(NameAlternative[!grepl("#",NameAlternative)])
  ListReport <- unique(ListReport)
  ListReport <- cbind(substr(ListReport,1,1),ListReport)
  ListReport[duplicated(ListReport[,1]),1] <- substr(ListReport[duplicated(ListReport[,1]),2],1,2)
  ListReportT <- unique(ListReport)
  ListReport <- ListReportT[order(ListReportT[,1]),]
  ListReport <- apply(ListReport,1,paste,collapse = ": ")
  
  
  TeN <- MSet[,1]
  for(i in 1:dim(ListReportT)[1]){
    TeN <- gsub(ListReportT[i,2],ListReportT[i,1],TeN)
  }
  TeN <- sapply(strsplit(as.character(TeN),"#"),function(x){paste(unique(substr(x,1,1)),collapse = "|")})
  MSet[,1] <- TeN
  MSet[AllSet,1] <- paste(MSet[AllSet,1] ,"All",sep = "_")
  # ListReport <- paste(ListReport,collapse = "\n")
}

MSetBackup <- MSet
MSet <- MSet[-AllSet,]
MSet <- MSet[order(MSet[,3],decreasing = T),]
AllCheckFracSeltemp <- MSet[,3]/sum(MSet[,3],na.rm = T)
names(AllCheckFracSeltemp) <- MSet[,1]
combi <- AllCheckFracSeltemp > 0.05

psfun <- MSet[combi,4]
names(AllCheckFracSeltemp)[psfun < alpha] <- paste(names(AllCheckFracSeltemp)[psfun < alpha],"*")

AllCheckFracSel <- c(AllCheckFracSeltemp[combi],sum(AllCheckFracSeltemp[!combi]))

names(AllCheckFracSel)[length(AllCheckFracSel)] <- "Other"

if(plotstuff){
  
  maisave <- par()$mai
  par(mai = rep(0.3,4))
  pie(AllCheckFracSel,col = colorRampPalette(cbPalette)(length(AllCheckFracSel)),border = "white",radius = 0.4,cex = 0.8)
  par(mai = maisave)
  MSet <- MSetBackup
  
  ps <- MSet[,4]
  ps <- p.adjust(ps,adjustp)
  
  ps <- -log10(ps)
  ps[is.infinite(ps) & ps > 0] <- max(ps[!is.infinite(ps)])*1.1
  jic <<-log10(MSet[,3])
  # rat <- MSet[,3]/MSet[,2]
  rat <- MSet[,3]#log2(rat)
  #rat[is.infinite(rat)] <- max(rat[!is.infinite(rat)],na.rm = T)
  cuti <- quantile(rat,probs = c(0.75),na.rm = T)
  rat <- rat>cuti
  rat[is.na(rat)] <- FALSE
  jic[is.infinite(jic)] <- NA
  texCex <- rep(0.1,length(jic))
  # print(alpha)
  ps <<- ps
  texCex[ps > -log10(alpha)&rat] <- 0.5
  
  jicord <- order(jic)
  jic <- jic[jicord]
  ps <- ps[jicord]
  MSet <- MSet[jicord,]
  texCex <- texCex[jicord]
  plot(jic ,(ps),type = "n",pch = 20,frame = F,xlab = "log10 Peptide Count",ylab = "-log10 adjusted p",xlim = range(jic,na.rm = T)+c(0,diff(range(jic,na.rm = T))*0.5),...)
  grid()
  cols <- rep("darkgrey",length(jic))
  cols[jic > log10(cuti) & ps > -log10(alpha)] <- "orange"
  points(jic ,(ps),col = cols,...,pch = 20)
  text(jic,jitter(ps,factor = diff(range(jic,na.rm = T))*0.05),MSet[,1],xpd = NA,pos = c(1,2,3,4),adj = c(0.5,1,0.5,0),cex = texCex,col = "grey20")
  
  abline(v = log10(cuti),lty = "dotted",h =-log10(alpha))
  
  legend("bottomright",ListReport,bg = "transparent",box.col = "transparent",cex = 0.8)
}
}

   # plot(qc.prepare.data$SpecStat)