SpeciesStatistic <- 
function(x,plotstuff = F,alpha = 0.01){
try(data(NameCounts))
NameAlternative <- gsub("-","",NameAlternative,fixed = T)
MN <- strsplitslot(x,1,"_",fixed = T)
NewN <- NameAlternative[M <- match(MN,names(FaNa))]
NewN[is.na(M)] <- "unknown"
hmpf <- match(names(FaNa),MN)

# Create Secind Comparisson set
FaNa2 <- FaNa
names(FaNa2) <- NameAlternative[hmpf]
FaNa2 <- FaNa2[!is.na(names(FaNa2))]
rt <-lapply(1:length(FaNa2),function(x){
  hui<- unlist(strsplit(names(FaNa2)[x],"#",fixed = T))
  xi <- rep(FaNa2[x],length(hui))
  names(xi) <- hui
  return(xi)
  }
  )
rt <- unlist(rt)

FaNa2 <- aggregate(rt,list(names(rt)),sum)
FaNa2t <- FaNa2[,2]
names(FaNa2t) <- FaNa2[,1]
# FaNaM <- match(MN,names(FaNa2))
# names()



cbPalette <- rev(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","tomato3"))
SpecCheck <-strsplitslot(x,1,"_")
AllCheck <- table(SpecCheck)
AllCheck2 <- table(unlist(strsplit(NewN,"#")))
if(length(AllCheck) < 500){
  
AllCheck <- sort(AllCheck,decreasing = T)
AllCheckFrac <- AllCheck/sum(AllCheck)
AllCheckSel <-  AllCheckFrac >0.05
AllCheckFracSel <- c(AllCheckFrac[AllCheckSel],sum(AllCheckFrac[!AllCheckSel]))
AllCHeckNum <- c(AllCheck[AllCheckSel],sum(AllCheck[!AllCheckSel]))

try(load(GenericDBPathCounts),silent = T)
if(exists("FaNa")){
  cat("Loading FastaCounts")
  if(any(names(FaNa)%in%names(AllCheck))){
    
MSet <- merge(as.matrix(FaNa),as.matrix(AllCheck),by = 0,all = T)  
MSet2 <- merge(as.matrix(FaNa2t),as.matrix(AllCheck2),by = 0,all = T)
MSet2 <- MSet2[!is.na(MSet2[,3]),]
MSet2[is.na(MSet2)] <- 0
ps <- apply(MSet,1,function(xt){xt <- as.numeric(as.character(xt))
;phyper(xt[3],xt[2],sum(MSet[,2],na.rm = T)-xt[3],length(SpecCheck),lower.tail = F)  })
ps2 <- apply(MSet2,1,function(xt){xt <- as.numeric(as.character(xt))
;phyper(xt[3],xt[2],sum(MSet2[,2],na.rm = T)-xt[3],length(SpecCheck),lower.tail = F)  })
ps <- p.adjust(ps,"BH") 
ps2 <- p.adjust(ps2,"BH")
ps[is.na(ps)] <- 1
MSet <- cbind(MSet,ps)
MSet2$Row.names <- paste(MSet2$Row.names,"all",sep = "_")
MSet2 <- cbind(MSet2,ps2)
#MSet2[,1]
colnames(MSet) <- c("ID","FastaCount","PepCount","Enrichment.p.BH")
colnames(MSet2) <- c("ID","FastaCount","PepCount","Enrichment.p.BH")
MSet3 <- rbind(MSet,MSet2)
MSet <- MSet3
MFUN <- match(names(AllCheckFracSel),MSet$ID)
psfun <- ps[MFUN] 
psfun[is.na(psfun)] <- 1
names(AllCheckFracSel)[length(AllCheckFracSel)] <- "Other"
names(AllCheckFracSel) <- paste(names(AllCheckFracSel),AllCHeckNum)
names(AllCheckFracSel)[psfun < alpha] <- paste(names(AllCheckFracSel)[psfun < alpha],"*")
if(plotstuff){
par(mfrow = c(1,2))
pie(AllCheckFracSel,col = colorRampPalette(cbPalette)(length(AllCheckFracSel)),border = "white")
ps <- -log10(MSet$Enrichment.p.BH)
ps[is.infinite(ps) & ps > 0] <- max(ps[!is.infinite(ps)])*1.1
plot(jic <-log10(MSet[,3]),(ps),type = "p",pch = 20,frame = F,xlab = "log10 Pep Count",ylab = "-log10 p")
grid()
jic[is.infinite(jic)]<- max(jic[!is.infinite(jic)])*1.1
text(log10(MSet[,3]),jitter(ps,factor = diff(range(jic,na.rm = T))*0.05),MSet[,1],xpd = NA,pos = c(1,2,3,4))
}
}else{return(NA)}
}else{
  MSet = NA
}
class(MSet) <- c("SpeciesStatistics","data.frame")
return(MSet)

}else{return(NA)}
}
# plsi <- SpeciesStatistic(DataEvidence$Leading.Razor.Protein,T)
# # tryError1 <- class(try(qc.prepare.data <<- qc.prepare(Data = temp.DataEvidence, SpeciesTable = SpeciesTable,placeholder = placeholder,templateFasta = RESettings$REpar,path = .path,filename = i, BSAID = BSAID,RESettings = RESettings,Peptides = Peptides, AllPeptides =tempAllPeptides,MSMS = tempMSMS)))
# # class(qc.prepare.data$SpecStat)
# 
# plot(plsi)
