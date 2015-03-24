ThreadControl <- 
function(folder,maxHours = 2,tableTC =  "_RmqqcFile_ThreadControl"){
TL1 <- system("tasklist",intern = T)
wd <- getwd()	
setwd(folder)

TL <- grep("MaxQuantCmd.exe",TL1,ignore.case = T)
#TL <- grep("cpqteam.exe",TL1,ignore.case = T)
  if(length(TL)==0){
    TL <- grep(pattern <- "MaxQua",TL1, ignore.case = T)
  }
  
  
  
 if(length(TL) > 0){
TLobserve <- TL1[TL]
#TLobserve2 <- TLobserve
#TLobserve2 <- gsub("6984","2948",TLobserve2)
#TLobserve <- c(TLobserve2, TLobserve) 

temp <- lapply(TLobserve,function(x){
	tempX <- strsplit(x," ")
	tempX <- unlist(tempX)
	tempX  <-(tempX[tempX != ""])

	return(tempX) 
})

TC <- list.files(folder,pattern = tableTC )

Table.Dat <- sapply(temp,function(x){
	return((c(x[1],x[2],as.numeric(Sys.time()))))
	
	
})
Table.Dat <- t(Table.Dat)

if(length(TC) == 0){
	if(is.vector(Table.Dat)){Table.Dat <- t(as.matrix(Table.Dat))}
	rownames(Table.Dat) <- NULL
	write.csv(Table.Dat, tableTC, row.names = F)
}else{
	
	
}

ControlTC <- 	read.csv(tableTC)	
if(all(dim(ControlTC) == c(3,1))){ControlTC <- t(ControlTC)}
Delete 	<- setdiff(ControlTC[,2], Table.Dat[,2])
Add 		<- setdiff(Table.Dat[,2],ControlTC[,2] )
if(length(Add) > 0){
	
	if(is.vector(Table.Dat)){Table.Dat <- t(as.matrix(Table.Dat))}
	write.csv(Table.Dat[Table.Dat[,2] == Add], tableTC, row.names = F,append = T)

}
ControlTC <- 	read.csv(tableTC)	
if(all(dim(ControlTC) == c(3,1))){ControlTC <- t(ControlTC)}

if(length(Delete) > 0){
	for(i in Delete){
		ControlTC <- ControlTC[ControlTC[,2] !=i ,]
	}
	write.csv(ControlTC,tableTC,row.names = F)
	
}
ControlTC <- 	read.csv(tableTC)	
if(all(dim(ControlTC) == c(3,1))){ControlTC <- t(ControlTC)}

######
# Force Quit if Program runs longer than 3 hours
######
DiffTime<- as.numeric(Sys.time())-ControlTC[,3] 
DiffTime <- ControlTC[DiffTime > 60*60* maxHours,2]
if(length(DiffTime)){
sapply(DiffTime,function(x){
	system(paste("taskkill /pid",x,"/f"))
})
}
}
setwd(wd)
}