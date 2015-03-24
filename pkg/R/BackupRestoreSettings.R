BackupRestoreSettings <- 
function(action = "b",path = "./",unlinkFile = F,name = NULL,ASK = T){
pathData <- paste(path.package("mqqc"),"data",sep = "/")
if(ASK){
	if(action == "b"){
		PathName <- tkgetSaveFile()
		PathName <- tclvalue(PathName)
	}
	if(action == "r"){
		PathName <- tkgetOpenFile()
		PathName <- tclvalue(PathName)
	}
}else{
	PathName  <- paste(path,paste("MQQC-BackupSettings098fwoeifo2i3f09uc09ausdßv09p3oijrqp2hfßa98vuß0a9s8dvß09wuer",".Rdata"),sep = "/")	
}

if(action == "b"){
FileToLoad <- c("MailSettings","MQQCspecies.csv","Param.Rdata","MailList.txt")
MQQCSettings <- list()
for(i in 1:length(FileToLoad)){
	if(length(grep(".Rdata$",FileToLoad[i])) > 0){
		try.error <- class(try(load(paste(pathData,FileToLoad[i],sep = "/"))))
		if(try.error!= "try-error"){
			MQQCSettings[[i]] <- output	
		}else{
			MQQCSettings[[i]] <- NULL # Not sure if this will work, what happens with  MQQCGUI function
		}
		
	}else{
		filePath <- paste(pathData,FileToLoad[i],sep = "/")
	
		if(file.exists(filePath)){
				file <- readLines(filePath)	
		}else{
				file <-  t(as.matrix(rep("no list",2)))
		}
		MQQCSettings[[i]] <- file
	}
}
names(MQQCSettings) <- FileToLoad[1:length(MQQCSettings)]

if(length(PathName) > 0){
	try(save(MQQCSettings,file =PathName))
}
}
if(action == "r"){
	if(length(PathName) > 0){
		try(load(PathName))
	}
	
	for(i in 1:length(MQQCSettings) ){
		if(length(grep(".Rdata$",names(MQQCSettings)[i])) > 0){		
			output <- MQQCSettings[[i]]
			save(output,file = paste(pathData,"Param.Rdata",sep = "/"))
		}else{
			write(MQQCSettings[[i]],paste(pathData,names(MQQCSettings)[i],sep = "/"))
		}
	}
	if(unlinkFile){
		unlink(PathName)
	}	
}
if(ASK){
	tkmessageBox(message = "Backup/Restore done.")
}
cat("\rBackup/Restore done.")
}
