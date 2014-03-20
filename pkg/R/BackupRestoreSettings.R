BackupRestoreSettings <- 
function(action = "b",path = "./",unlinkFile = T){
pathData <- paste(path.package("mqqc"),"data",sep = "/")
PathName  <- paste(path,paste("MQQC-BackupSettings098fwoeifo2i3f09uc09ausdßv09p3oijrqp2hfßa98vuß0a9s8dvß09wuer",".Rdata"),sep = "/")	
	
if(action == "b"){
FileToLoad <- c("MailSettings","MQQCspecies.csv","Param.Rdata")
MQQCSettings <- list()
for(i in 1:length(FileToLoad)){
	if(length(grep(".Rdata$",FileToLoad[i])) > 0){
		load(paste(pathData,FileToLoad[i],sep = "/"))
		MQQCSettings[[i]] <- Param
	}else{
		file <- readLines(paste(pathData,FileToLoad[i],sep = "/"))
		MQQCSettings[[i]] <- file
	}
}
names(MQQCSettings) <- FileToLoad

if(length(path) > 0){
	try(save(MQQCSettings,file =PathName))
}
}
if(action == "r"){
	if(length(path) > 0){
	
	files <- list.files(path,pattern = "MQQC-BackupSettings")
	try(load(PathName))
	}
	for(i in 1:length(MQQCSettings) ){
		if(length(grep(".Rdata$",names(MQQCSettings)[i])) > 0){		
			Param <- MQQCSettings[[i]]
			save(Param,file = paste(pathData,"Param.Rdata",sep = "/"))
		}else{
			write(MQQCSettings[[i]],paste(pathData,names(MQQCSettings[[i]])))
		}
	}
	if(unlinkFile){
		unlink(PathName)
	}	
}
}
