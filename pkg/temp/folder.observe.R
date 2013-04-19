#fun <- print("hui")


folder.observe <- function(folder,fun= print(1),temp.name){
	
	if(length(folder) == 0){
		require(tcltk)
		folder <- tk_choose.dir(,caption = "Please Select your")
		
	}
	# initiation
	setwd(folder)

	init.start 		<- list.files.nodir(folder,pattern = temp.name)
	
	if(length(init.start) == 0){
		cat("\rinitiate txt")
		files <- list.files.nodir(folder)
		setwd(folder)
		files <- files[! files == temp.name]
		write(files,file = temp.name)
	}else{
		cat("\rload init.start")
		files <- readLines(init.start)
	}
	####
	# Check if there is any evidence to process...
	####
	
	evidenceToProcess <- checkMqqcInfo(folder)
	if(length(evidenceToProcess) > 0){
		for(i in 1:length(evidenceToProcess)){
			tempI 		<- evidenceToProcess[i]
			qcResults 	<- start.qc(tempI)
			
		}
		
	}

	
	loop <- T
	
	while(loop){
		cat(paste("\r",Sys.time(),"Status: Observing", folder))
		Sys.sleep(1)
		obs.files		<- list.files()
		obs.files.diff 		<- setdiff(obs.files,files) 
		obs.files.minus 	<- setdiff(files,obs.files) 

		if(length(obs.files.diff) > 0){
			#cat(paste("\rfound.something",obs.files.diff))
			files <- c(files, obs.files.diff)
			write(files,file = temp.name)
			batch <- T
			init.sample <- obs.files.diff
			while(batch){
				temp.batch 		<- sapply(init.sample, checkSize)
				temp.batch.n 	<- names(temp.batch)[temp.batch == 0][1]
				if(!is.na(temp.batch.n)){
					alarm()
 					fun(temp.batch.n)
				}
				init.sample <- setdiff(init.sample,temp.batch.n)
				cat("\r",temp.batch.n)
				
				if(length(init.sample) == 0){
					batch <- F
				}
				
			}
		
			

		}
		if(length(obs.files.minus) > 0){
			cat("\rCleanup deleted files")
			for(te in 1:length(obs.files.minus)){
				files <- files[!files == obs.files.minus[te]]
			}
			write(files,file = temp.name)
		}
		
		
		
	}
	
	
}


check.file.changing <- function(files,mode = "size"){
	temp <- file.info(files)
	
	temp1 <- temp[,names(temp) == mode]
	temp1 <- cbind(rownames(temp),temp1)
	Sys.sleep(2)
	temp2 <- temp[,names(temp) == mode]
	temp2 <- cbind(rownames(temp),temp2)
	
	temp.all	<- merge(temp1,temp2,by = 1)
	
	temp.all.F 	<- apply(temp.all[,2:3],1,function(x){x<- all(x == 0);return(x)})

	if(!all(!temp.all.F)){
		temp.all 	<- temp.all[!temp.all.F,]
	}
	temp.all <- temp.all[temp.all[,2] == temp.all[,3],1]
	#}#else{temp.all <- ""}
	
	return(temp.all)
}
checkSize <- function(x){
	temp.x <- file.info(x)$size
	Sys.sleep(1)
	temp.y <- file.info(x)$size
	if(temp.y == 0){temp.y <- temp.x -1}
	
	return(temp.y-temp.x)
}

fun <- function(x){cat("\r",x)}

test <- folder.observe("~/documents/Skripte/R-standard-WD/test-folder",fun = mqStarter,temp.name = "test.txt")

