folder.observe <-
function(folder = NULL,fun= mqStarter,temp.name = "test"){

	
	
	temp.name <- "test"
	temp.name <- paste("_RmqqcFile_",temp.name,".txt",sep = "")
	
	if(length(folder) == 0){
		require(tcltk)
		folder <- tk_choose.dir(caption = "Please Select your")
		
	}
	# initiation
	setwd(folder)

	init.start 		<- list.files.nodir(folder,pattern = temp.name)
	
	if(length(init.start) == 0){
		catFun("initiate txt")
		files <- list.files.nodir(folder)
		setwd(folder)
		files <- files[! files == temp.name]
		write(files,file = temp.name)
	}else{
		catFun("load init.start")
		files <- readLines(init.start)
		files <- files[! files == temp.name]

	}

	
	loop <- T
	
	while(loop){
		
		####
	# Check if there is any evidence to process...
	####
	setwd(folder)

	evidenceToProcess <- checkMqqcInfo(folder)
  
  evidenceToProcess <- evidenceCheck(folder)  
	if(length(evidenceToProcess) > 0){
		for(i in 1:length(evidenceToProcess)){
			tempI 		<- evidenceToProcess[i]
			qcResults 	<- start.qc(tempI)
			
		}
		
	}
	setwd(folder)
		
		
		
		catFun(paste(Sys.time(),"Status: Observing", folder))
		Sys.sleep(1)
		obs.files			  <- list.files()
		temp.obs 			  <- grep("^_RmqqcFile_",obs.files)
		obs.files 			<- grepSubsetControl(temp.obs, obs.files)
		
		obs.files.diff 		<- setdiff(obs.files,files) 
		obs.files.minus 	<- setdiff(files,obs.files) 

		if(length(obs.files) > 0){
			#cat(paste("\rfound.something",obs.files.diff))
			files <- c(files, obs.files.diff)
			write(files,file = temp.name)
			batch <- T
			init.sample <- obs.files
			while(batch){
			  temp.batch   	<- sapply(init.sample, checkSize)
			  temp.batch.n 	<- names(temp.batch)[temp.batch == 0][1]
			  if(!is.na(temp.batch.n)){
			    # starting Maxquant stuff
			    tryError <- class(try(fun(temp.batch.n,mq.fun)))
			    if(tryError == "try-error"){
			      #	return(temp.batch.n)
			      catFun(paste("error in file", temp.batch.n))
			    }
			  }
			  init.sample <- setdiff(init.sample,temp.batch.n)
			  #catFun(temp.batch.n)
			  
			  if(length(init.sample) == 0 | is.na(temp.batch.n)){
			    batch <- F
			  }
			  
			}
			catFun("Closed Loop")

		}

		if(length(obs.files.minus) > 0){
			catFun("Cleanup deleted files")
			for(te in 1:length(obs.files.minus)){
				files <- files[!files == obs.files.minus[te]]
			}
			write(files,file = temp.name)
		}
			
		
	}
	
return(folder = folder)	
}


#folder.observe("/users/testmqqc2")
