file.ready <- 	check.file.changing(obs.files.diff)	
				print(file.ready)
				if(length(file.ready)< 0){
					return(file.ready)
					file.ready 	<- file.ready[1]
					init.sample <- init.sample[!init.sample == file.ready]
					fun(init.sample)	
				}