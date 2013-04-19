checkSize <- function(x){
	temp.x <- file.info(x)$size
	Sys.sleep(1)
	temp.y <- file.info(x)$size
	return(temp.y-temp.x)
}

obs.files.diff <- test

			batch <- T
			init.sample <- obs.files.diff
			return(obs.files.diff)
			while(batch){
				temp.batch 		<- sapply(init.sample, checkSize)
				temp.batch.n 	<- names(temp.batch)[temp.batch == 0][1]
				
				init.sample <- setdiff(init.sample,temp.batch.n)
				print(temp.batch.n)
				
				if(length(init.sample) == 0){
					batch <- F
				}
				
			}
		}
			
