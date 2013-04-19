install.packages("E:/mqqc",repos = NULL,type = "source")

temp.name <- "text.csv"
folder <- "C:/mqqc.observe"
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

loop <- T