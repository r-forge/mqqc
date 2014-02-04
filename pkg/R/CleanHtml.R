CleanHtml <- 
function(htmloutPath){
unlink(list.files(htmloutPath,full.name = T,pattern = "pdf$",recursive = T), recursive = T,force = T)
wd<- getwd()
setwd(htmloutPath)
writeToHtml()
setwd(wd)
}
