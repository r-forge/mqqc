

SendMailFun <- 
function(Message = "",title = "",filename = "",SendMail = T,placeholder = "PLACEHOLDER",RESettings = NULL){
if(length(RESettings) > 0){
MailList <- list.files(path.package("mqqc"),pattern = "MailList.txt",recursive=T,full.name = T)
if(length(MailList) > 0&SendMail){
  print("WRITING MAIL")
  MailList  <- read.table(MailList,sep = "\t",colClasses = "character",stringsAsFactors = F)
  MailList 	<- apply(MailList,2,as.character)
  MailList <- MailList[!is.na(MailList[,1]),]
  if(is.vector(MailList)){MailList <- t(as.matrix(MailList))}
  MailPatterns <- sapply(MailList[,1],function(x){gsub(placeholder,x,RESettings$REmail)})
  if(!all(sapply(MailPatterns,length) == 0)){
    MailID <- sapply(MailPatterns ,function(x){
      xn <- as.character(filename)
      try(x  <- grep(x,as.character(filename),value = F))
      return(length(x) > 0)
    })
    if(any(MailID)){
      if(!is.vector(MailList)){
        MailID <- MailList[MailID,]
      }else{
        MailID <- MailList[MailID]
      }
      MailID <- grep("@",MailID,fixed = T,value = T)[1]
      for(Mail in MailID){
        # PrepareMail("Hiho","Ich freue mich, dass diese eMail ankommt!",recipient = "henrik.zauber@mdc-berlin.de",MailSecurity = NULL)
        doit <- require("mailR")
        if(doit){
          PrepareMail(Title = title,Message = Message,recipient=gsub("@","@",as.character(Mail),fixed = T),MailSecurity = NULL)
        }
      }
    }
  }
}
}else{
  cat("no RESettings found")
}

}
