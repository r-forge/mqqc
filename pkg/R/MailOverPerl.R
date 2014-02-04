MailOverPerl <- 
function(MailList= NULL,perl.init){
if(length(MailList)>=6){
addVec <- paste("send_mail(",paste(lapply(MailList,function(x){x <- paste("\"",x,"\"",collapse = "",sep = "")}),collapse = ",",sep = ""),")",sep = "")
temp<- readLines(perl.init)
DropZone <- grep("#DropZone",temp)
temp <- temp[-c((DropZone+1):length(temp))]
write(c(temp,addVec), perl.init)

print("Mail")
wd <- getwd()
setwd(dirname(perl.init))
PerlSend <<- perl.init
PerlMail <<- system(PerlCommand<<-paste("perl",basename(perl.init)), wait = T, intern = F,ignore.stdout = T,ignore.stderr = T)
setwd(wd)
}
}


