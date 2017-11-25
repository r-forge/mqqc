
PrepareMail <- 
function(Title= "Tomato",Message="Merry Christmas",recipient = "santa\\@clause.np",MailSecurity = list(ssl = F,tls = T,port= 587),...){

MailList <- list()
MailList$user <- "user"
MailList$pass <- "pass"
MailList$server <- "server"
MailList$to <- recipient
MailList$from <- "from"
MailList$title <- Title
MailList$message <- Message

MailSettings <- list.files(paste(path.package("mqqc"),"data",sep = "/"),pattern = "MailSettings$",full.name = T)

if(length(MailSettings) > 0){	

	MailSettings <- readLines(MailSettings[1])

	if(length(MailSecurity) == 0){
	  cat("Trying to obtain settings from MailSettings")
	  MailSecurity$ssl <-F
	  MailSecurity$tls <-F
	  if(MailSettings[6]== "tls"){
	    MailSecurity$ssl <-F
	    MailSecurity$tls <-T
	    
	  }
	  if(MailSettings[6]== "ssl"){
	    MailSecurity$ssl <-T
	    MailSecurity$tls <-F
	  }
	  MailSecurity$port = as.numeric(MailSettings[5])
	}
	MailList <<- MailList
	perl.init <- list.files(paste(path.package("mqqc"),"data",sep = "/"),pattern = "mailPerl.pl",full.name = T)

	MailList <<- MailList
	perl.init <<- perl.init
	MailSecurity <<- MailSecurity
	if(require(mailR)){
	  cat("\rsending Mail to",MailList$to)
	  send.mail(from = MailSettings[4],to = MailList$to,subject =MailList$title,body = MailList$message,smtp = list(host.name = MailSettings[3],port = MailSecurity$port,user.name = MailSettings[1],passwd = MailSettings[2],ssl = MailSecurity$ssl,tls = MailSecurity$tls),authenticate = T,send = T,...)
	}
	}



}



