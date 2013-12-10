
PrepareMail <- 
function(Title= "Tomato",Message="Merry Christmas",recipient = "santa\\@clause.np"){

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
		#print("hu")

	MailSettings <- readLines(MailSettings[1])
		#print("hu")

	if(length(MailSettings) ==4){
		MailList[c(1,2,3,5)] <- MailSettings	
	}
	MailList <<- MailList
	perl.init <- list.files(paste(path.package("mqqc"),"data",sep = "/"),pattern = "mailPerl.pl",full.name = T)

	MailList <<- MailList
	perl.init <<- perl.init
	MailOverPerl(MailList,perl.init)
		
	}



}


