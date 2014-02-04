fixMailList <- 
function(){
	MailPath <- list.files(paste(path.package("mqqc"),"data",sep = "/"),pattern = "MailList.txt",recursive = T,full.name = T)
	if(length(MailPath) == 0){
	Mails <- matrix(c("IM","Its@Me.yx"),1,2)
	}else{
			Mails <- read.table(MailPath[1],sep = "\t")
	}
		try(colnames(Mails) <- c("abbreviation","email"))		
		Mails <- fix(Mails)
		if(dim(Mails)[2] ==2){
			write.table(Mails,MailPath,quote = F, row.names = F, col.names = F,sep = "\t")	
		}
		
}
