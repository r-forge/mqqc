MailImport <- 
function(){
			importMail <- tclvalue(tkmessageBox(type = "yesno",icon = "question",message="Do you like to import a Mail list (tab delim; 2 columns)?"))
		if(importMail == "yes"){
			print("start import")
			temp <-tclvalue(tkgetOpenFile())
			tempRead <- read.table(temp,sep = "\t")
			print(is.matrix(tempRead))
			if(is.data.frame(tempRead)){
				if(dim(tempRead)[2] == 2){
					file.copy(temp,tempPAth<- paste(path.package("mqqc"),"data","MailList.txt",sep = "/"))
					print(tempPAth)
					print("copied MailList...")
				}else{tkmessageBox(icon = "warning",message = "Import failed. MailList.txt requires a tab delimited table with two columns (template,address)....")}
			}
			
			
			
		}
}		

		