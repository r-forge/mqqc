LoadSettings <- 
function(...){
	for(i in 1:length(list(...))){
		assign(names(list(...)[i]),list(...)[[i]],envir = .GlobalEnv)
		
		
	}
}

#LoadSettings(folder = NULL,MQ = NULL,fastaFile = NULL,fun= mqStarter,temp.name = "test", DeleteFiles = F,cores = NULL,SpeciesTable = T,templateFasta = "._.*_.*_PLACEHOLDER_",placeholder = "PLACEHOLDER",FUNLAST = FUNFINAL,sucFolder = "_RmqqcFile_Processed",htmloutPath = "D:/_RmqqcFile_mqqcHtml",gui = T,SendMail = T, automatedStart = F)