mqqcGUI <- 
function(){
library(tcltk)
.GlobalEnv$MQQCRestored <- F

try(load(file=paste(path.package("mqqc"),"data/Param.Rdata",sep = "/")))

outputError <- F
if(exists("output")){
  if(length(output) == 0){
    outputError <- T
  }
}

if(!exists("output")|outputError){
	loadQ	<- tkmessageBox(message = "No Parameter file found. Import new Rdata container?",type = "yesno")
	if(tclvalue(loadQ) == "yes"){
		pathToRdata <- tkgetOpenFile()
		try(load(tclvalue(pathToRdata)))
		
	}else{
		output <- list(	
		MQ = "",
		folder ="",
		fastaFile = "",
		htmloutPath = "",
		DeleteFiles =0,
		cores = "auto" ,
		SpeciesTable = T
		)
	}
	
	
}

exportRdata <- function(){
		species.path<- paste(path.package("mqqc"),"data/Param.Rdata",sep = "/")
		out <- tclvalue(tkgetSaveFile(initialfile = "Param.Rdata"))
		if(out!=""){
			file.copy(species.path,out)
		}
}
	


fontHeading <- tkfont.create(family = "Tahoma",size=15,weight="bold")

  checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
if(length(checkMQ)> 0){
	MQpath <- tclVar(readLines(checkMQ))
}else{MQpath <- tclVar("MQ-path")}

tt <- tktoplevel()
	tkwm.title(tt,"MQQC")


ttf1 <- tkframe(tt)
ttf2 <- tkframe(tt)
tkLFtable <- ttklabelframe(ttf2,text = "Species Table")

init <- "/home"
MQpath    <- tclVar(output$MQ)
if(output$MQ != ""){init <- output$MQ}

tkgrid(tklabel(tt,text = "MQQC",font = fontHeading),columnspan = 2)
dirFrame <- ttklabelframe(ttf1,text = "MaxQuant Folder")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(init){
	      
        File <- tclvalue(tkchooseDirectory(parent = tt,title = "MaxQuant Folder",initialdir = init))
        if(File != ""){
        tclvalue(MQpath) <- File
        } 
        }
    browseButton <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse, 	borderwidth=3)
    
    
    locationField <- ttkentry(dirFrame, textvariable=MQpath,justify = "right")        
    tkgrid(browseButton,locationField)
	tkgrid(dirFrame)
	
StandardFasta <- tclVar(output$fastaFile)
dirFrame <- ttklabelframe(tt,text = "Standard Fasta")

	buttonRcmdr2 <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse2 <- function(init){
        File <- tclvalue(tkgetOpenFile(parent = tt,title = "Default Fasta File",initialdir = init))
        
         if(File != ""){
        tclvalue(StandardFasta) <- File
        } 
        }
    browseButton2 <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse2, 	borderwidth=3)
    
    
    locationField <- ttkentry(dirFrame, textvariable=StandardFasta,justify = "right")        
    #tkgrid(browseButton2,locationField)
	#tkgrid(dirFrame,padx = 5,pady = 5)


Analysis.Folder <- tclVar(output$folder)
dirFrame <- ttklabelframe(ttf1,text = "Analysis Folder")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(init){
        File <- tclvalue(tkchooseDirectory(parent = tt,title = "Analysis Folder",initialdir = init))
          if(File != ""){
        tclvalue(Analysis.Folder) <- File
        } 
        }
    browseButton <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse, 	borderwidth=3)
    
    
    locationField <- ttkentry(dirFrame, textvariable=Analysis.Folder,justify = "left")        
    tkgrid(browseButton,locationField)
	tkgrid(dirFrame,padx = 5,pady = 5)
	

	TKMisc <- ttklabelframe(ttf2,text = "Misc")
	
	
	########
	# other buttons
	########

	cb <- tkcheckbutton(TKMisc)
	cbVar <- tclVar(as.numeric(output$DeleteFiles))
	tkconfigure(cb,variable=cbVar)
	tkgrid(tklabel(TKMisc,text = "Clean up"),cb)
	
	########
	# other buttons
	########

	cb <- tkcheckbutton(TKMisc)
	cbVar2 <- tclVar(as.numeric(output$Debug))
	tkconfigure(cb,variable=cbVar2)
	tkgrid(tklabel(TKMisc,text = "Debug Mode"),cb)
	
	
	######
	# cores
	######
	
	tb1.duplicates.var 					<- c("auto",1:24)
	tb1.val.pep.duplicates 				<- tclVar()  
	tclvalue(tb1.val.pep.duplicates) 	<- (output$cores)
	comboBox 							<- ttkcombobox(TKMisc,values=tb1.duplicates.var,textvariable = tb1.val.pep.duplicates,width = 17,state = "readonly",width = 6)
	tkgrid(tklabel(TKMisc,text = "Cores"),comboBox,pady = 2)
	tkgrid(TKMisc,sticky = "SW")
	
	####
	# set fasta files
	####
speciesFun <- 	function(){
		     species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv",full.name = T,recursive = T)
			 species <- read.csv(species.path)
			 try(speciesTK(species))	
				species$Fasta <- .GlobalEnv$mqqcSpeciesSet
              write.csv(species,file =species.path,quote = F, row.names = F)
			
	}
speciesFunFix <- 	function(){
		     species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv",full.name = T,recursive = T)
			 species <- read.csv(species.path)
			 try(species <- fix(species))	
			 write.csv(species,file =species.path,quote = F, row.names = F)
         print("wrote Species Table")
		
	}

loadSpecies <- function(){
		inFile <- tclvalue(tkgetOpenFile(initialfile = "MQQCspecies.csv"))
		species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv",full.name = T,recursive = T)
		inF <- 		try(readLines(inFile,n = 1))
		oldF <- try(readLines(species.path ,n = 1))
		tkMes <- "yes"
		if(inF== oldF){
				tkMes	<- tclvalue(tkmessageBox(icon = "warning",message = paste("Wrong  table format!\n\nHeader doesn't match. Should be:\n",oldF,"\n but is:\n",inF,"\nImport anyway?",sep = "\n"),type = "yesno"))

		}	
			if(tkMes == "yes"){
				tkMes	<- tclvalue(tkmessageBox(icon = "warning",message = paste("Backup original table?",sep = "\n"),type = "yesno"))
				if(tkMes == "yes"){
					out <- tclvalue(tkgetSaveFile(initialfile = "MQQCspecies.csv"))
					if(out !=""){
						file.copy(species.path,out, overwrite = F)
					}
				}

				file.copy(inFile, species.path, overwrite = T)
			}
		

}
exportSpecies <- function(){
		species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv",full.name = T,recursive = T)
		out <- tclvalue(tkgetSaveFile(initialfile = "MQQCspecies.csv"))
		if(out!=""){
			file.copy(species.path,out)
		}
}

ExportImportFun <- function(){
	tt2 <- tktoplevel()
	tt2.rb1 <- tkradiobutton(tt2)
	tt2.rb2 <- tkradiobutton(tt2)

	
	tt2.quant.method <- tclVar("Backup")
    tkconfigure(tt2.rb1,variable=tt2.quant.method,value="Backup")
	tkconfigure(tt2.rb2,variable=tt2.quant.method,value="Restore")
	tkgrid(tklabel(tt2,text = "Backup"),tklabel(tt2,text = "Restore"))
	tkgrid(tt2.rb1,tt2.rb2)
	# Stop Go

tkgrid(tkbutton(tt2,text = "go",command = function(){.GlobalEnv$aborttt2  <- F;tkdestroy(tt2)}),tkbutton(tt2,text = "stop",command = function(){ .GlobalEnv$aborttt2  <- T; tkdestroy(tt2)}),columnspan = 1,padx = 3,pady = 3)

tkwait.window(tt2)
if(.GlobalEnv$aborttt2){
	
}else{
	.GlobalEnv$MQQCRestored <- F
	if(tclvalue(tt2.quant.method) == "Backup"){
		BackupRestoreSettings("b")
	}else{
		BackupRestoreSettings("r")	
		.GlobalEnv$MQQCRestored <- T	
		.GlobalEnv$MQQCRestartNow<- tclvalue(tkmessageBox(type = "yesno",message ="MQQC GUI needs to be restarted to apply the new settings. Restart now?"))
	}
}
if(.GlobalEnv$MQQCRestartNow == "yes"){
	print("Destroy")
	.GlobalEnv$abort <- F
	tkdestroy(tt)
}
}
######



	fastaSetFix <- tkbutton(tkLFtable,text = "Table",command = speciesFunFix)	
	fastaSet <- tkbutton(tkLFtable,text = "Fasta",command = speciesFun)
	exportSet <- tkbutton(tkLFtable,text = "Export",command = exportSpecies)
	loadSet <- tkbutton(tkLFtable,text = "Import",command = loadSpecies)

	tkgrid(fastaSet, fastaSetFix,pady = 2,sticky = "NSWE")
	tkgrid(exportSet,loadSet,pady = 2,sticky = "NSWE")
	tkgrid(tkLFtable,columnspan = 2,sticky = "NSWE")

	####
	# HTML
	####
	HTML <- tclVar(output$htmloutPath)
dirFrame <- ttklabelframe(ttf1,text = "HTML Path")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(init){
		File <- tclvalue(tkchooseDirectory(parent = tt,title = "HTML Folder",initialdir = init))
		if(File != ""){
        tclvalue(HTML) <- File
        } 
                }
    browseButton <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse, 	borderwidth=3)
    
    
    locationField <- ttkentry(dirFrame, textvariable=HTML)        
    tkgrid(browseButton,locationField)
	tkgrid(dirFrame,columnspan = 2 )
tkgrid(ttf1,ttf2,pady = 1,padx = 1,sticky = "NSWE")
.GlobalEnv$abort <- F

tkgrid(ttkseparator(tt),sticky = "WE",columnspan = 2,padx = 3,pady=1)
	ExImBut <- tkbutton(tt,text = "Backup/Restore all",command = ExportImportFun)
	tkgrid(ExImBut, columnspan = 3)	
tkgrid(ttkseparator(tt),sticky = "WE",columnspan = 2,padx = 3,pady=1)
	
tkgrid(tkbutton(tt,text = "go",command = function(){tkdestroy(tt)}),tkbutton(tt,text = "stop",command = function(){ .GlobalEnv$abort  <- T; tkdestroy(tt)}),columnspan = 1,padx = 3,pady = 3)
#tkwm.resizable(tt, "FALSE","FALSE")
temp <- tkframe(ttf1)
	TKBexport <- tkbutton(temp,text = "export Paths",command = exportRdata)
	TKMail <- tkbutton(temp,text = "@",command = fixMailList)
	TKMailImport <- tkbutton(temp,text = "@ import",command = MailImport)

	#TKBload <- tkbutton(ttf1,text = "export Param.Rdata",command = exportRdata)

	tkgrid(TKBexport, TKMailImport,TKMail)
	
	tkgrid(temp)
	tkwait.window(tt)

if(.GlobalEnv$abort ){stop("abort by user")}

cores <- tclvalue(tb1.val.pep.duplicates)
if(cores == "auto"){cores <- NULL}

output <- list(	
		MQ = tclvalue(MQpath),
		folder = tclvalue(Analysis.Folder),
		fastaFile = tclvalue(StandardFasta),
		htmloutPath = tclvalue(HTML),
		DeleteFiles = as.logical(as.numeric(tclvalue(cbVar))),
		Debug		 = as.logical(as.numeric(tclvalue(cbVar2))),

		cores = cores ,
		SpeciesTable = T
		)
if(!.GlobalEnv$MQQCRestored ){
save(output,file=paste(path.package("mqqc"),"data/Param.Rdata",sep = "/"))
}
# check settings
for(i in 1:4){
	tempI <- as.character(output[[i]])
	tempI <- list.files(dirname(tempI),pattern= basename(tempI))
	if(length(tempI) == 0){
		output[[i]] <- ""		
	}
}
		
		

return(output)

}

#hui <- mqqcGUI()
#print(hui)
