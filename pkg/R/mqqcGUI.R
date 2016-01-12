mqqcGUI <- 
function(){
library(tcltk)
library(tcltk2)
require("widgetTools")
.GlobalEnv$MQQCRestartNow <- "no"
.GlobalEnv$MQQCRestored <- F
    fontHeading <- tkfont.create(family = "Tahoma",size=14,weight="bold")
.GlobalEnv$aborttt2 <- T

try(load(file=paste(path.package("mqqc"),"data/Param.Rdata",sep = "/")))
.GlobalEnv$Backup <- F
outputError <- F
if(exists("output")){
  if(length(output) == 0){
    outputError <- T
  }
}

if(!exists("output")|outputError){
	#loadQ	<- tkmessageBox(message = "No Parameter file found. Import new Rdata container?",type = "yesno")
	#if(tclvalue(loadQ) == "yes"){
	if(0){
		pathToRdata <- tkgetOpenFile()
		try(load(tclvalue(pathToRdata)))
		
	}else{
		output <- list(	
		MQ = "",
		folder ="",
		fastaFile = "",
		htmloutPath = "",
		DeleteFiles = 0,
		cores = 1 ,
		SpeciesTable = T,
		REpar = "\\D*_\\d*_[^_]*_PLACEHOLDER_",
		REmac = "^\\D*_",
		REmail = "\\D*_\\d*_PLACEHOLDER_",
		REtime = "_\\d{8}_",
		REmisc =  "[^_]*_[^_]*_[^_]*_[^_]*",
		BSAID = "P02769"
		
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
	



  checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
if(length(checkMQ)> 0){
	MQpath <- tclVar(readLines(checkMQ))
}else{MQpath <- tclVar("MQ-path")}


col <- c("#FFFFFF","#426787")

col <- rev(col)
tt <- tktoplevel(bg =col[1])


# topMenu <- tkmenu(tt)           # Create a menu
# tkconfigure(tt, menu = topMenu) # Add it to the 'tt' window
# themes <- tk2theme.list()
# themeMenu <- tkmenu(topMenu, tearoff = FALSE)
# if ("alt" %in% themes) tkadd(themeMenu, "command", label = "alt",
    # command = function() tk2theme("alt"))
# if ("aqua" %in% themes) tkadd(themeMenu, "command", label = "aqua",
    # command = function() tk2theme("aqua"))
# if ("clam" %in% themes) tkadd(themeMenu, "command", label = "clam",
    # command = function() tk2theme("clam"))
# tkadd(themeMenu, "command", label = "clearlooks",
    # command = function() tk2theme("clearlooks"))
# if ("classic" %in% themes) tkadd(themeMenu, "command", label = "classic",
    # command = function() tk2theme("classic"))
# if ("default" %in% themes) tkadd(themeMenu, "command", label = "default",
    # command = function() tk2theme("default"))
# tkadd(themeMenu, "command", label = "keramik",
    # command = function() tk2theme("keramik"))
# tkadd(themeMenu, "command", label = "plastik",
    # command = function() tk2theme("plastik"))
# tkadd(themeMenu, "command", label = "radiance (fonts change too)!",
    # command = function() tk2theme("radiance"))
# if ("vista" %in% themes) tkadd(themeMenu, "command", label = "vista",
    # command = function() tk2theme("vista"))
# if ("winnative" %in% themes) tkadd(themeMenu, "command", label = "winnative",
    # command = function() tk2theme("winnative"))
# if ("xpnative" %in% themes) tkadd(themeMenu, "command", label = "xpnative",
    # command = function() tk2theme("xpnative"))
# tkadd(themeMenu, "separator")
# tkadd(themeMenu, "command", label = "Quit", command = function() tkdestroy(tt))
# tkadd(topMenu, "cascade", label = "Theme", menu = themeMenu)
# tkfocus(tt)



	tkwm.title(tt,"MQQC")
tkgrid(tk2label(tt,text = "MQQC",font = fontHeading,background = col[1], foreground = col[2]),columnspan = 3)
tkwm.resizable(tt, "FALSE","FALSE")	

Tabs <- c("General Settings", "Advanced")
ttb <- tkframe(tt,bg = col[2])
tt1 <- tk2frame(ttb)

nb 	<- tk2notebook(tt1, tabs = Tabs) # starts notebook
tkgrid(nb,columnspan = 2,padx=5,sticky = "WE",sticky = "NSWE")
tt2 <- tk2notetab(nb, Tabs[1])
tt3 <- tk2notetab(nb, Tabs[2])


ttf1 <- tk2frame(tt2)
ttf2 <- tk2frame(tt2)
tkLFtable <- tk2labelframe(ttf2,text = "MQQC Parameter Table")
tk2tip(ttf2,"Set here required settings for MaxQuant and Quality Control analysis.")
init <- "/home"
MQpath    <- tclVar(output$MQ)
if(output$MQ != ""){init <- output$MQ}

dirFrame <- tk2labelframe(ttf1,text = "MaxQuant Folder")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(init){
	      
        File <- tclvalue(tkchooseDirectory(parent = tt2,title = "MaxQuant Folder",initialdir = init))
        if(File != ""){
        tclvalue(MQpath) <- File
        } 
        }
    browseButton <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse, 	borderwidth=3)
    

    locationField <- tk2entry(dirFrame, textvariable=MQpath,justify = "right")        
    tkgrid(browseButton,locationField,sticky = "NSWE",padx = 1,pady = 1)
	tkgrid(dirFrame,sticky = "NSWE",padx = 3,pady = c(5,3))
	tk2tip(dirFrame,"Path to MaxQuant Folder.\nCurrent supported version is 1.3.0.5.\nLater version might work as well.")
	
	
StandardFasta <- tclVar(output$fastaFile)
dirFrame <- tk2labelframe(tt2,text = "Standard Fasta")

	buttonRcmdr2 <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse2 <- function(init){
        File <- tclvalue(tkgetOpenFile(parent = tt2,title = "Default Fasta File",initialdir = init))
        
         if(File != ""){
        tclvalue(StandardFasta) <- File
        } 
        }
    browseButton2 <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse2, 	borderwidth=3)
    
  
    locationField <- tk2entry(dirFrame, textvariable=StandardFasta,justify = "right")   
       
    #tkgrid(browseButton2,locationField)
	#tkgrid(dirFrame,padx = 5,pady = 5)


Analysis.Folder <- tclVar(output$folder)
dirFrame <- tk2labelframe(ttf1,text = "Analysis Folder")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(init){
        File <- tclvalue(tkchooseDirectory(parent = tt2,title = "Analysis Folder",initialdir = init))
          if(File != ""){
        tclvalue(Analysis.Folder) <- File
        } 
        }
    browseButton <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse, 	borderwidth=3)
    
    
    locationField <- tk2entry(dirFrame, textvariable=Analysis.Folder,justify = "left")        
    tkgrid(browseButton,locationField,sticky = "NSWE",padx = 1,pady = 1)
	tkgrid(dirFrame, sticky = "WENS",padx = 2,pady = c(2,2))
	tk2tip(dirFrame,"MQQC Main Folder. \nNew raw files, dropped here, will be analyzed by MQQC.")
	

	TKMisc <- tk2labelframe(ttf2,text = "Misc")
	
	########
	# other buttons
	########

	cb <- tk2checkbutton(TKMisc)
	cbVar <- tclVar(as.numeric(output$DeleteFiles))
	tkconfigure(cb,variable=cbVar)
	
	tk2label <-  tk2label(TKMisc,text = "Clean up")
	tkgrid(tk2label,cb)
	tk2tip(tk2label,"If checked, analysed raw files will be deleted after 24h, only MQQC results are stored.")
	########
	# other buttons
	########

	cb <- tk2checkbutton(TKMisc)
	cbVar2 <- tclVar(as.numeric(output$Debug))
	tkconfigure(cb,variable=cbVar2)
	tk2label <- tk2label(TKMisc,text = "Debug Mode")
	tkgrid(tk2label ,cb)
	tk2tip(tk2label,"Display Warnings and errors in console.")	


	tb1.TableOrder.var 					<- c("system","source")
	tb1.val.pep.TableOrder 				<- tclVar()  
	if(length(output$htmlsort) == 0){output$htmlsort = "source"}
	tclvalue(tb1.val.pep.TableOrder) 	<- (output$htmlsort)
	comboBox 							<- tk2combobox(TKMisc,values=tb1.TableOrder.var,textvariable = tb1.val.pep.TableOrder,width = 17,state = "readonly",width = 6)
	tk2labelTaOr <- tk2label(TKMisc,text = "Table Sorting")
	tkgrid(tk2labelTaOr,comboBox,pady = 2,sticky = "NSWE")
	#tkgrid(TKMisc,sticky = "SWNE",padx = 2)
	tk2tip(tk2labelTaOr,"Number of threads MQQC is allowed to use. \"auto\" tries to identify available threads and uses n-1 for MQQC analysis.")
	
	
	######
	# cores
	######
	
	tb1.duplicates.var 					<- c("auto",1:24)
	tb1.val.pep.duplicates 				<- tclVar()  
	tclvalue(tb1.val.pep.duplicates) 	<- (output$cores)
	comboBox 							<- tk2combobox(TKMisc,values=tb1.duplicates.var,textvariable = tb1.val.pep.duplicates,width = 17,state = "readonly",width = 6)
	tk2label <- tk2label(TKMisc,text = "Threads")
	tkgrid(tk2label,comboBox,pady = 2,sticky = "NSWE")
	#tkgrid(TKMisc,sticky = "SWNE",padx = 2)
	tk2tip(tk2label,"Number of threads MQQC is allowed to use. \"auto\" tries to identify available threads and uses n-1 for MQQC analysis.")

	####
	# set fasta files
	####
speciesFun <- 	function(){
		     species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv$",full.name = T,recursive = T)
			 species <- read.csv(species.path)
			 try(speciesTK(species))	
				species$Fasta <- .GlobalEnv$mqqcSpeciesSet
              write.csv(species,file =species.path,quote = F, row.names = F)
			
	}
speciesFunFix <- 	function(){
		     species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv$",full.name = T,recursive = T)
			 species <- read.csv(species.path)
			 try(species <- fix(species))	
			 write.csv(species,file =species.path,quote = F, row.names = F)
         print("wrote Species Table")
		
	}

loadSpecies <- function(){
		inFile <- tclvalue(tkgetOpenFile(initialfile = "MQQCspecies.csv"))
		species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv$",full.name = T,recursive = T)
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
		species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv$",full.name = T,recursive = T)
		out <- tclvalue(tkgetSaveFile(initialfile = "MQQCspecies.csv"))
		if(out!=""){
			file.copy(species.path,out)
		}
}

ExportImportFun <- function(){
	tt2 <- tktoplevel()
	tt2.rb1 <- tk2radiobutton(tt2)
	tt2.rb2 <- tk2radiobutton(tt2)

	
	tt2.quant.method <- tclVar("Backup")
    tkconfigure(tt2.rb1,variable=tt2.quant.method,value="Backup")
	tkconfigure(tt2.rb2,variable=tt2.quant.method,value="Restore")
	tkgrid(tk2label(tt2,text = "Backup"),tk2label(tt2,text = "Restore"))
	tkgrid(tt2.rb1,tt2.rb2)
	# Stop Go
.GlobalEnv$aborttt2 <- T
tkgrid(tkbutton(tt2,text = "GO",command = function(){.GlobalEnv$aborttt2  <- F;tkdestroy(tt2)},bg = "#009193",fg = "white"),tkbutton(tt2,text = "Cancel",command = function(){ .GlobalEnv$aborttt2  <- T; tkdestroy(tt2)},bg = "#ff7e79",fg = "white"),columnspan = 1,padx = 3,pady = 3)
tkwait.window(tt2)



if(.GlobalEnv$aborttt2){
	
}else{
	if(tclvalue(tt2.quant.method) == "Backup"){
		Control <- tkmessageBox(type = "yesno",message= "Warning,  a backup requires closing of the Interface. Do you like to continue?",icon = "warning")
		if(tclvalue(Control) == "yes"){
		tkdestroy(tt)
		
		#BackupRestoreSettings("b")
		.GlobalEnv$MQQCRestored <- F
		.GlobalEnv$MQQCRestartNow <- "yes"
		.GlobalEnv$Backup <- T
		
		
		tt2.quant.method <<- tt2.quant.method
		Control <<- Control		
		
		}
	}else{
		BackupRestoreSettings("r")	
		tryError <- class(try(trk <- readLines(paste(path.package("mqqc"),"data/mailSettings",sep = "/"))))
		print("Refining MailSettings")
		try(trk1 <- tclVar(trk[1]))
		try(trk2 <- tclVar(trk[2]))
		try(trk3 <- tclVar(trk[3]))
		try(trk4 <- tclVar(trk[4]))	
		
		.GlobalEnv$MQQCRestored <- T	
		.GlobalEnv$Backup <- F
		
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



	fastaSetFix <- tk2button(tkLFtable,text = "Table",command = speciesFunFix)	
	tk2tip(fastaSetFix,"Edit Parameter Table.\nSettings are in table format with experiment types in rows and experiment specific parameters in columns.\n \"ExperimentID\" is the label required in the raw File name to select the correct experiment type.")
	fastaSet <- tk2button(tkLFtable,text = "Fasta",command = speciesFun)
	tk2tip(fastaSet,"Redefine paths to required fasta files in Parameter Table.")
	exportSet <- tk2button(tkLFtable,text = "Export",command = exportSpecies)
	
	tk2tip(exportSet,"Export Parameter Table.\nUseful to modify settings in alternative table viewers.") 
	loadSet <- tk2button(tkLFtable,text = "Import",command = loadSpecies)
	tk2tip(loadSet,"Import a tab delimited Parameter Table.\nThe Format has to be the same as in an exported table, using the \"Export\" button.")
	tkgrid(fastaSetFix ,fastaSet,pady = 2,sticky = "NSWE",padx = 1)
	tkgrid(exportSet,loadSet,pady = 2,sticky = "NSWE",padx = 1)
#	tkgrid(tkLFtable,columnspan = 2,sticky = "NSWE",padx = 1)
tkgrid(tkLFtable,sticky = "NSWE",padx = 5,pady = 5)
tkgrid(TKMisc,sticky = "NSWE",padx = 5,pady = 5)

	####
	# HTML
	####
	HTML <- tclVar(output$htmloutPath)
dirFrame <- tk2labelframe(ttf1,text = "HTML Path")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(init){
		File <- tclvalue(tkchooseDirectory(parent = tt2,title = "HTML Folder",initialdir = init))
		if(File != ""){
        tclvalue(HTML) <- File
        } 
                }
    browseButton <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse, 	borderwidth=3)
    
 
    locationField <- tk2entry(dirFrame, textvariable=HTML)        
    tkgrid(browseButton,locationField,sticky = "NWES",padx = 1,pady = 1)
	tkgrid(dirFrame,columnspan = 2 ,sticky = "NSWE",padx = 2,pady = c(2,2))
	tk2tip(dirFrame,"Folder for MQQC report in html Format.\nDisplays Information from the last 10 runs for each Machine.\nThis folder can be located on a server to provide access over Intra/Internet.")

	
tkgrid(ttf1,ttf2,pady = 1,padx = 1,sticky = "NSWE")
.GlobalEnv$abort <- F

#tkgrid(ttkseparator(tt2),sticky = "WE",columnspan = 2,padx = 3,pady=1)
	ExImBut <- tk2button(tt1,text = "Backup/Restore all",command = ExportImportFun)
	tkgrid(ExImBut, columnspan = 3)	
	tk2tip(ExImBut,"Backup MQQC settings or Restore from a previous safed Backup.\n Before updating to a newer Version of MQQC, it is recommended to backup the settings which are otherwise lost.")
#tkgrid(ttkseparator(tt2),sticky = "WE",columnspan = 2,padx = 3,pady=1)

##=======
# Second Tab
#========
print("hu")

tb2.Par.GE <- tclVar(as.character(output$REpar))# tclVar("._.*_.*_PLACEHOLDER")
tb2.Machine.GE <- tclVar(as.character(output$REmac))#tclVar("._.*_.*_PLACEHOLDER")
tb2.Mail.GE <- tclVar(as.character(output$REmail))#tclVar("._.*_.*_PLACEHOLDER") #.*_.*_HZ
tb2.Time.GE <-tclVar(as.character(output$REtime))# tclVar("._.*_.*_PLACEHOLDER")
tb2.Misc.GE <-tclVar(as.character(output$REmisc))# tclVar("._.*_.*_PLACEHOLDER")

RegExp 			<- tk2labelframe(tt3,text = "Regular Expressions")
width <- 20
tempMachine 	<- tkentry(RegExp,textvariable = tb2.Machine.GE,width = width,background = "white")
tempTime 		<- tkentry(RegExp,textvariable = tb2.Time.GE,width = width,background = "white")
tempMail 		<- tkentry(RegExp,textvariable = tb2.Mail.GE,width = width,background = "white")
tempPar 			<- tkentry(RegExp,textvariable = tb2.Par.GE,width = width,background = "white")
tempMisc 		<- tkentry(RegExp,textvariable = tb2.Misc.GE,width = width,background = "white")

tkgrid(tk2label(RegExp,text = "Machine *"), tempMachine,pady = 2,sticky = "W",padx = 2)
tkgrid(tk2label(RegExp,text = "Date *"), tempTime,pady = 2,sticky = "W",padx = 2)
tkgrid(tk2label(RegExp,text = "Mail *"), tempMail,pady = 2,sticky = "W",padx = 2)
tkgrid(tk2label(RegExp,text = "Parameter"), tempPar,pady = 2,sticky = "W",padx = 2)
tkgrid(tk2label(RegExp,text = "Misc *"), tempMisc,pady = 2,sticky = "W",padx = 2)

##=======
# Machine Table
#========

myRarray <- as.character(output$Machines)

tclarray <- tclArray()
for(i in 1:length(myRarray)){
	tclarray[[(i-1),0]] <- myRarray[i]
}

MachineTK 			<- tk2labelframe(tt3,text = "Machines")



table1 <- displayInTable(tclarray,nrow=length(tclarray),ncol=0,tt = MachineTK,height = 4)

#tkgrid(RegExp,sticky = "WE",columnspan = 1)
tkgrid(RegExp,MachineTK,sticky = "NWSE",columnspan = 1,padx = 5,pady=3)

tk2tip(MachineTK,"ID(s) of used Systems.\nOptional for html output and archive.")
tk2tip(RegExp,"Regular Expressions to identify Machine ID, Time, Mail ID and Parameter ID in the raw file name. * - optional ")

StdID 			<- tk2labelframe(tt3,text = "Standard Parameter IDs")

StdIDlow 	<- tclVar(as.character(output$StdIDlow))
StdIDhigh 	<- tclVar(as.character(output$StdIDhigh))
StdIDlowID 	<- tclVar(as.character(output$BSAID))
LowIDframe 	<- tk2labelframe(StdID,text = "Protein Standard")
HighIDframe 	<- tk2labelframe(StdID,text = "Complex Sample Standard")

LowComplex <- tk2entry(LowIDframe, textvariable= StdIDlow,justify = "left",width = 10)        
HighComplex <- tk2entry(HighIDframe, textvariable= StdIDhigh,justify = "left",width = 10)        
LowComplexID <- tk2entry(LowIDframe, textvariable= StdIDlowID,justify = "left",width = 10)        


LowID 	<- tk2label(LowIDframe,text = "Parameter ID")
HighID 	<- tk2label(HighIDframe,text = "Parameter ID")
LowIDID <- tk2label(LowIDframe,text = "Protein ID")

tkgrid(HighID, HighComplex,pady = 2,sticky = "E",padx = 2)
tkgrid(LowID, LowComplex,pady = 2,sticky = "E",padx = 2)
tkgrid(LowIDID, LowComplexID,pady = 2,sticky = "E",padx = 2)
tkgrid(HighIDframe,sticky = "E",columnspan = 2)
tkgrid(LowIDframe,sticky = "E",columnspan = 2)

tk2tip(StdID,"Parameter IDs, referred to be used as gold standard samples.\nThese IDs need to be listed in the Parameter Setting Table.")
tk2tip(HighID,"High complex reference sample, e.g. E. coli proteome.")
tk2tip(LowID,"Low complex reference sample, e.g. bovine serumalbumin")

#========


tryError <- class(try(trk <- readLines(paste(path.package("mqqc"),"data/mailSettings",sep = "/"))))
if(tryError == "try-error"){trk <- c("Username","Password","smtp.server","mail@me.mars")}
ttTRK 		<- tk2labelframe(tt3,text = "Mail Settings")
trkLab1 <- tk2label(ttTRK,text = "Username",justify = "left")
trkLab2 <- tk2label(ttTRK,text = "Password",justify = "left")
trkLab3 <- tk2label(ttTRK,text = "smtp.server",justify = "left")
trkLab4 <- tk2label(ttTRK,text = "email address",justify = "left")

tk2tip(ttTRK,"Mail Server Settings.\nRequires Perl and email account that supports SMTP and SASL for authentication.\nIt is recommended to setup a new email account for mqqc usage only.\n WARNING! Safety of password storage in mqqc cannot be warrantied!")


trk1 <- tclVar(trk[1])
trk2 <- tclVar(trk[2])
trk3 <- tclVar(trk[3])
trk4 <- tclVar(trk[4])

TRK1 <- tkentry(ttTRK, textvariable = trk1,background = "white",width = 10)
TRK2 <- tkentry(ttTRK, textvariable = trk2,show = "*",background = "white",width = 10)
TRK3 <- tkentry(ttTRK, textvariable = trk3,background = "white",width = 10)
TRK4 <- tkentry(ttTRK, textvariable = trk4,background = "white",width = 10)

tkgrid(trkLab1,TRK1,sticky = "WE",padx = 2,pady = 2)
tkgrid(trkLab2,TRK2, sticky = "W",padx = 2,pady = 2)
tkgrid(trkLab3,TRK3, sticky = "W",padx = 2,pady = 2)
tkgrid(trkLab4,TRK4, sticky = "W",padx = 2,pady = 2)
tkgrid(StdID ,ttTRK ,sticky = "WNSE",padx = 5,pady=5)


#========================================================

#tkwm.resizable(tt2, "FALSE","FALSE")
temp <- tk2labelframe(ttf1,text = "E-Mail IDs")

	#TKBexport <- tk2button(temp,text = "Mail Settings",command = function(){tkfocus(tt3))})
	TKMail <- tk2button(temp,text = "@ Edit",command = fixMailList)
	TKMailImport <- tk2button(temp,text = "@ import",command = MailImport)

	#TKBload <- tkbutton(ttf1,text = "export Param.Rdata",command = exportRdata)

tkgrid( TKMailImport,TKMail,padx = 3,pady=1,sticky = "WE")
#tk2tip(TKBexport,"Exports set Paths.\nA complete backup  is provided with Backup/Restore all function.")
tk2tip(TKMailImport,"Import a 2 Column table with User ID in first and mail in second column.")
tk2tip(TKMail,"Edit Mailinglist.")

tkgrid(temp,sticky = "NSWE",padx = 2,pady = c(2,2))

tkgrid(tt1,columnspan = 2,padx = 1,pady = 1,sticky = "NSWE")
	tkgrid(ttb,columnspan = 2,padx = 10,pady = 1,sticky = "NSWE")

	tkgrid(tkbutton(tt,width = 5,font = fontHeading,text = "GO",command = function(){tkdestroy(tt)},bg = "#009193",fg = "white"),tkbutton(tt,width = 5,font = fontHeading,text = "STOP",command = function(){ .GlobalEnv$abort  <- T; tkdestroy(tt)},bg = "#ff7e79",fg = "white"),columnspan = 1,padx = 3,pady = 3)

#tk2theme("clearlooks")

tkwait.window(tt)




if(.GlobalEnv$abort ){stop("abort by user")}

x <- 0
Machines <- c()
out <- 1
while(!is.null(out)){
	out <- tclarray[[x,0]]
	x <- x+1
	if(!is.null(out)){
		Machines <- c(Machines,tclvalue(out))
	}
	
}
#print(Machines)

trk <- c(tclvalue(trk1),tclvalue(trk2),tclvalue(trk3),tclvalue(trk4))
if(!.GlobalEnv$MQQCRestored){
write(trk ,paste(path.package("mqqc"),"data/mailSettings",sep = "/"))
}
rm(trk,trk1,trk2,trk3,trk4)

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
		SpeciesTable = T,
		REpar = tclvalue(tb2.Par.GE),
		REmac = tclvalue(tb2.Machine.GE),
		REmail = tclvalue(tb2.Mail.GE),
		REtime = tclvalue(tb2.Time.GE),
		REmisc = tclvalue(tb2.Misc.GE),
		Machines = Machines,
		StdIDhigh = tclvalue(StdIDhigh),
		StdIDlow = tclvalue(StdIDlow),
		BSAID = tclvalue(StdIDlowID),
		TabOrd = tclvalue(tb1.val.pep.TableOrder)
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
		
if(.GlobalEnv$aborttt2){
	
}else{
	if(exists("Backup")){
		if(Backup ){
			
		BackupRestoreSettings("b")
		.GlobalEnv$MQQCRestored <- F
		.GlobalEnv$MQQCRestartNow <- "yes"
		.GlobalEnv$Backup <- F
		tclvalue(tt2.quant.method) <- "nothing"
		}
		rm("Backup")
	}
	
}

return(output)

}

 Param <- mqqcGUI()
 print(Param)
	# while(.GlobalEnv$MQQCRestartNow == "yes"){
  	#	 Param <- mqqcGUI()
	#}
	# # print(Param)
# RESettings <- Param[grep("^RE",names(Param))]
# #save(Param, RESettings,file =  "MQQCGUIsetting.Rdata")


#Param <- mqqcGUI()