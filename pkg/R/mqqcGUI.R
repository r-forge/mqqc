mqqcGUI <- 
function(){
library(tcltk)

try(load(file=paste(path.package("mqqc"),"data/Param.Rdata",sep = "/")))
if(!exists("output")){
	output <- list(	
		MQ = "",
		folder ="",
		fastaFile = "",
		htmloutPath = "",
		DeleteFiles =0,
		cores = "auto" ,
		SpeciesTable = T
		)}

fontHeading <- tkfont.create(family = "Tahoma",size=15,weight="bold")

  checkMQ <- list.files(paste(path.package("mqqc"),"data",sep ="/"),pattern = "MQpath",full.name = T)
if(length(checkMQ)> 0){
	MQpath <- tclVar(readLines(checkMQ))
}else{MQpath <- tclVar("MQ-path")}

tt <- tktoplevel()

ttf1 <- tkframe(tt)
ttf2 <- tkframe(tt)

MQpath <- tclVar(output$MQ)

tkgrid(tklabel(tt,text = "MQQC",font = fontHeading),columnspan = 2)
dirFrame <- ttklabelframe(ttf1,text = "MaxQuant Folder")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(){
        File <- tclvalue(tkchooseDirectory(parent = tt,title = "MaxQuant Folder"))
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
	onBrowse2 <- function(){
        File <- tclvalue(tkgetOpenFile(parent = tt,title = "Default Fasta File"))
        
         if(File != ""){
        tclvalue(StandardFasta) <- File
        } 
        }
    browseButton2 <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse2, 	borderwidth=3)
    
    
    locationField <- ttkentry(dirFrame, textvariable=StandardFasta,justify = "right")        
    tkgrid(browseButton2,locationField)
	tkgrid(dirFrame,padx = 5,pady = 5)


Analysis.Folder <- tclVar(output$folder)
dirFrame <- ttklabelframe(ttf1,text = "Analysis Folder")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(){
        File <- tclvalue(tkchooseDirectory(parent = tt,title = "Analysis Folder"))
          if(File != ""){
        tclvalue(Analysis.Folder) <- File
        } 
        }
    browseButton <- buttonRcmdr(dirFrame, text=gettext("Browse", domain="R-Rcmdr"), command=onBrowse, 	borderwidth=3)
    
    
    locationField <- ttkentry(dirFrame, textvariable=Analysis.Folder,justify = "left")        
    tkgrid(browseButton,locationField)
	tkgrid(dirFrame,padx = 5,pady = 5)
	

	
	
	
	########
	# other buttons
	########
	
	cb <- tkcheckbutton(ttf2)
	cbVar <- tclVar(0)
	tkconfigure(cb,variable=cbVar)
	tkgrid(tklabel(ttf2,text = "Clean up"),cb)
	
	
	######
	# cores
	######
	
	tb1.duplicates.var 					<- c("auto",1:24)
	tb1.val.pep.duplicates 				<- tclVar()  
	tclvalue(tb1.val.pep.duplicates) 	<-"auto"
	comboBox 							<- ttkcombobox(ttf2,values=tb1.duplicates.var,textvariable = tb1.val.pep.duplicates,width = 17,state = "readonly",width = 6)
	tkgrid(tklabel(ttf2,text = "Cores"),comboBox,pady = 2)
	
	
	####
	# set fasta files
	####
speciesFun <- 	function(){
		     species.path<- list.files(path.package("mqqc"),pattern = "MQQCspecies.csv$",full.name = T,recursive = T)
			 species <- read.csv(species.path)
			 try(speciesTK(species))	
				species$Fasta <- .GlobalEnv$mqqcSpeciesSet
              write.csv(species,file =paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"),quote = F, row.names = F)
		
	}
	
	
	fastaSet <- tkbutton(ttf2,text = "Check",command = speciesFun)
	tkgrid(tklabel(ttf2,text = "Set all Fasta"),fastaSet,pady = 2)
	
	####
	# HTML
	####
	HTML <- tclVar(output$htmloutPath)
dirFrame <- ttklabelframe(ttf1,text = "HTML Path")

	buttonRcmdr <- function(..., borderwidth, fg, foreground, relief) ttkbutton(...)
	onBrowse <- function(){
		File <- tclvalue(tkchooseDirectory(parent = tt,title = "HTML Folder"))
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
tkgrid(tkbutton(tt,text = "go",command = function(){tkdestroy(tt)}),tkbutton(tt,text = "stop",command = function(){ .GlobalEnv$abort  <- T; tkdestroy(tt)}),columnspan = 1,padx = 3,pady = 3)
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
		cores = cores ,
		SpeciesTable = T
		)
save(output,file=paste(path.package("mqqc"),"data/Param.Rdata$",sep = "/"))
# check settings
print(output)
for(i in 1:4){
	tempI <- as.character(output[[i]])
	tempI <- list.files(dirname(tempI),pattern= basename(tempI))
	print(tempI)
	if(length(tempI) == 0){
		output[[i]] <- ""
		
	}
}
		
		

return(output)

}

#hui <- mqqcGUI()
#print(hui)
