tkControl <-
function(info = "MQQC INFO BOX",info2 = "Status..." ,info3 = "", htmloutPath = NULL){
FunBut <- function(){print("Stopping MQQC");xSleepTime <<- 10;loop <<- F}

col <- c("#FFFFFF","#426787")

if(!all(is.na(c(info,info2)))){
  	  info <- paste(info,info2,sep = "\n")

require(tcltk)
if(!exists("tt")){
tt <<- tktoplevel(bg =col[2])
  tkwm.title(tt,"mqqc")
  tkString 	<<- tklabel(tt,text = info,bg = col[2],fg = col[1])
  tkString2 <<- tklabel(tt,text = info3,bg = col[2],fg = col[1])
 
  tkgrid(.GlobalEnv$tkString)
  tkgrid(.GlobalEnv$tkString2)
  tt2 <- tkframe(tt)
  TKMail <- tkbutton(tt2,text = "@",command = fixMailList)
TKSpecies <- tkbutton(tt2,text = "Table",command = speciesFunFix)
TKClean <- tkbutton(tt2,text = "Empty Html",command = function(){CleanHtml(htmloutPath)})
tkgrid(TKMail,TKSpecies,TKClean)
tkgrid(tt2)
tkgrid(tkbutton(tt,text = "Stop MQQC",command = FunBut,bg = "#ff7e79",fg =  "white"),columnspan = 3,pady = 5)

}
if(tclvalue(tkwinfo("exists",tt)) == "0"){
  tt <<- tktoplevel(bg =col[2])
  tkwm.title(tt,"mqqc")
  tkString <<- tklabel(tt,text = info,bg = col[2],fg = col[1])
  tkString2 <<- tklabel(tt,text = info3,bg = col[2],fg = col[1])
 
  tkgrid(.GlobalEnv$tkString)
  tkgrid(.GlobalEnv$tkString2)
  
  tt2 <- tkframe(tt,bg = col[2])
  TKMail <- tkbutton(tt2,text = "@",command = fixMailList)
TKSpecies <- tkbutton(tt2,text = "Table",command = speciesFunFix)
TKClean <- tkbutton(tt2,text = "Empty Html",command = function(){CleanHtml(htmloutPath)})
tkgrid(TKMail,TKSpecies,TKClean)

tkgrid(tt2)
tkgrid(tkbutton(tt,text = "Stop MQQC",command = FunBut,bg = "#ff7e79",fg =  "white"),columnspan = 3,pady = 5)

}else{  

tkconfigure(.GlobalEnv$tkString,text = info)
  if(info3!=""){

	tkconfigure(.GlobalEnv$tkString2,text = info3)
}
}
}else{
  if(info3!=""){
	  try(tkconfigure(.GlobalEnv$tkString2,text = info3))
  }
}



}
