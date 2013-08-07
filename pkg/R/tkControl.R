tkControl <-
function(info = "MQQC INFO BOX",info2 = "Status..." ,info3 = "", htmloutPath = NULL){


if(!all(is.na(c(info,info2)))){
  	  info <- paste(info,info2,sep = "\n")

require(tcltk)
if(!exists("tt")){
  tt <<- tktoplevel() 
  tkwm.title(tt,"mqqc")
  tkString 	<<- tklabel(tt,text = info)
  tkString2 <<- tklabel(tt,text = info3)
 
  tkgrid(.GlobalEnv$tkString)
  tkgrid(.GlobalEnv$tkString2)
  tt2 <- tkframe(tt)
  TKMail <- tkbutton(tt2,text = "@",command = fixMailList)
TKSpecies <- tkbutton(tt2,text = "Table",command = speciesFunFix)
TKClean <- tkbutton(tt2,text = "Empty Html",command = function(){CleanHtml(htmloutPath)})
tkgrid(TKMail,TKSpecies,TKClean)
tkgrid(tt2)

}
if(tclvalue(tkwinfo("exists",tt)) == "0"){
  tt <<- tktoplevel()
  tkwm.title(tt,"mqqc")
  tkString <<- tklabel(tt,text = info)
  tkString2 <<- tklabel(tt,text = info3)
 
  tkgrid(.GlobalEnv$tkString)
  tkgrid(.GlobalEnv$tkString2)
  
  tt2 <- tkframe(tt)
  TKMail <- tkbutton(tt2,text = "@",command = fixMailList)
TKSpecies <- tkbutton(tt2,text = "Table",command = speciesFunFix)
TKClean <- tkbutton(tt2,text = "Empty Html",command = function(){CleanHtml(htmloutPath)})
tkgrid(TKMail,TKSpecies,TKClean)

tkgrid(tt2)
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
