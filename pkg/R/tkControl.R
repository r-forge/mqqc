tkControl <-
function(info = "MQQC INFO BOX",info2 = "Status..." ,info3 = ""){

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
}
if(tclvalue(tkwinfo("exists",tt)) == "0"){
  tt <<- tktoplevel()
  tkwm.title(tt,"mqqc")
  tkString <<- tklabel(tt,text = info)
  tkString2 <<- tklabel(tt,text = info3)
 
  tkgrid(.GlobalEnv$tkString)
  tkgrid(.GlobalEnv$tkString2)
}else{  

tkconfigure(.GlobalEnv$tkString,text = info)
  if(info3!=""){

	tkconfigure(.GlobalEnv$tkString2,text = info3)
}
}
}else{
	print("hui")
  if(info3!=""){
	  try(tkconfigure(.GlobalEnv$tkString2,text = info3))
  }
}



}
