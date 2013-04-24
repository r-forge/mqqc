tkControl <-
function(info = "MQQC INFO BOX",info2 = "Status..."){
  info <- paste(info,info2,sep = "\n")
  
require(tcltk)
if(!exists("tt")){
  tt <<- tktoplevel() 
  tkwm.title(tt,"mqqc")
  tkString <<- tklabel(tt,text = info)
  tkgrid(.GlobalEnv$tkString)
}
if(tclvalue(tkwinfo("exists",tt)) == "0"){
  tt <<- tktoplevel()
  tkwm.title(tt,"mqqc")
  tkString <<- tklabel(tt,text = info)
  tkgrid(tkString)
}else{  tkconfigure(.GlobalEnv$tkString,text = info)
}



}