displayInTable <- 
function(tclarray,title="",height=6,width=-4,nrow=-1,ncol=-1,tt)
{
tclRequire("Tktable")


backup <- tclarray	
Add <- function(text){
tkinsert(table1,"rows","end",1)
	
}
Remove <- function(text){
	tkdelete(table1,"rows","end",1)

}


hu <- tk2frame(tt)
PlusB		<- tk2button(hu,text = "+",command = Add)
MinusB 	<- tk2button(hu,text = "-",command = Remove)
	tkgrid(PlusB,sticky = "WE",padx = 6,pady = 4)
	tkgrid(MinusB,sticky = "WE",padx = 6,pady = 4)


  table1 <- tkwidget(tt,"table",rows=nrow,cols=ncol,
                     height=height+1,width=width+1,yscrollcommand=function(...) tkset(yscr,...),colwidth=14)
  #xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tk2scrollbar(tt,command=function(...)tkyview(table1,...), orient = "vertical")

  tkgrid(hu,table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  #tkgrid(xscr,sticky="new")
  tkconfigure(table1,variable=tclarray,background="white",selectmode="extended")
  
  # tkgrid(tkbutton(tt,text = "Reload",command = function(){  tkconfigure(table1,variable=tclarray,background="white",selectmode="extended")
# }))
  
  
  #tkgrid(tkbutton(tt,text = "Done",command = function(){tkdestroy(tt)}))

  return (table1)
}
 	#Param <- mqqcGUI()

#Param <- mqqcGUI()
