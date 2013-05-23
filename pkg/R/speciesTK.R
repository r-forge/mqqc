speciesTK <-
function(x,des=2,dataCol=3 ){
tt <- tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
				   command=function(...)tkyview(ttbox,...))

ttbox <- tkframe(tt)
listtboxK 			<- list()
buttboxonTK		<- list()
temp <- as.matrix(x)[, dataCol]
.GlobalEnv$mqqcSpeciesSet <- temp
hui <- function(x){
	testI <- x
	tkbutton(ttbox,text = "Browse",width = 6,command= function(){
	temp <- tclvalue(tkgetOpenFile()	)
  if(temp !=""){
    tkconfigure(listtboxK[[testI]],text = temp)
    .GlobalEnv$mqqcSpeciesSet[testI] <- temp
  }

	}

	)
}
for(i in 1:dim(x)[1]){
	listtboxK[[i]] <- tklabel(ttbox,text = as.character(x[i, dataCol]))
	tkgrid(tklabel(ttbox,text = as.character(x[i,des])),listtboxK[[i]],hui(i))
}
BDes <- tkbutton(tt,text = "Done",command = function(){tkdestroy(tt)})
tkgrid(ttbox,scr)
tkgrid(BDes)
tkgrid.configure(scr,sticky="ns")
tkwait.window(tt)
}
