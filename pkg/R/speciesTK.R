speciesTK <-
function(x,des=2,dataCol=3 ){
tt2 <- tktoplevel()
tt <- tkframe(tt2)

scr <- tkscrollbar(tt, repeatinterval=5,
				   command=function(...)tkyview(ttbox,...))

ttbox <- tkframe(tt)
listtboxK 			<- list()
buttboxonTK		<- list()
temp <- as.matrix(x)[, dataCol]
.GlobalEnv$mqqcSpeciesSet <- temp
hui <- function(x){
	testI <- x
	tk2button(ttbox,text = "Browse",width = 6,command= function(){
	temp <- tclvalue(tkgetOpenFile()	)
  if(temp !=""){
    tkconfigure(listtboxK[[testI]],text = temp)
    .GlobalEnv$mqqcSpeciesSet[testI] <- temp
  }

	}

	)
}

x <<- x
for(i in 1:dim(x)[1]){
	listtboxK[[i]] <- tklabel(ttbox,text = as.character(x[i, dataCol]))
	tkgrid(tklabel(ttbox,text = as.character(x[i,des])),listtboxK[[i]],hui(i))
}


FunChange <- function(Species){
		AllChange <- tk_choose.dir()
		Files <- as.character(x[, dataCol])
		Files <- basename(Files)
		Files <- paste(AllChange,basename(Files),sep = "/")
		#print(length(			print(listtboxK[[i]])))
	
	apply(cbind(1:length(Files), Files),1,function(y){
					tkconfigure(listtboxK[[as.numeric(y[1])]],text =  y[2])
					.GlobalEnv$mqqcSpeciesSet[as.numeric(y[1])] <- y[2]
	})	
				#print(listtboxK[[i]])
		
}
CDes <- tk2button(tt2,text = "ChangeAll",command = FunChange)
BDes <- tk2button(tt2,text = "Done",command = function(){tkdestroy(tt2)})
tkgrid(tt)
tkgrid(ttbox,scr)
tkgrid(CDes)
tkgrid(BDes)
tkgrid.configure(scr,sticky="ns")
tkwait.window(tt2)
}
