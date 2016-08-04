CheckFastaDB <- 
function(MQ,CheckFasta){

dbSet <- paste(MQ,"bin","conf","databases.xml",sep = "/")
dbSetF <- readLines(dbSet)
if(length(grep(CheckFasta,dbSetF)) == 0){
  addLinePos <- max(grep("<databases filename",dbSetF))
  addLine <- paste("      <databases filename=",CheckFasta," search_expression=\"(.*)\" replacement_expression=\"%1\" />")
  dbSetF <- c(dbSetF[1:addLinePos],addLine,dbSetF[(addLinePos+1):length(dbSetF)])
  cat("\rDatabases not set. Extending databases.xml!")
  write(dbSetF,dbSet)
}

}