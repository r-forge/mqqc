MQdbadder <- 
function(checkMQ.bin,db){
  if(!is.na(db)){
    dbXML <- readLines(dbPath <- paste(checkMQ.bin,"/bin/conf/databases.xml",sep = ""))
    
    CheckDB<- grep(paste("filename=\"",basename(db),sep = ""),dbXML)
    if(length(CheckDB) == 0){
      entry <- paste("      <databases filename=\"",basename(db),"\" search_expression=\"(.*)\" replacement_expression=\"%1\" />",sep = "")
      entrypoint <- grep("</Databases>",dbXML,ignore.case = T)
      XMLOUT <- c(dbXML[1:(entrypoint-1)],entry,dbXML[entrypoint:length(dbXML)])
      cat("\rWARNING: added DB to MQ config xml")  
      write(XMLOUT,dbPath)
    }
  }
}