stringSplitter <- function(x,RESettings,placeholder = "PLACEHOLDER"){

TimeVec <- grepRE(x,RESettings$REtime)
TimeVec <- gsub("_","", TimeVec)
MachineVec <- grepRE(x,RESettings$REmac)
x <- gsub(".raw","_raw",x)

RE <- gsub(placeholder,"[^_]*", RESettings$REpar)
SpeciesTag <- grepRE(x,RE)
SpeciesTag <- sapply(strsplit(SpeciesTag,"_"),function(x){x[length(x)]})


RE <- gsub(placeholder,"[^_]*", RESettings$REmail)
UserTag <- grepRE(x,RE)
UserTag <- sapply(strsplit(UserTag,"_"),function(x){x[length(x)]})

RE <- gsub(placeholder,"[^_]*", RESettings$REtime)
TimeTag <- grepRE(x,RE)
TimeTag <- gsub("_","", TimeTag)


RE <- RESettings$REmisc
MiscTag <- grepRE(x,RE,invert = T)
MiscTag <- gsub("^_","", MiscTag)
MiscTag <- gsub(".raw$","", MiscTag,ignore.case = T)
	
	
TabInfo <- cbind(MachineVec,TimeTag,UserTag,SpeciesTag, MiscTag)
return(TabInfo)
}
#temp <- stringSplitter(as.character(collectListSorted$Name),RESettings)
