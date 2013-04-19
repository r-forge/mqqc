## MQ run Check for new files

## If new file start MQcmd

## modify mqpar.xml with parameters of new file
path 				<- "/Users/cRacker-DEMO/aaaaaaatakeo"
list.files.dummy 	<- "test.raw"
xmlNEW 			<- xml.replace(c("filePaths"),paste(path,list.files.dummy,sep = "/"),mqpar)

if(1==0){
	write(xml.new,file = "temp.xml")
	system("MAXQUANTcmd xml.new path")	
}

## maybe path needs to be set du backslashes
temp.path <- mq.path.extract("filePaths",xmlNEW)
if(length(grep("\\", temp.path,fixed = T)) > 0){
	temp.path <- path.convert(temp.path)
}
evidence.path <- search.evidence(dirname(temp.path))
start.qc(evidence.path,n=1)


## new