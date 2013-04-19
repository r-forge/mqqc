listFiles <-
function(folder){
	temp.files 	<- file.info(list.files(folder))
	temp.files 	<- rownames(temp.files)[!temp.files$isdir]
}
