listFolders <-
function(folder,full = TRUE){
	temp.files 	<- file.info(list.files(folder,full.name = full))

	temp.files 	<- rownames(temp.files)[temp.files$isdir]
}
