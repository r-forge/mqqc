listFolders <-
function(folder,full = TRUE,pattern = NULL){
	temp.files 	<- file.info(list.files(folder,full.name = full,pattern = pattern))

	temp.files 	<- rownames(temp.files)[temp.files$isdir]
}
