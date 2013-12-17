listFolders <-
function(folder,full = TRUE,pattern = NULL,rec = F){
	temp.files 	<- file.info(list.files(folder,full.name = full,pattern = pattern,recursive = rec))

	temp.files 	<- rownames(temp.files)[temp.files$isdir]
}
