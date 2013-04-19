list.files.nodir <- 
function(path = ".",pattern = NULL)
{
	tempx <- list.files(path,pattern)
	try(tempx <- tempx[!file.info(tempx)$isdir])
	return(tempx)
	
}