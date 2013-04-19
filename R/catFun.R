catFun <- 
function(x,n=50)
{
  if(nchar(x) > n){
    n = nchar(x)
  }
  
  cat("\r",x,rep(" ",(n - nchar(x))))
  
}