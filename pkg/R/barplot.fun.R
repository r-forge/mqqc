barplot.fun <-
function(x,name.lab ,max.val){
x[is.infinite(x)] <- NA
temp.b <- barplot(x[order(temp.time)],names.arg = temp.time[order(temp.time)] ,las = 2,main = paste(unique(name.collect)[g],"\n",name.lab),ylim = c(0, max.val))
grid()
abline(v= temp.b[as.logical(temp.extra[order(temp.time)])],col = 2)
abline(h=median(x[order(temp.time)],na.rm = T),col = "darkgreen" )
legend("topright",legend = median(x[order(temp.time)],na.rm = T))
}
