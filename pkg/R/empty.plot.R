empty.plot <- 
function(ylimVec = c(0,1),xlimvec = c(0,1),ylabVec = "",MainVec = "",...)
{plot(1,xlim = xlimvec,type = "n",axes = F,xlab = "",ylab =ylabVec,ylim = ylimVec,main = MainVec,...)}
