ASCIIprofileplot <- 
function(plotData, wv = 70){
if(length(grep("txtplot",library())) > 0){
	require("txtplot")
	
	
sink("ASCIIplot.txt")
cat("Elution Profile\n")
try(txtplot(plotData$profile[,1],plotData$profile[,2],ylab = "m/z",xlab = "Retention Time in min",height = wv*0.5,width = wv))
cat("\n\nPeptide Density over time\n")
try(txtplot(plotData$retentionTime[,1],plotData$retentionTime[,2],ylab = "density",xlab = "Retention Time in min",width = wv))
cat("\n\nPeptide m/z Density over time\n")
try(txtplot(plotData$mz[,1],plotData$mz[,2],ylab = "density",xlab = "m/z",width = wv)
)
if(length(plotData$BSAretentionTime) > 0){
cat("\n\nBSA Peptide Density over time\n")
try(txtplot(plotData$BSAretentionTime[,1],plotData$BSAretentionTime[,2],ylab = "Density",xlab = "Retention Time in min",width = wv)
)}
sink()

}else{
	cat("No txtplot package found.")
}
}
#ASCIIprofileplot(TotalScoreRes$plotData)
#system("open ./ASCIIplot.txt")