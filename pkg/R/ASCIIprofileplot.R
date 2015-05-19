ASCIIprofileplot <- 
function(plotData,AllPepsData = NULL, wv = 120){
if(length(grep("txtplot",library())) > 0){
	require("txtplot")
	
	
sink("ASCIIplot.txt")
if(length(AllPepsData) == 0){
try(txtplot(plotData$profile[,1],plotData$profile[,2],ylab = "m/z",xlab = "time [min]",height = wv*0.5,width = wv))
}else{
  
  cat("\n\nPeptide Density over time\n")
  cat("\n\nChromatogram All Peaks\n")
  
  try(txtplot(AllPepsData[,1],AllPepsData[,2],ylab = "Intensity",xlab = "time [min]",width = wv))
  cat("Identified Peptides\n")
  
  try(txtplot(plotData$profile[,1],plotData$profile[,2],ylab = "m/z",xlab = "time [min]",height = wv*0.5,width = wv))
  
  try(txtplot(plotData$retentionTime[,1],plotData$retentionTime[,2],ylab = "density",xlab = "Retention Time in min",width = wv))
  cat("\n\nPeptide m/z Density over time\n")
}

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
# ASCIIprofileplot(TotalScoreRes$plotData,AllPepsData)
# system("open ./ASCIIplot.txt")
# 
# plotData <- TotalScoreRes$plotData
# AllPepsData = ChrPath$all
