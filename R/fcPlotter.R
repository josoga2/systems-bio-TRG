# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the fcPlotter() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2333-2353.

fcPlotter <- function(inputFCData) {
  #how to plot the output of foldchange and residuals
  
  #par(mfrow = c(1,2))
  #plot(x = inputFCData$allScores$medTRGDIFFVuntResid, y = -log10(inputFCData$allScores$padj),
  #    cex = 0.5, pch =19, col = 'grey')
  #points(x = inputFCData$Upregulated$medTRGDIFFVuntResid, y = -log10(inputFCData$Upregulated$padj),
  #      cex = 0.5, pch =19, col = 'blue')
  #points(x = inputFCData$downregulated$medTRGDIFFVuntResid, y = -log10(inputFCData$downregulated$padj),
  #      cex = 0.5, pch =19, col = 'blue')
  
  
  plot(x = inputFCData$allScores$medTRGDIFFVuntResid, y = -log10(inputFCData$allScores$padj),
       cex = 0.5, pch =19, col = 'grey')
  points(x = inputFCData$upRegResid$medTRGDIFFVuntResid, y = -log10(inputFCData$upRegResid$padj),
         cex = 0.5, pch =19, col = 'blue')
  points(x = inputFCData$downRegResid$medTRGDIFFVuntResid, y = -log10(inputFCData$downRegResid$padj),
         cex = 0.5, pch =19, col = 'blue')
  
  
}
