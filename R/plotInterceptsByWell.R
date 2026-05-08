# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plotInterceptsByWell() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1463-1487.

plotInterceptsByWell <- function(completeData, lp, well, repPick=1) {
  
  replicates <- c('rep1', 'rep2', 'rep3')
  
  for (i in replicates[repPick]) {
    #baseplot
    plot(0,0, xlim= c(0,20), ylim = c(-40,10), col = 'white', main = paste0(lp, "_", well))
    #plot A
    with(subset(completeData, ORDER == i & wellName == 'A1' & lpNum == lp ), 
         abline(a = UNT.Y_INTERCEPT, b = UNT.GRate, col="grey", lwd=3))
    #plot B
    with(subset(completeData, ORDER == i & wellName == well & lpNum == lp ), 
         abline(a = UNT.Y_INTERCEPT, b = UNT.GRate, col=1, lwd=3))
    #plot C
    with(subset(completeData, ORDER == i & wellName == 'A1' & lpNum == lp), 
         abline(a = Y_INTERCEPT, b = GRate, col="Chocolate", lwd=3))
    #plot D
    with(subset(completeData, ORDER == i & wellName == well & lpNum == lp ), 
         abline(a = Y_INTERCEPT, b = GRate, col=3, lwd=3))
    
    abline(v =0)
    abline(h =odDetection)
  }
  
}
