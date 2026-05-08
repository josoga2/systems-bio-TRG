# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plotIntercepts() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1438-1460.

plotIntercepts <- function(lp, repl = 1) {
  plateToPick = repl+1
  
  for (rowID in 1:384) {
    plot(0,0, xlim= c(0,20), ylim = c(-20,10), col = 'white')
    with(subset(lp[[1]], wellName == 'A1'), abline(a = Y_INTERCEPT, b = GRate))
    if (!is.na(lp[[1]][rowID,]$TRG)) {
      with(lp[[1]][rowID,], abline(a = Y_INTERCEPT, b = GRate))
    } else {
      abline(0,0)
    }
    
    with(subset(lp[[plateToPick]], wellName == 'A1'), abline(a = Y_INTERCEPT, b = GRate, col = plateToPick))
    if (!is.na(lp[[plateToPick]][rowID,]$TRG)) {
      with(lp[[plateToPick]][rowID,], abline(a = Y_INTERCEPT, b = GRate, col = plateToPick))
    } else {
      abline(0,0)
    }
    
    abline(v =0)
    abline(h =odDetection)
  }
}
