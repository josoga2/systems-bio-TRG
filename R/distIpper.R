# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the distIpper() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1031-1048.

distIpper <- function(fullDataset, ColToSelect) {
  plateOrder <- c('unt','rep1','rep2','rep3')
  tempdf <- NULL
  for (lp in fullDataset) {
    
    
    
    newGen <- lp[[1]][c('compoundID', 'lpNum', 'wellName')]
    newGen$UNT <- lp[[1]][[ColToSelect]]
    newGen$REP1 <- lp[[2]][[ColToSelect]]
    newGen$REP2 <- lp[[3]][[ColToSelect]]
    newGen$REP3 <- lp[[4]][[ColToSelect]]
    tempdf <- rbind(tempdf, newGen)
    
    
  }
  return(tempdf)
}
