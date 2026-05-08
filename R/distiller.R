# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the distiller() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 995-1010.

distiller <- function(fullDataset, ColToSelect) {
  plateOrder <- c('unt','rep1','rep2','rep3')
  tempdf <- NULL
  for (lp in fullDataset) {
    
    n <- 1
    for (plate in lp) {
      newGen <- plate[c(ColToSelect, 'compoundID', 'lpNum', 'PlateTreatment','wellName')]
      newGen$UNT <- lp[[1]][[ColToSelect]]
      newGen$ORDER <- plateOrder[n]
      tempdf <- rbind(tempdf, newGen)
      n <- n+1
    }
  }
  return(tempdf)
}
