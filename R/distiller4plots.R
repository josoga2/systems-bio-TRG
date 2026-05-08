# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the distiller4plots() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1012-1029.

distiller4plots <- function(fullDataset, ColToSelect) {
  plateOrder <- c('unt','rep1','rep2','rep3')
  tempdf <- NULL
  for (lp in fullDataset) {
    
    n <- 1
    for (plate in lp) {
      newGen <- plate[c(c(ColToSelect), 'compoundID', 'lpNum', 'PlateTreatment','wellName')]
      UNT <- lp[[1]][,c(ColToSelect)]
      newGen$ORDER <- plateOrder[n]
      newGen <- cbind(newGen, 'UNT' = UNT)
      tempdf <- rbind(tempdf, newGen)
      n <- n+1
    }
  }
  tempdf <- subset(tempdf, ORDER != 'unt')
  return(tempdf)
}
