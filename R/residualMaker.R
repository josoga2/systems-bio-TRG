# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the residualMaker() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1490-1496.

residualMaker <- function(fullProcData, newColName, colX, colY) {
  varX <- fullProcData[[colX]]
  varY <- fullProcData[[colY]]
  
  fullProcData[[newColName]] <- resid(lm(varY-varX ~ 0))
  return(fullProcData)
}
