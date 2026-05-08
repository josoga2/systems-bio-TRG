# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the medNormFunc() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 190-196.

medNormFunc <- function(strainSub){
  dataMAD <- abs(mad(strainSub, na.rm = T))
  dataMED <- median(strainSub, na.rm = T)
  
  STDRES <- strainSub/dataMED
  return(STDRES)
}
