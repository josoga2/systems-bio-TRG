# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the dvForm() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1419-1429.

dvForm <- function(fullDataset) {
  tempData <- list()
  odDetection <- 0.01113222
  unt <- fullDataset[[1]]
  for (plate  in fullDataset) {
    plate$davForm <- (log(odDetection)-(subset(plate, wellName == 'A1')$Y_INTERCEPT + unt$Y_INTERCEPT - subset(unt, wellName == 'A1')$Y_INTERCEPT))/plate$GRate
    
    tempData <- c(tempData, list(plate))
  }
  return(tempData)
}
