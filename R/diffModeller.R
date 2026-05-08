# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the diffModeller() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1379-1387.

diffModeller <- function(fullDataset) {
  tempData <- list()
  for (plate in fullDataset) {
    plate$X_intercept_diff <- plate$detectionIntercept - mean(subset(plate, wellName %in% allDmsoWell)$detectionIntercept, na.rm =T)
    plate$Y_intercept_diff <- plate$Y_INTERCEPT - mean(subset(plate, wellName %in% allDmsoWell)$Y_INTERCEPT, na.rm = T)
    tempData <- c(tempData, list(plate))
  }
  return(tempData)
}
