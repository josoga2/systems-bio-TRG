# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the odDetectoR() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1364-1373.

odDetectoR <- function(fullDataset) {
  odDetection <- 0.01113222
  tempData <- list()
  for (plate in fullDataset) {
    plate$X_INTERCEPT <- (-1*plate$Y_INTERCEPT)/plate$GRate
    plate$detectionIntercept <- (odDetection - plate$Y_INTERCEPT)/plate$GRate
    tempData <- c(tempData, list(plate))
  }
  return(tempData)
}
