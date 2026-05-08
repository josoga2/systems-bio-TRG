# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the getXiNtercept() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1395-1409.

getXiNtercept<- function(fullDataset) {
  tempData <- list()
  
  unt <- fullDataset[[1]]$X_intercept_diff #think about this
  unty <- fullDataset[[1]]$Y_intercept_diff
  
  for (plate in fullDataset) {
    curveCDetectionIntercept <- mean(subset(plate, wellName %in% allDmsoWell)$detectionIntercept, na.rm =T) #mean of DMSO wells
    
    plate$PredXInterceptTreated <- unt+ curveCDetectionIntercept
    plate$PredYInterceptTreated <- unty+ plate$Y_INTERCEPT
    tempData <- c(tempData, list(plate))
  }
  return(tempData)
}
