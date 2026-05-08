# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the zScorer() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 150-175.

zScorer <- function(plateDataset) {
  std_rep <- c()
  
  for (plateN in 1:length(plateDataset)) {
    curr_plate <- c()
    curr_plate_mmScore <- c()
    tempData <- plateDataset[[plateN]]
    wellMean <- mean(tempData$TRG, na.rm =T)
    wellSD <- sd(tempData$TRG, na.rm = T)
    tempDataMin <- min(tempData$TRG, na.rm = T)
    tempDataMax <- max(tempData$TRG, na.rm = T)
    
    for (well in c(tempData$TRG)) {
      std_score <- (well - wellMean)/wellSD
      minMaxScore <- (well-tempDataMin)/(tempDataMax-tempDataMin)
      curr_plate <- c(curr_plate, std_score)
      curr_plate_mmScore <- c(curr_plate_mmScore, minMaxScore)
    }
    tempData$ZScore <- curr_plate
    tempData$mmScore <- curr_plate_mmScore
    std_rep <- c(std_rep, list(tempData))
  }
  
  
  return(std_rep)
}
