# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the dmsoSelector() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1505-1520.

dmsoSelector <- function(fullDataset, treatment_No, ColToSelect) {
  #1 = unt
  #2,3,4 = rep1, rep2, rep3
  
  tempDF <- unique(fullDataset[[1]][[treatment_No]])['wellName']
  tempDF$LP1 <- unique(fullDataset[[1]][[treatment_No]])[ColToSelect]
  tempDF$LP2 <- fullDataset[[2]][[treatment_No]][ColToSelect]
  tempDF$LP3 <- fullDataset[[3]][[treatment_No]][ColToSelect]
  tempDF$LP4 <- fullDataset[[4]][[treatment_No]][ColToSelect]
  tempDF$LP5 <- fullDataset[[5]][[treatment_No]][ColToSelect]
  tempDF$LP6 <- fullDataset[[6]][[treatment_No]][ColToSelect]
  
  tempDF <- subset(tempDF, wellName %in% allDmsoWell)
  
  return(tempDF)
}
