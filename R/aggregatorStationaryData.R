# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the aggregatorStationaryData() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1699-1723.

aggregatorStationaryData <- function(stationaryInput, Parameter) {
  rep1Soln <- c()
  rep2Soln <- c()
  rep3Soln <- c()
  compoundIDAll <- c()
  
  for (lp in unique(stationaryInput$lpNum)) {
    splitStatData <- subset(stationaryInput, lpNum==lp)
    untDMSO <- mean(subset(splitStatData[c(1:384),], colName %in% allDmsoWell)[[Parameter]], na.rm = T)
    rep1 <- splitStatData[c(385:768),][[Parameter]]
    rep2 <- splitStatData[c(769:1152),][[Parameter]]
    rep3 <- splitStatData[c(1153:1536),][[Parameter]]
    
    rep1Soln <- c(rep1Soln, c(untDMSO-rep1))
    rep2Soln <- c(rep2Soln, c(untDMSO-rep2))
    rep3Soln <- c(rep3Soln, c(untDMSO-rep3))
    compoundIDAll <- c(compoundIDAll, splitStatData[c(385:768),]$compoundID)
    
  }
  aggregatorStationaryDataFrame <- data.frame('compoundID' = compoundIDAll,
                                              'Rep1' = rep1Soln,
                                              'Rep2' = rep2Soln,
                                              'Rep3' = rep3Soln)
  return(aggregatorStationaryDataFrame)
}
