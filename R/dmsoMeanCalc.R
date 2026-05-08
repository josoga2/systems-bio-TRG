# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the dmsoMeanCalc() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1522-1525.

dmsoMeanCalc <- function(FullDataset, Var) {
  DMSO_DAT <- subset(FullDataset, allDmsoWell %in% wellName)
  return(mean(DMSO_DAT[[Var]], na.rm=T))
}
