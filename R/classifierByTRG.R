# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the classifierByTRG() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1606-1626.

classifierByTRG <- function(completeDataAll, greenGuys, redGuys) {
  resGroup <- c()
  for (vali in 1:nrow(completeDataAll)) {
    
    if(completeDataAll$compoundID[vali] %in% greenGuys) {
      resGroup <- c(resGroup, "ClassIII")
    }else if(completeDataAll$compoundID[vali] %in% redGuys) {
      resGroup <- c(resGroup, "ClassIV")
      
    }else if (completeDataAll$TRG[vali] == 0 & completeDataAll$UNT.TRG[vali] == 0) {
      resGroup <- c(resGroup, "Killers")
    }else if(completeDataAll$TRG[vali] == 0 & completeDataAll$UNT.TRG[vali] != 0) {
      resGroup <- c(resGroup, "Extenders")
    }else{
      resGroup <- c(resGroup, "Regular")
    }
  }
  
  completeDataAll$Classification <- resGroup  
  return(completeDataAll)
}
