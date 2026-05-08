# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the trgVerdictFixer() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2504-2532.

trgVerdictFixer <- function(yoyoyo) {
  
  fixed.TRG <- c()
  fixed.UNT.TRG <- c()
  for (ir in 1:length(rownames(yoyoyo))) {
    currRow <- yoyoyo[ir,]
    if (currRow$TRG == 0 & currRow$Verdict=="Growing") {
      fixed.TRG <- c(fixed.TRG, currRow$TRG)
    }else if(currRow$TRG > 0 & currRow$Verdict=="Growing"){
      fixed.TRG <- c(fixed.TRG, currRow$TRG)
    }else {
      fixed.TRG <- c(fixed.TRG, NA)
    }
    
    if (currRow$UNT.TRG == 0 & currRow$UNT.Verdict=="Growing") {
      fixed.UNT.TRG <- c(fixed.UNT.TRG, currRow$UNT.TRG)
    }else if(currRow$UNT.TRG > 0 & currRow$UNT.Verdict=="Growing"){
      fixed.UNT.TRG <- c(fixed.UNT.TRG, currRow$UNT.TRG)
    }else {
      fixed.UNT.TRG <- c(fixed.UNT.TRG, NA)
    }
    
  }
  
  yoyoyo$fixed.TRG <- fixed.TRG
  yoyoyo$fixed.UNT.TRG <- fixed.UNT.TRG
  
  return(yoyoyo)
}
