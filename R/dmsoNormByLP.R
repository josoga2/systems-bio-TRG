# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the dmsoNormByLP() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2240-2275.

dmsoNormByLP <- function(antibiotics, untVar, treatVar, varColTitle) {
  
  treatment = NULL
  if (antibiotics == 'amik') {
    treatment = amikacinCompleteDataAll
  }else if(antibiotics == 'amox'){
    treatment = amoxicillinCompleteDataAll
  }else if(antibiotics == 'cipro'){
    treatment = ciproCompleteDataAll
  }
  
  tempResult <- NULL
  
  for (lp in unique(treatment$lpNum)) {
    #current Library plate
    #print(lp)
    thisLPAlone <- subset(treatment, lpNum == lp)
    #untreated column
    thisLPAlone.UNT <- thisLPAlone[untVar]
    #treated column
    thisLPAlone.TREAT <- thisLPAlone[treatVar]
    #dmso data mean for untreated
    thisLPAlone.DMSO.UNT <- mean(subset(thisLPAlone, wellName %in% allDmsoWell)[[untVar]])
    #dmso data mean for treated
    thisLPAlone.DMSO.TREAT <- mean(subset(thisLPAlone, wellName %in% allDmsoWell)[[treatVar]])
    
    #solve for treated
    thisLPAlone[paste(varColTitle)] <- thisLPAlone.TREAT - thisLPAlone.DMSO.TREAT
    #solve for untreated
    thisLPAlone[paste0('UNT.',varColTitle)] <- thisLPAlone.UNT - thisLPAlone.DMSO.UNT
    
    tempResult <- rbind(tempResult, thisLPAlone)
  }
  
  return(tempResult)
}
