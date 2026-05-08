# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the lmCalcResiduals() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2471-2491.

lmCalcResiduals <- function(antibiotics, varVone, newColName) {
  #specific for med_TRG_DIFF
  
  treatment = NULL
  if (antibiotics == 'amik') {
    treatment = amikacinCompleteDataAll
  }else if(antibiotics == 'amox'){
    treatment = amoxicillinCompleteDataAll
  }else if(antibiotics == 'cipro'){
    treatment = ciproCompleteDataAll
  }
  
  if (varVone == F) {
    treatment[newColName] <- resid(lm(data = treatment,formula =
                                        med_TRG_DIFF~UNT.med_TRG_DIFF))
  }else {
    treatment[newColName] <- resid(lm(data = treatment,formula =
                                        med_TRG_DIFF-UNT.med_TRG_DIFF~0))
  }
  return(treatment) 
}
