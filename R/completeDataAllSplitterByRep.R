# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the completeDataAllSplitterByRep() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2535-2556.

completeDataAllSplitterByRep <- function(antibiotics, colToSplit, untColToSplit) {
  
  treatment = NULL
  if (antibiotics == 'amik') {
    treatment = amikacinCompleteDataAll
  }else if(antibiotics == 'amox'){
    treatment = amoxicillinCompleteDataAll
  }else if(antibiotics == 'cipro'){
    treatment = ciproCompleteDataAll
  }
  
  dataReady <- data.frame(
    'compoundID' = subset(treatment, ORDER.x == 'rep1')[['compoundID.x']],
    'lpNum' = subset(treatment, ORDER.x == 'rep1')[['lpNum.x']],
    'UNT' = subset(treatment, ORDER.x == 'rep1')[[untColToSplit]],
    'REP1' = subset(treatment, ORDER.x == 'rep1')[[colToSplit]],
    'REP2' = subset(treatment, ORDER.x == 'rep2')[[colToSplit]],
    'REP3' = subset(treatment, ORDER.x == 'rep3')[[colToSplit]]
  )
  
  return(dataReady)
}
