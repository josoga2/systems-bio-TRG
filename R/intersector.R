# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the intersector() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1630-1638.

intersector <- function(classification) {
  amkX = subset(amikacinCompleteDataAll, Classification == classification)$compoundID
  cipX = subset(ciproCompleteDataAll, Classification == classification)$compoundID
  amxX = subset(amoxicillinCompleteDataAll, Classification == classification)$compoundID
  print(ggvenn(list(cip=cipX, amk = amkX, amx = amxX), show_percentage = F, text_size = 6))
  return(list(amik = setdiff(amkX, c(cipX, amxX)),
              amox = setdiff(amxX, c(cipX, amkX)),
              cipr = setdiff(cipX, c(amkX, amxX)))) 
}
