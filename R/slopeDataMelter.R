# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the slopeDataMelter() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2559-2579.

slopeDataMelter <- function(inpSlopes) {
  Intercepts <- c()
  Slopes <- c()
  
  
  for (i in inpSlopes) {
    if (is.null(i)) {
      Intercepts <- c(Intercepts, NA)
      Slopes <- c(Slopes, NA)
    }else {
      Intercepts <- c(Intercepts, i[[1]])
      Slopes <- c(Slopes, i[[2]])
    }
    
  }
  
  return(list("Intercepts" = Intercepts,
             "Slopes" = Slopes))
  
  
}
