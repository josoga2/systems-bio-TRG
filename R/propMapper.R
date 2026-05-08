# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the propMapper() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3196-3199.

propMapper <- function(queryValue, queryColumn = 'Abbv', targetColumn) {
  soln = drugMap[[targetColumn]][which(drugMap[[queryColumn]] == queryValue)]
  return(soln[1])
}
