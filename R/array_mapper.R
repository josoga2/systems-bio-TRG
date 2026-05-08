# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the array_mapper() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3203-3217.

array_mapper <- function(inp.array, c_scheme) {
  cmap <- c()
  localFactors <- c()
  arr.uniq <- unique(inp.array)
  
  for (i in 1:length(inp.array)) {
    #print(i)
    nnum <- which(arr.uniq == inp.array[i])
    cmap <- c(cmap, c_scheme[nnum])
    localFactors <- c(localFactors, nnum)
  }
  
  return(list("ColorMap" = cmap, 'Factors' = localFactors))
  
}
