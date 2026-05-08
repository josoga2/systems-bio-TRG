# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the myDensLinesAd() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2162-2179.

myDensLinesAd <- function(variables, 
                     shadeCol = "#22B258", bordCol = "black",
                     titleR = "", lineWidth = 3,
                     MM = F) {
  
  tempDens <- density(variables, na.rm = T)
  
  lines(tempDens, main = titleR, lwd = lineWidth, col = bordCol)
  polygon(tempDens, col = shadeCol, border = bordCol)
  
  #plot mean and median
  if (MM == T) {
    abline(v = mean(variables, na.rm = T), col = 'red')
    abline(v = median(variables, na.rm = T), col = 'black')
    #add legend for the mean and median
    legend("topright", legend = c('median', 'mean'), col = c('black', 'red'), lty = 1, lwd = 2, inset = 0.1)
  }
}
