# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plotdirWel() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3104-3118.

plotdirWel <- function(plNam, 
                       welNam,
                       plLoc = "/Users/josoga2/Documents/wale_docs/phd/data/Joana/Hand_over_JO/Single_plates/",
                       col = "black") {
  loc <- paste0(
    plLoc, 
    plNam)
  
  datSor <- read.delim(loc, header = T)
  #print(head(datSor))
  for (wel in welNam) {
    lines(datSor$Time, datSor[,wel], pch = 19, col = col)
  }
  
}
