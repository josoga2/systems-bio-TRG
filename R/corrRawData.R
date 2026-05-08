# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the corrRawData() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1132-1162.

corrRawData <- function(variableData) {
  
  allCorMtx <- NULL
  
  corORDER <- c('rep1vrep2', 'rep2vrep3', 'rep1vrep3')
  for (i in 1:6) {
    myCurData <- variableData[[i]]
    corTAB <- data.frame(melt(myCurData[[2]][2:385])$value,
                         melt(myCurData[[3]][2:385])$value,
                         melt(myCurData[[4]][2:385])$value)
    
    names(corTAB) <- plateOrder[2:4]
    COR_MTX <- cor(corTAB)
    tempCorList <- c(COR_MTX[2,1], COR_MTX[2,3], COR_MTX[2,1])
    
    plot(corTAB$Rep1, corTAB$Rep2, pch = 19, col = "#24749B", cex = 0.1, main = corORDER[1])
    plot(corTAB$Rep1, corTAB$Rep3, pch = 19, col = "#F6992F", cex = 0.1, main = corORDER[3])
    plot(corTAB$Rep2, corTAB$Rep3, pch = 19, col = "#22B258", cex = 0.1, main = corORDER[2])
    
    allCorMtx <- cbind(allCorMtx, tempCorList)
  }
  rownames(allCorMtx) <- corORDER
  colnames(allCorMtx) <- lpNum
  
  densPbl <- density(c(melt(allCorMtx)$value))
  plot(densPbl, main = "Correlation Freq Distr.", lwd = 0, col = 'black')
  polygon(densPbl, col = "#F6992F")
  
  corrplot(t(allCorMtx), addCoef.col = 'white', col = COL1('Blues'))
  #print(allCorMtx)
}
