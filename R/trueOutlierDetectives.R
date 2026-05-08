# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the trueOutlierDetectives() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1164-1191.

trueOutlierDetectives <- function(fullDataset, n = 2) {
  
  untcut <- 1.5*IQR(fullDataset$UNT, na.rm = T) + 
    quantile(fullDataset$UNT, na.rm = T)[[4]] 
  
  rep1cut <- (1.5*IQR(fullDataset$REP1, na.rm = T) + 
                quantile(fullDataset$REP1, na.rm = T)[[4]])*n 
  
  rep2cut <- (1.5*IQR(fullDataset$REP2, na.rm = T) + 
                quantile(fullDataset$REP2, na.rm = T)[[4]])*n 
  
  rep3cut <- n*(1.5*IQR(fullDataset$REP3, na.rm = T) + 
                  quantile(fullDataset$REP3, na.rm = T)[[4]])*n 
  
  
  untOut <- subset(fullDataset, UNT > untcut)$compoundID
  rep1Out <- subset(fullDataset, REP1 > rep1cut)$compoundID
  rep2Out <- subset(fullDataset, REP2 > rep2cut)$compoundID
  rep3Out <- subset(fullDataset, REP3 > rep3cut)$compoundID
  
  cut1 <- setdiff(rep1Out, untOut)
  cut2 <- setdiff(rep2Out, untOut)
  cut3 <- setdiff(rep3Out, untOut)
  
  opList = c(subset(data.frame(table(c(cut1, cut2, cut3)), stringsAsFactors = F), Freq >= 2)$Var1)
  
  return(as.vector(opList))
}
