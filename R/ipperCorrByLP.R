# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the ipperCorrByLP() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1072-1096.

ipperCorrByLP <- function(Ipper, main = "") {
  
  ipperCorrByLPData <- c()
  
  for (valy in unique(Ipper$lpNum)) {
    newIpper <- subset(Ipper, lpNum == valy)
    rep1vrep2 <- round(with(newIpper, cor(REP2, REP1, method = "pearson")), digits = 2)
    rep1vrep3 <- round(with(newIpper, cor(REP3, REP1, method = "pearson")), digits = 2)
    rep2vrep3 <- round(with(newIpper, cor(REP3, REP2, method = "pearson")), digits = 2)
    
    ipperCorrByLPData <- c(ipperCorrByLPData, list(c(rep1vrep2, rep1vrep3, rep2vrep3)))
  }
  ipperCorrByLPData = matrix(unlist(ipperCorrByLPData), ncol = 6)
  
  colnames(ipperCorrByLPData) <- unique(Ipper$lpNum)
  rownames(ipperCorrByLPData) <- c("rep1vrep2", 'rep1vrep3', 'rep2vrep3')
  
  #heatmap.2(ipperCorrByLPData, margins = c(10,10), main = main,
   #         scale = NULL, trace = "none", key = T, density.info = "none", col = c("#924154","#87C79F"),
    #        sepwidth=c(0.01,0.01), sepcolor = "white", Rowv = F, Colv = F,
     #       colsep=1:ncol(ipperCorrByLPData),rowsep=1:nrow(ipperCorrByLPData), cexCol = 1.2, cexRow = 1.2,
      #      dendrogram = "none", cellnote = ipperCorrByLPData, notecol = "white", notecex = 1.0)
  
  return(ipperCorrByLPData)
}
