# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the ipperPlots() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1050-1070.

ipperPlots <- function(Ipper, main) {
  
  par(mfrow = c(1,3))
  
  with(Ipper, plot(REP1, REP2, pch = 19, cex = 0.75, xlim = c(0,20), ylim = c(0,20), main = main))
  abline(lm(REP2~REP1, Ipper))
  rep1vrep2 <- round(with(Ipper, cor(REP2, REP1, method = "pearson")), digits = 2)
  text(0.75, 20, label= paste0('R2 =', rep1vrep2))
  
  with(Ipper, plot(REP1, REP3, pch = 19, cex = 0.75, xlim = c(0,20), ylim = c(0,20), main = main))
  abline(lm(REP3~REP1, Ipper))
  rep1vrep3 <- round(with(Ipper, cor(REP3, REP1, method = "pearson")), digits = 2)
  text(0.75, 20, label= paste0('R2 =', rep1vrep3))
  
  with(Ipper, plot(REP2, REP3, pch = 19, cex = 0.75, xlim = c(0,20), ylim = c(0,20), main = main))
  abline(lm(REP3~REP2, Ipper))
  rep2vrep3 <- round(with(Ipper, cor(REP3, REP2, method = "pearson")), digits = 2)
  text(0.75, 20, label= paste0('R2 =', rep2vrep3))
  
  return(c(rep1vrep2, rep1vrep3, rep2vrep3))
}
