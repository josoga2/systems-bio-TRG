# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plotAnythingV2() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2278-2331.

plotAnythingV2 <- function(antibiotics, 
                           varX,
                           varY,
                           xLim,
                           xLab=NULL,
                           yLim,
                           yLab = NULL,
                           main =NULL,
                           byRep = F,
                           repToUse = NULL,
                           byLP = F,
                           lpToUse = NULL,
                           colCol = NULL,
                           verticalLine = 0,
                           horizLine = 0,
                           secondHorizLine = 0) {
  
  treatment = NULL
  if (antibiotics == 'amik') {
    treatment = amikacinCompleteDataAll
  }else if(antibiotics == 'amox'){
    treatment = amoxicillinCompleteDataAll
  }else if(antibiotics == 'cipro'){
    treatment = ciproCompleteDataAll
  }
  
  dataReady = NULL
  if (byRep == T) {
    dataReady <- subset(treatment, ORDER == repToUse)[c(varX, varY, colCol)]
    colorScaleMidpoint <- mean(dataReady[[colCol]], na.rm =T)
    pl <- ggplot(data = dataReady, aes_string(x = varX, y = varY, color = colCol))+
      geom_point(cex=1.2)+lims(x = xLim, y = yLim)+labs(x = xLab, y = yLab)+
      theme_base()+scale_color_gradient2(low = "#534ca9",  high = '#aa5f6c', midpoint = 0.1)+
      ggtitle(antibiotics)
  }else if(byLP == T){
    dataReady <- subset(treatment, lpNum == lpToUse)[c(varX, varY, colCol)]
    colorScaleMidpoint <- mean(dataReady[[colCol]], na.rm =T)
    pl <- ggplot(data = dataReady, aes_string(x = varX, y = varY, color = colCol))+
      geom_point(cex=1.2)+lims(x = xLim, y = yLim)+labs(x = xLab, y = yLab)+
      theme_base()+scale_color_gradient2(low = "#534ca9",  high = '#aa5f6c', midpoint = 0.05 )+
      ggtitle(antibiotics)
  }else{
    dataReady <- treatment[c(varX, varY, colCol)]
    colorScaleMidpoint <- mean(dataReady[[colCol]], na.rm =T)
    pl <- ggplot(data = dataReady, aes_string(x = varX, y = varY, color = colCol))+
      geom_point(cex=1.2)+lims(x = xLim, y = yLim)+labs(x = xLab, y = yLab)+
      theme_base()+scale_color_gradient2(low = "#534ca9",  high = '#aa5f6c', midpoint = 0.75)+
      ggtitle(antibiotics)+geom_vline(xintercept = verticalLine)+geom_hline(yintercept = horizLine)+
      geom_hline(yintercept = secondHorizLine, linetype = 'dashed')
  }
  
  return(pl)
  #return(dataReady)
}
