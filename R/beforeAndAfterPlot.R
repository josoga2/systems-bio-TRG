# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the beforeAndAfterPlot() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1758-1783.

beforeAndAfterPlot <- function(completeData, 
                               beforeX, 
                               afterY, 
                               axis = c(0,3), 
                               main = "", 
                               colorColumn = "black",
                               colorScaleMidpoint= 10) {
  beforeAxiz=mean(completeData[[beforeX]], na.rm=T)
  AfterAxiz=mean(completeData[[afterY]], na.rm=T)
  
  
  p <- ggplot(completeData, aes_string(x = beforeX, y = afterY, color = colorColumn))+
    geom_point(cex=0.5,)+labs(x = "Before Treatment", y='After Treatment')+
    theme_base(base_size = 12)+lims(x=axis, y=axis)+
    geom_vline(xintercept = beforeAxiz)+
    geom_hline(yintercept = AfterAxiz)+
    annotate("text", x = 1.54, y = 0.1, color="red",
             label=paste("Mean = ", round(beforeAxiz,digits = 3)))+
    annotate("text", x = 0.3, y = 1.7, color="red",
             label=paste("Mean = ", round(AfterAxiz,digits = 3)))+
    ggtitle(main)+
    scale_color_gradient2(low = "#534ca9",  high = '#aa5f6c', midpoint = colorScaleMidpoint)
  
  return(ggMarginal(p, type = "histogram"))
  
}
