# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the meltedHeatMap() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 3245-3259.

meltedHeatMap <- function(inputTab, write.cex = 10){
  melted.data <- reshape2::melt(inputTab)
  melted.data$prot <- factor(melted.data[[1]])
  melted.data$variable <- factor(melted.data[[2]])
  
  theVals <- colnames(melted.data)
  
  ggplot(melted.data, aes_string(x = theVals[1], y = theVals[2], fill = 'value'))+
    geom_tile(color = 'white', lwd = 1.5)+theme_minimal()+
    #scale_fill_gradient2(low = "tan1", mid= 'white', high = "lightblue", midpoint = 0.5, limits = c(0,1)) +
    scale_fill_gradient(low = "white", high = "tan1", limits = c(0,100)) +
    coord_fixed(expand = T)+
    geom_text(cex = write.cex, aes(label = round(value, digits = 2)))+
    theme(axis.title =element_text(size=write.cex,face="bold"))
}
