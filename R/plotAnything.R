# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the plotAnything() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 2181-2238.

plotAnything <- function(antibiotics, 
                         replicate = NULL, 
                         varY, 
                         varX, 
                         compoundID = NULL, 
                         lp = NULL, 
                         byRep = T, 
                         singleVar = NULL ) {
  
  treatment = NULL
  if (antibiotics == 'amik') {
    treatment = amikacinCompleteDataAll
  }else if(antibiotics == 'amox'){
    treatment = amoxicillinCompleteDataAll
  }else if(antibiotics == 'cipro'){
    treatment = ciproCompleteDataAll
  }
  
  treatmentData = NULL
  if (antibiotics == 'amik') {
    treatment = amikData
  }else if(antibiotics == 'amox'){
    treatment = amoxData
  }else if(antibiotics == 'cipro'){
    treatment = ciproData
  }
  
  
  dataReady = NULL
  if (byRep == F) {
    dataReady <- subset(treatment, ORDER == replicate & compoundID == compoundID & lpNum == lp)[c(varX, varY)]
    
  }else{
    treatIpper <- distIpper(treatment, singleVar)
    treatIpper[is.na(treatIpper)] = 0
    
    
    dataReady <- subset(treatIpper, lpNum == lp)[c(varX, varY)]
    #print(head(dataReady))
    #print(unique(dataReady$lpNum))
    
    
    #plot(dataReady, pch = 21, cex = 1, col = 'black', bg = '#22B258', main = paste0(antibiotics, " ", lp), xlim = c(0,20), ylim = c(0,20))
    #legend('topright', legend = paste('Correlation =', round(cor(dataReady)[2], 2)), 
    #      cex = 1.2, pch = 19, col = '#22B258')
    
    pl <- ggplot(dataReady, aes_string(varX, varY))+
      geom_point(color = '#24749B')+theme_base()+lims(x = c(0,20), y = c(0,20))+
      annotate(geom = 'text', x = 10, y = 20, cex=5, color = '#24749B', label = paste('Correlation =', round(cor(dataReady)[2], 2)))+
      ggtitle(paste0(antibiotics, " ", lp))+
      theme(plot.title = element_text(hjust = 0.5))+geom_abline(intercept = 0, slope = 1, linetype="dashed", linewidth=0.5)+
      geom_smooth(method='lm', se = F, na.rm = T)
    
    return(ggMarginal(pl, type = "density", fill = '#24749B'))
    
  }
  
}
