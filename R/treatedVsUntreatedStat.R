# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the treatedVsUntreatedStat() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1786-1845.

treatedVsUntreatedStat <- function(untVar, treatVar, antibiotics) {
  
  treatment = NULL
  if (antibiotics == 'amik') {
    treatment = amikacinCompleteDataAll
  }else if(antibiotics == 'amox'){
    treatment = amoxicillinCompleteDataAll
  }else if(antibiotics == 'cipro'){
    treatment = ciproCompleteDataAll
  }
  
  pval <- c()
  filteredCID <- c()
  fc <- c()
  
  
  
  for (valy in unique(c(treatment$compoundID))) {
    treatVarData <- subset(treatment, compoundID == valy)[[treatVar]]
    
    untVarData <- c(subset(amikacinCompleteDataAll, compoundID == valy)[[untVar]],
                    subset(amoxicillinCompleteDataAll, compoundID == valy)[[untVar]],
                    subset(ciproCompleteDataAll, compoundID == valy)[[untVar]])
    #print(untVarData)
    
    #print(paste0(valy, length(treatVarData), "___", length(untVarData)))
    
    qq = c(t.test(x = untVarData, y = treatVarData)$p.value)
    pval <- c(pval, qq)
    filteredCID <- c(filteredCID, valy)
    foldChange <- mean(untVarData, na.rm=T)/(mean(treatVarData, na.rm=T)-1)
    fc <- c(fc, foldChange)
    
  }
  
  outDF <- data.frame('compoundID' = filteredCID,
                      'pValues' = pval,
                      'foldChange' = fc,
                      "padj" = p.adjust(pval, method = "BH")) 
  
  outDF <- merge(x = treatment, y = outDF, by = "compoundID")
  
  #by Foldchange
  upOUTDF <- subset(outDF, foldChange >= 1)# & -log10(pValues) > -log10(0.05))
  downOUTDF <- subset(outDF, foldChange <= -1)# & -log10(pValues) > -log10(0.05))
  
  #by residual
  upOUTDF.resid <- subset(outDF, medTRGDIFFVuntResid >= 0.5)# & -log10(pValues) > -log10(0.05))
  downOUTDF.resid <- subset(outDF, medTRGDIFFVuntResid <= -0.5)# & -log10(pValues) > -log10(0.05))
  
  
  return(list("Upregulated" = upOUTDF,
              "downregulated" = downOUTDF,
              "upRegResid" = upOUTDF.resid,
              "downRegResid" = downOUTDF.resid,
              "allScores" = outDF))
  
  #return(outDF)
  
}
