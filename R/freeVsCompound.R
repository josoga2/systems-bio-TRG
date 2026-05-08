# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the freeVsCompound() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 1847-1903.

freeVsCompound <- function(treatVar, antibiotics, fcThreshold = 0.5) {
  
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
  
  tmvd <- c()
  #print(length(dmsoVarData))
  
  for (valy in unique(c(treatment$compoundID))) {
    treatVarData <- subset(treatment, compoundID == valy)[[treatVar]]
    
    currentPlate <- str_split((treatment$compoundID)[1], pattern = '_', simplify = F)[[1]][1]
    dmsoVarData <- sample(x = c(subset(treatment, wellName %in% allDmsoWell & lpNum == currentPlate)[[treatVar]]), size = 18)
    
    #print(length(dmsoVarData))
    #print(paste0(valy, length(treatVarData), "___", length(untVarData)))
    
    qq = c(t.test(x = dmsoVarData, y = treatVarData, var.equal = F, mu=0)$p.value)
    pval <- c(pval, qq)
    filteredCID <- c(filteredCID, valy)
    foldChange <- mean(dmsoVarData, na.rm=T)/(mean(treatVarData, na.rm=T)-1)
    fc <- c(fc, foldChange)
    
  }
  
  outDF <- data.frame('compoundID' = filteredCID,
                      'pValues' = pval,
                      'foldChange' = fc,
                      "padj" = p.adjust(pval, method = "BH")) 
  
  
  
  plot(outDF$foldChange, -log10(outDF$pValues), cex = 0.75, pch = 16, col = "#6287B8", 
       xlim = c(-1,1), main = antibiotics, ylim = c(0,5), xlab = "Fold Change", 
       ylab = "-log10(Adjusted P)", cex.lab = 1.5, cex.main = 1.5)
  abline(h = -log10(0.05), col = 'grey', lty = 2)
  abline(v = fcThreshold, col = 'grey', lty = 2)
  abline(v = -fcThreshold, col = 'grey', lty = 2)
  
  upOUTDF <- subset(outDF, foldChange >= fcThreshold & -log10(pValues) > -log10(0.05))
  downOUTDF <- subset(outDF, foldChange <= -fcThreshold & -log10(pValues) > -log10(0.05))
  
  return(list("Upregulated" = upOUTDF,
              "downregulated" = downOUTDF,
              "allScores" = outDF))
  #return(dmsoVarData)
}
