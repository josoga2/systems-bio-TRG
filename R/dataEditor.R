# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Defines the dataEditor() function for the TRG drug-combination analysis workflow.
# Source: Extracted from serialPlotter_384.R, lines 177-188.

dataEditor <- function(ExperimentPlate, Lib_Plate_Num, Plate_Treatments) {
  for (plateN in 1:length(ExperimentPlate)) {
    ExperimentPlate[[plateN]]$lpNum <- paste0('LP', Lib_Plate_Num)
    ExperimentPlate[[plateN]]$PlateTreatment <- c('UNT', Plate_Treatments, Plate_Treatments, Plate_Treatments)[plateN] 
    ExperimentPlate[[plateN]]$compoundID <- paste0(ExperimentPlate[[plateN]]$lpNum, '_', ExperimentPlate[[plateN]]$wellName)
    ExperimentPlate[[plateN]]$ConditionTreatment <- paste0(ExperimentPlate[[plateN]]$PlateTreatment, '_', ExperimentPlate[[plateN]]$lpNum, '_', ExperimentPlate[[plateN]]$wellName)
    
    ExperimentPlate[[plateN]] <- merge(x = ExperimentPlate[[plateN]], y = cleanLibMap, by = 'compoundID')
  }
  
  return(ExperimentPlate)
}
