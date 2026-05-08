# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Sources every modular function file used by the TRG drug-combination workflow.

.function_files <- c(
  "aggregatorStationaryData.R",
  "array_mapper.R",
  "beforeAndAfterPlot.R",
  "boxScatter.R",
  "classifierByTRG.R",
  "completeDataAllSplitterByRep.R",
  "corrDataTable.R",
  "corrDataTableDMSO.R",
  "corrRawData.R",
  "dataEditor.R",
  "derivative.R",
  "diffModeller.R",
  "distiller.R",
  "distiller4plots.R",
  "distIpper.R",
  "dmsoMeanCalc.R",
  "dmsoNormByLP.R",
  "dmsoSelector.R",
  "dvForm.R",
  "fcPlotter.R",
  "freeVsCompound.R",
  "getXiNtercept.R",
  "import_and_TRG.R",
  "intersector.R",
  "ipperCorrByLP.R",
  "ipperPlots.R",
  "lmCalcResiduals.R",
  "lowessConverter.R",
  "medNormFunc.R",
  "meltedHeatMap.R",
  "myDensLinesAd.R",
  "myDensPl.R",
  "odDetectoR.R",
  "plateDBCreator.R",
  "plateViewer.R",
  "plot_molecule.R",
  "plotAnything.R",
  "plotAnythingV2.R",
  "plotdirWel.R",
  "plotIntercepts.R",
  "plotInterceptsByWell.R",
  "plotSampleFromFile.M9.R",
  "plotSampleFromFile.R",
  "plottingMachineX1.R",
  "propMapper.R",
  "quadrSave_View.R",
  "rawDataMaker.R",
  "replicatePlotter.M9.R",
  "replicatePlotter.R",
  "residualMaker.R",
  "serialPlotter.R",
  "sliding_slope.R",
  "slopeDataMelter.R",
  "stabilizeR.R",
  "stationaryData.R",
  "strangeMean.R",
  "t_col.R",
  "treatedVsUntreatedStat.R",
  "TRG_CALCULATOR_THREE_stab.m9.R",
  "TRG_CALCULATOR_THREE_stab.R",
  "TRG_CALCULATOR_THREE.R",
  "trgVerdictFixer.R",
  "trueOutlierDetectives.R",
  "trySlidingGR.R",
  "wellPlatePlotter.R",
  "zScorer.R"
)

.frame_files <- vapply(sys.frames(), function(frame) {
  if (!is.null(frame$ofile)) frame$ofile else NA_character_
}, character(1))
.frame_files <- .frame_files[!is.na(.frame_files)]
.function_root <- if (length(.frame_files) > 0) {
  dirname(normalizePath(.frame_files[[length(.frame_files)]]))
} else {
  getwd()
}
for (.function_file in .function_files) {
  source(file.path(.function_root, .function_file), local = FALSE)
}

rm(.frame_files, .function_file, .function_files, .function_root)
