# TRG drug combination

Author: Adewale Ogunleye

## Overview

This repository contains R code for analysing time-to-regrowth (TRG) drug-combination screening data from plate-reader experiments. The workflow imports raw and processed growth-curve data, calculates TRG and growth-rate metrics, normalizes plate-level measurements, detects outliers or hits, and generates plate maps, replicate plots, correlation views, density plots, and summary tables.

The original monolithic `serialPlotter_384.R` script has been reorganized so that each top-level function now lives in its own file under `R/`. The main script remains the entry point for loading project data, package dependencies, global analysis objects, and the modular function set.

## Repository layout

```text
.
|-- LICENSE
|-- README.md
|-- serialPlotter_384.R
`-- R/
    |-- _source_functions.R
    |-- serialPlotter.R
    |-- TRG_CALCULATOR_THREE.R
    |-- TRG_CALCULATOR_THREE_stab.R
    `-- ... one file per helper function
```

## Main files

| File | Purpose |
| --- | --- |
| `serialPlotter_384.R` | Entry-point script. Loads data, libraries, global variables, and modular functions. |
| `R/_source_functions.R` | Sources every function file in `R/` without loading the full analysis dataset. |
| `R/serialPlotter.R` | Main serial plate plotting and TRG reporting workflow. |
| `R/TRG_CALCULATOR_THREE.R` | Core TRG and growth-rate calculator for imported plate-reader data. |
| `R/TRG_CALCULATOR_THREE_stab.R` | Stabilized TRG calculator used by the 384-well workflow. |
| `R/TRG_CALCULATOR_THREE_stab.m9.R` | M9-medium variant of the stabilized TRG calculator. |

## Requirements

The script expects R plus the packages loaded in `serialPlotter_384.R`:

```r
install.packages(c(
  "runner", "gplots", "ggplot2", "tidyverse", "car", "ggrepel",
  "gridExtra", "ggExtra", "ggthemes", "factoextra", "cowplot",
  "stringr", "ggpubr", "ggvenn", "RColorBrewer", "reshape2",
  "alluvial", "corrplot", "vioplot", "data.table"
))
```

The workflow also uses packages that may need installation from Bioconductor, GitHub, or local sources depending on your R setup:

```r
# Check availability first
requireNamespace("bioassays", quietly = TRUE)
requireNamespace("phenoScreen", quietly = TRUE)
requireNamespace("ggbiplot", quietly = TRUE)
requireNamespace("tuple", quietly = TRUE)
```

## Data assumptions

The current entry script uses absolute paths from the original analysis environment:

```text
/Users/josoga2/Documents/wale_docs/phd/data
/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data
/Users/josoga2/Documents/wale_docs/phd/R/Collection_of_scripts/Aux_functions.R
```

Several functions depend on global objects loaded by `serialPlotter_384.R`, including `LibMap`, `cleanLibMap`, `metafile96`, `metafile384`, `rawdata96`, `rawdata384`, antibiotic-specific complete datasets, and processed `.RData` objects. For reusable analysis on another machine, update these paths before sourcing the entry script.

## Quick start

To load only the functions:

```r
source("R/_source_functions.R")
```

This is useful when you already have the required data objects in memory and want to call individual functions.

To run the full original project setup:

```r
source("serialPlotter_384.R")
```

This loads the project data files, attaches required libraries, creates global helper objects, and sources all modular functions.

## Typical workflow

1. Place plate-reader `.txt` output files in a folder.
2. Confirm the first column is `Time` and remaining columns are well names.
3. Load the project functions with `source("R/_source_functions.R")` or the full entry script.
4. Run `serialPlotter()` for a plate folder to produce PDF visual checks and TRG summaries.
5. Use downstream helpers to normalize, classify, inspect outliers, and generate publication-style plots.

Example:

```r
source("serialPlotter_384.R")

results <- serialPlotter(
  whereIsFileLocated = "/path/to/plate/output/",
  saveFileAs = "trg_report.pdf",
  plateType = 384,
  sepStat = "tog",
  Treatment = "Treatment",
  Lib_Plate_Num = 1,
  untPos = 1,
  smoothing = FALSE
)
```

## Function map

### Plate import, plotting, and reporting

| Function file | Description |
| --- | --- |
| `R/serialPlotter.R` | Runs the main multi-plate TRG plotting and report generation workflow. |
| `R/wellPlatePlotter.R` | Draws 96-well or 384-well plate maps from a single numeric vector. |
| `R/rawDataMaker.R` | Imports raw plate-reader `.txt` files into R data frames. |
| `R/plateViewer.R` | Visualizes raw plate-reader wells from a folder of plate files. |
| `R/quadrSave_View.R` | Saves quadrant-style plate views for inspection. |
| `R/import_and_TRG.R` | Imports plate files and calculates TRG metrics in one workflow. |
| `R/plottingMachineX1.R` | Iterates through folder-based plate outputs and plots TRG calculations. |
| `R/plotdirWel.R` | Plots a selected well across a named plate directory. |
| `R/plotSampleFromFile.R` | Plots an example growth curve from antibiotic screen files. |
| `R/plotSampleFromFile.M9.R` | M9-medium version of `plotSampleFromFile()`. |
| `R/replicatePlotter.R` | Plots replicate growth curves for a selected well. |
| `R/replicatePlotter.M9.R` | M9-medium version of `replicatePlotter()`. |

### TRG and growth-rate calculation

| Function file | Description |
| --- | --- |
| `R/TRG_CALCULATOR_THREE.R` | Calculates TRG, growth rate, fit, baseline, elbow, and intercept metrics. |
| `R/TRG_CALCULATOR_THREE_stab.R` | Stabilized 384-well TRG calculator. |
| `R/TRG_CALCULATOR_THREE_stab.m9.R` | Stabilized M9-medium TRG calculator. |
| `R/stabilizeR.R` | Stabilizes early growth-curve baseline behavior before derivative analysis. |
| `R/derivative.R` | Uses derivative-style slope changes to identify baseline and regrowth behavior. |
| `R/sliding_slope.R` | Calculates maximum growth slope over sliding windows. |
| `R/trySlidingGR.R` | Repeats sliding growth-rate estimation across nearby baseline values. |
| `R/lowessConverter.R` | Smooths growth curves with LOWESS before TRG analysis. |
| `R/slopeDataMelter.R` | Converts slope/intercept list output into vectors. |

### Normalization, scoring, and data preparation

| Function file | Description |
| --- | --- |
| `R/zScorer.R` | Adds z-score and min-max normalized TRG scores per plate. |
| `R/medNormFunc.R` | Median-normalizes numeric TRG-like vectors. |
| `R/dataEditor.R` | Adds library plate metadata, compound IDs, and treatment labels. |
| `R/dmsoSelector.R` | Selects DMSO-related measurements for a treatment and variable. |
| `R/dmsoMeanCalc.R` | Calculates DMSO means for a selected variable. |
| `R/dmsoNormByLP.R` | Performs DMSO normalization by library plate. |
| `R/residualMaker.R` | Adds residual values from a linear model to processed data. |
| `R/lmCalcResiduals.R` | Calculates residuals for antibiotic-specific model variables. |
| `R/completeDataAllSplitterByRep.R` | Splits complete datasets into untreated and replicate columns. |
| `R/stationaryData.R` | Builds stationary-phase summaries from raw plate files. |
| `R/aggregatorStationaryData.R` | Aggregates stationary data by a selected parameter. |

### Hit detection and classification

| Function file | Description |
| --- | --- |
| `R/distiller.R` | Distills full nested antibiotic datasets into long hit-detection tables. |
| `R/distiller4plots.R` | Distills full datasets into plotting-ready long tables. |
| `R/distIpper.R` | Converts nested replicate data into untreated/replicate wide tables. |
| `R/trueOutlierDetectives.R` | Identifies repeated outliers across replicate columns. |
| `R/classifierByTRG.R` | Classifies compounds using TRG-based green/red group inputs. |
| `R/intersector.R` | Intersects classification results across groups. |
| `R/trgVerdictFixer.R` | Applies verdict-based cleanup to TRG and untreated TRG values. |
| `R/freeVsCompound.R` | Compares free antibiotic and compound-treated effects. |
| `R/treatedVsUntreatedStat.R` | Calculates treated-versus-untreated statistical comparisons. |

### Correlation, distribution, and summary plots

| Function file | Description |
| --- | --- |
| `R/corrRawData.R` | Generates raw-data correlation summaries. |
| `R/corrDataTable.R` | Builds correlation tables from plate folders. |
| `R/corrDataTableDMSO.R` | Builds DMSO-specific correlation tables. |
| `R/ipperPlots.R` | Plots wide untreated/replicate summaries. |
| `R/ipperCorrByLP.R` | Plots replicate correlations by library plate. |
| `R/plotIntercepts.R` | Plots intercept diagnostics for a selected library plate. |
| `R/plotInterceptsByWell.R` | Plots intercept diagnostics for a selected well. |
| `R/boxScatter.R` | Combines boxplot and scatter-style visualization. |
| `R/beforeAndAfterPlot.R` | Plots before/after comparisons for processed datasets. |
| `R/myDensPl.R` | Creates density plots for selected variables. |
| `R/myDensLinesAd.R` | Adds adjusted density lines to plots. |
| `R/plotAnything.R` | General plotting helper for antibiotic datasets. |
| `R/plotAnythingV2.R` | Updated general plotting helper. |
| `R/fcPlotter.R` | Plots fold-change data. |
| `R/meltedHeatMap.R` | Creates heatmaps from melted tabular data. |

### Annotation, mapping, and utility helpers

| Function file | Description |
| --- | --- |
| `R/odDetectoR.R` | Detects optical-density thresholds in growth curves. |
| `R/diffModeller.R` | Builds difference-model columns for growth data. |
| `R/getXiNtercept.R` | Estimates x-intercepts for modelled growth curves. |
| `R/dvForm.R` | Builds derivative/formula-style transformed columns. |
| `R/plot_molecule.R` | Plots molecule structure information when chemistry metadata is available. |
| `R/propMapper.R` | Maps query values from one annotation column to another. |
| `R/array_mapper.R` | Maps array-like values to a color scheme. |
| `R/t_col.R` | Adds transparency to colors. |
| `R/plateDBCreator.R` | Creates a 384-well plate annotation database with quadrant labels and controls. |
| `R/strangeMean.R` | Calculates specialized mean summaries for antibiotic datasets. |

## Outputs

The workflow can generate:

- PDF reports with raw growth curves, replicate correlations, plate maps, TRG-vs-growth-rate plots, outlier tables, Venn diagrams, and boxplots.
- Data frames containing TRG, growth rate, z-score, min-max score, DMSO-normalized values, median-adjusted values, replicate labels, compound IDs, and treatment metadata.
- Diagnostic plots for intercepts, densities, correlations, fold changes, and plate annotations.

## Notes for maintenance

- Keep function edits inside the matching `R/<function>.R` file.
- Add new function files to `R/_source_functions.R` so the loader can import them.
- Keep project-level setup, package loading, and data-loading paths in `serialPlotter_384.R`.
- Avoid adding new analysis logic directly to the entry script unless it is global setup.
- Prefer relative paths for future portability; the current script preserves the original absolute data paths.
