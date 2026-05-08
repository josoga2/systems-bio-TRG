# Project: TRG drug combination
# Author: Adewale Ogunleye
# Description: Entry-point script for loading project data, dependencies, and modular TRG analysis functions.

#source
setwd("/Users/josoga2/Documents/wale_docs/phd/data")
source("/Users/josoga2/Documents/wale_docs/phd/R/Collection_of_scripts/Aux_functions.R")

LibMap <- read.table('../PLANS/LibMap.txt', sep = '\t', quote = '\t', header = T)
LibMap$compoundID <- paste0('LP',LibMap$Library.plate,'_', LibMap$Well)
cleanLibMap <- LibMap[c('compoundID', 'Catalog.Number')]

workDirectory <- '/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/'


### FINALLY PROCESSED DATASET
load(file = paste0(workDirectory, "amikacinCompleteDataAll.RData"))
load(file = paste0(workDirectory, "amoxicillinCompleteDataAll.RData"))
load(file = paste0(workDirectory, "ciproCompleteDataAll.RData"))
load(file = paste0(workDirectory, "CompleteDataAllABX.RData"))
#LOAD RAW DATA
load(file = paste0(workDirectory, "rawDataListsInR/rawAmikacin.RData"))
load(file = paste0(workDirectory, "rawDataListsInR/rawAmoxicillin.RData"))
load(file = paste0(workDirectory, "rawDataListsInR/rawCiprofloxacin.RData"))
#LOAD MINIFIED DATA
load(file = paste0(workDirectory, 'amikacinMinified.RData'))
load(file = paste0(workDirectory, "amoxicillinMinified.RData"))
load(file = paste0(workDirectory, "ciproMinified.RData"))
load(file = paste0(workDirectory, "allAbxMinified.RData"))
#LOAD MEAN DATA
load(file = paste0(workDirectory, 'amikacinMeanSTR.RData'))
load(file = paste0(workDirectory, 'amoxicillinMeanSTR.RData'))
load(file = paste0(workDirectory, 'ciprofloxacinMeanSTR.RData'))
load(file = paste0(workDirectory, 'AllAbxMeanSTR.RData'))
load(file = paste0(workDirectory, 'AllAbxMeanMini.STR.RData'))
#import pahse one output
load(file = paste0(workDirectory, "PhaseOneQ_A.RData"))

#correlation heatmap for TRG
load(file = paste0(workDirectory, 'corrHeatMap.all_abx.RData'))

#SET NA DATA AS WELL
amikacinCompleteDataAll.na <- amikacinCompleteDataAll
amoxicillinCompleteDataAll.na <- amoxicillinCompleteDataAll
ciproCompleteDataAll.na <- ciproCompleteDataAll

amikacinCompleteDataAll.na["TRG"][amikacinCompleteDataAll.na["TRG"] == 0] <- NA
amoxicillinCompleteDataAll.na["TRG"][amoxicillinCompleteDataAll.na["TRG"] == 0] <- NA
ciproCompleteDataAll.na["TRG"][ciproCompleteDataAll.na["TRG"] == 0] <- NA

#import NA STRANGE MINIFIED MEAN
load(file = paste0(workDirectory, 'amox.NAMEAN.RData'))
load(file = paste0(workDirectory, 'amik.NAMEAN.RData'))
load(file = paste0(workDirectory, 'cipro.NAMEAN.RData'))

#import cushioed data
load(file = paste0(workDirectory, 'phaseOne.Cush.RData'))

######## DMSO4 DATA #####
#pick all DMSO wells
allDmsoWell <- unique(subset(LibMap, Catalog.Number == 'DMSO')$Well)
allDmsoNoisyWell <- unique(subset(LibMap, Catalog.Number == 'DMSO noisy')$Well)

plateOrder <- c("UNT", "Rep1", "Rep2", "Rep3")
  
lpNum <- c("lp1", "lp2", "lp3", "lp4", "lp5", "lp6")

LPNUM <- c("LP1", "LP2", "LP3", "LP4", "LP5", "LP6")

#### M9 Processed Data ###########

load(file = paste0(workDirectory, "CiproData.M9.RData"))
load(file = paste0(workDirectory, "AmoxData.M9.RData"))

#########################

library(runner)
library(gplots)
#library(plotly)
library(ggplot2)
library(tidyverse)
library(car)
library(ggrepel)
library(gridExtra)
library(ggExtra)
library(ggthemes)
library(factoextra)
library(cowplot)
library(stringr)
library(bioassays)
library(ggplot2)
library(phenoScreen)
library(gridExtra)
library(ggpubr)
library(car)
library(ggvenn)
library(RColorBrewer)
library(reshape2)
library(ggbiplot)
library(alluvial)
library(tuple)
library(corrplot)
library(vioplot)


#quadrant_splitter

# Source modular functions extracted from the original serial plotter workflow.
.frame_files <- vapply(sys.frames(), function(frame) {
  if (!is.null(frame$ofile)) frame$ofile else NA_character_
}, character(1))
.frame_files <- .frame_files[!is.na(.frame_files)]
.cmd_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(.frame_files) > 0) {
  .project_root <- dirname(normalizePath(.frame_files[[length(.frame_files)]]))
} else if (length(.cmd_file) > 0) {
  .project_root <- dirname(normalizePath(sub("^--file=", "", .cmd_file[[1]])))
} else {
  .project_root <- getwd()
}
source(file.path(.project_root, "R", "_source_functions.R"), chdir = TRUE)
rm(.cmd_file, .frame_files, .project_root)













setwd("~/Documents/wale_docs/phd/data") # optional

#df <- read.table(file = "test_ground/Output/Ed1a_10.txt", sep = '\t', header = T)

#Auxilliary Functions

###### ---- alternate TRG VERSIONS ----


#!important to run the derivative aux function first

#derivative(stabilizeR(sample.well, generatePlot = F, stabByMean = T), pl = T)



#NOTE
#TRG_CALC can function on its own if you supply an already imported dataset i.e. the read.table() importer



#pdf(file = "litData.pdf" ,useDingbats = F, paper = 'a4r', width = 120, height = 80)
#par(mfrow = c(8,12), cex=0.25, mar=c(2,2,2,2), oma=c(2,2,2,2), no.readonly = T)
#a = TRG_CALCULATOR_THREE(plate = litData, date = NULL, plate_no = NULL, min_start = -4)

#dev.off()
#NOTE
#TO calculate for multiple datasets in multiple folders, use plottingMachine. 
#IT WORKS WITH THE LAB'S CODING FOLDER STRUcture, "../Output" 

#Ploting function
#before using it, you can define create a pdf file to dump the plots !optional


#######---- Load data ------
setwd("~/Documents/wale_docs/phd/data/")
load("/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/amikstationaryData.RData")
load("/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/amoxstationaryData.RData")
load("/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/ciprostationaryData.RData")

load("/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/amikacin.RData")
load("/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/amoxicillin.RData")
load("/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Full_Data/ciprofloxacin.RData")

#######---- Hit detection scripts ------





library(data.table)

#make raw data from files

#correlation on raw data



####### DMSO correlation #####


#function for replicate correlations

########
#plot across replicates at once

######------ plot replicates across a lp at once ######------ 


#replicatePlotter.M9(whereIsFileLocated = "Processed/Amoxicillin/lp1/", well = "A1")


######------ modelling with david's model ######------ 
odDetection <- 0.01113222
#apply to all
amikData = lapply(amikData, odDetectoR)
amoxData = lapply(amoxData, odDetectoR)
ciproData = lapply(ciproData, odDetectoR)


amikData = lapply(amikData, diffModeller)
amoxData = lapply(amoxData, diffModeller)
ciproData = lapply(ciproData, diffModeller)


#predict


amikData = lapply(amikData, getXiNtercept)
amoxData = lapply(amoxData, getXiNtercept)
ciproData = lapply(ciproData, getXiNtercept)



#using david's formula

amikData = lapply(amikData, dvForm)
amoxData = lapply(amoxData, dvForm)
ciproData = lapply(ciproData, dvForm)



#plot the schema

## PLOT INTERCEPTS BY WELL

#make new data from complete processed data

##
######## All well names #####
allWellNames <- names(read.table(file = 'Phase_1_Data/Full_Data/Full_Amikacin/lp1/plate1_OD.txt', header = T))

sampleWellName <- allWellNames[sample(384, 1)]
#### DMSO SELECTOR #####



# sample Data plottler


#plotSampleFromFile.M9(abx = "Amik", well = 'D1',repNum = 1 , lp = 1)



#########----CLASSIFIER------


#########----Intersector------

######------StatoinarydATA CALCULATOR-----

#### DO NOT RECALCULATE ########################################
#amikstationaryData <- stationaryData("Full_Amikacin/")       ##
#amoxstationaryData <- stationaryData("Full_Amoxicillin/")    ##
#ciprostationaryData <- stationaryData("Full_Ciprofloxacin/") ##
#save(amikstationaryData, file = "amikstationaryData.RData")  ##
#save(amoxstationaryData, file = "amoxstationaryData.RData")  ##
#save(ciprostationaryData, file = "ciprostationaryData.RData")##
#### DO NOT RECALCULATE ########################################



#amikStatIOD <- aggregatorStationaryData(stationaryInput = amikstationaryData, Parameter = "StationaryOD")
#amoxStatIOD <- aggregatorStationaryData(stationaryInput = amoxstationaryData, Parameter = "StationaryOD")
#ciprStatIOD <- aggregatorStationaryData(stationaryInput = ciprostationaryData, Parameter = "StationaryOD")


###################--- plot molecules ----

###################--- Before and After ----






#head(strangeMean("Amox.M9"))
#strangeMean("Amik")

#####--Library Maker----

libXter <- read.table("/Users/josoga2/Documents/wale_docs/phd/data/Phase_1_Data/Library_Classification.txt", sep = "\t", header = T)

libXter <- libXter[complete.cases(libXter),]

xter <- c()
for (valy in c(libXter$Research.Area)) {
  tempXter <- str_split(valy, pattern = ";")[[1]]
  
  if (length(tempXter) > 1) {
    xter <- c(xter, "2+ Research Application" )
  }else{
    xter <- c(xter, tempXter[1])
  }
}

libXter$CleanFeature <- xter

pthwy <- c()
for (valy in c(libXter$PathWay)) {
  tempPw <- str_split(valy, pattern = ";")[[1]]
  
  if(length(tempPw) > 1){
    pthwy <- c(pthwy, "2+ Pathways")
  }else{
    pthwy <- c(pthwy, tempPw[1])
  }
}

libXter$CleanPathways <- pthwy

libXter <- data.frame(lapply(libXter, function(x) {gsub("others", "Others", x)}))

infect <- c()

for (valy in libXter$Research.Area) {
  if("Infection" %in% str_split(valy, ';')[[1]]){
    infect <- c(infect, "Infection")
  }else{
    infect <- c(infect, "Others")
  }
}

libXter$infectionStatus <- infect
#merge libxter with libmap
updLibmap <- merge(LibMap, libXter, by="Catalog.Number", all=F)

####Merge complete data with library details
amikacinCompleteDataAllWithLibMap <- merge(amikacinCompleteDataAll, updLibmap, by.x = "compoundID.x", by.y = 'compoundID')
amoxicillinCompleteDataAllWithLibMap <- merge(amoxicillinCompleteDataAll, updLibmap,  by.x = "compoundID.x", by.y = 'compoundID')
ciproCompleteDataAllWithLibMap <- merge(ciproCompleteDataAll, updLibmap,by.x = "compoundID.x", by.y = 'compoundID')

#print("stop")


#### plot ANything ----













#### lm residuals determinator -----


###All DONE
#amikacinCompleteDataAll <- lmCalcResiduals('amik', varVone = F, newColName = "medTRGDIFFVuntResid")
#amikacinCompleteDataAll <- lmCalcResiduals('amik', varVone = T, newColName = "medTRGDIFFVoneResid")

#amoxicillinCompleteDataAll <- lmCalcResiduals('amox', varVone = F, newColName = "medTRGDIFFVuntResid")
#amoxicillinCompleteDataAll <- lmCalcResiduals('amox', varVone = T, newColName = "medTRGDIFFVoneResid")


#ciproCompleteDataAll <- lmCalcResiduals('cipro', varVone = F, newColName = "medTRGDIFFVuntResid")
#ciproCompleteDataAll <- lmCalcResiduals('cipro', varVone = T, newColName = "medTRGDIFFVoneResid")


### SPLIT any complete data by replicate

###----- Silly Function -----











#-------- color tools -------
AGTeam <- read.delim("/Users/josoga2/Documents/wale_docs/phd/data/TRG_Screen/AGTeam_color_table.txt", header = T)

cornColorList <- list("Greens" = c('#006644', '#00875A', '#36B37E', '#57D9A3', '#ABF5D1'),
                      "Blues" = c('#00A3BF','#00B8D9','#00C7E6','#79E2F2','#B3F5FF'),
                      "Orange" = c('#FF991F','#FFAB00','#FFC400','#FFE380','#FFF0B3'),
                      "Red" = c('#DE350B','#FF5630','#FF7452','#FF8F73','#FFBDAD'),
                      "Black" = c("#24292E", "#383F45", "#454C52", "#596066", "#676E76"),
                      "Grey" = c("#7F8790", "#9FA5AD", "#CED2D6", "#E5E7EA", '#F6F7F9'),
                      "Violet" = c("#A239A0", "#C054BE", "#C270C0", "#D6A9D5", "#E1C7E0"),
                      "Basic" = c('#006644', '#00A3BF','#FF991F','#DE350B',"#24292E","#7F8790","#A239A0"),
                      "Tableau10" = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"),
                      "TRG_project" = c("#1F6999", "#76D621",  "#EF9625", "#D5215A", "#773A8D", "#000000"),
                      "Tableau_Ext" = c("#4e79a7", "#8cd17d", "#e15759", "#fabfd2", "#a0cbe8", "#59a14f", "#b07aa1",
                                        "#ff9d9a", "#f28e2b", "#f1ce63", "#79706e", "#d4a6c8", "##e9e9e9", "#ffbe7d",
                                        "#bab0ac", "#9d7660", "#d37295", "#86bcb6", "#362a39", "#cd9942"),
                      "uni_tub" = c('#a41c34', '#c9b184', '#9d7508', '#a01c54', '#d43c34'),
                      'TRG_Ternary' = c("#1a80bb", "#ea801c", "#999999"),
                      'TRG_Blue_grad' = c("#1a80bbff", "#1a80bbe5", "#1a80bbcc","#1a80bbb2","#1a80bb99", "#51a80bb7f"),
                      'TRG_Orange_Grad' = c("#ea801cff", "#ea801ce5", "#ea801ccc", "#ea801cb2", "#ea801c99", "#ea801c7F"),
                      'NEW_TRG' = c('#219581', '#EB7E35', "#30539D", '#EBAD35', 'grey', "#65C0B1", "#728DC6", "#FFD686", '#FFB786', 'black', 'white' ))



#make colors maps


## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk



#MELTED HEATMAP



#plate map (or database) creator





## END
