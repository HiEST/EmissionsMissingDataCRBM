#' ---
#' jupyter:
#'   jupytext:
#'     text_representation:
#'       extension: .R
#'       format_name: spin
#'       format_version: '1.0'
#'       jupytext_version: 0.8.5
#'   kernelspec:
#'     display_name: R
#'     language: R
#'     name: ir
#'   language_info:
#'     codemirror_mode: r
#'     file_extension: .r
#'     mimetype: text/x-r-source
#'     name: R
#'     pygments_lexer: r
#'     version: 3.5.1
#' ---

library(parallel)
library(dplyr)
source("emissions.R")

cl <- 40


timeAttribute <- "fechahora"
coordNames <- c("longitude", "latitude")

storageType=""
emissionDBPath=""

# Emission Params
STEAMVer <- 1
interpolationGranularity <- 10

AISFile <- "AIS1week.csv"

# Read IHS Data
IHSData <- read.table(file = "IHSTestData.csv", sep = "\t", header = TRUE)

# IHS Preprocess
IHSData$MainEngineRPM[is.na(IHSData$MainEngineRPM)] <- 514  # Jalkanen 2009

MEPred <- read.table(file = "powerME-Predictions.csv", sep = ",", header=T)

IMOTestList <- MEPred$imo

#' # Power dataset preparation

MEPred

realpow <- IHSData[IHSData$LRIMOShipNO %in% IMOTestList,]

ihsfull <- merge(x = realpow, y = MEPred, by.x = 'LRIMOShipNO', by.y = 'imo')

avg <- ihsfull
avg$installedPowerME <- avg$avgtype

pred_act <- ihsfull
pred_act$installedPowerME <- avg$predicted_act

pred_hist <- ihsfull
pred_hist$installedPowerME <- avg$predicted_hist

#' # EMISSIONS


message("Reading data")
ships <- read.table(AISFile, header=T, sep=",", quote="\"")

shipIMOList <- intersect(IMOTestList, IHSData$LRIMOShipNO)

cl <- min(4,length(shipIMOList))

message("Estimating emissions")
emisListReal <- estimateEmissions(ships, realpow, shipIMOList=shipIMOList,
               cl=cl, meanEmissionFactors=TRUE, STEAMVer=STEAMVer,
               # Storage Params
               storageType=storageType, dbpath=emissionDBPath,
               interpolation=interpolationGranularity
            )

emisListAvg <- estimateEmissions(ships, avg, shipIMOList=shipIMOList, 
               cl=cl, meanEmissionFactors=TRUE, STEAMVer=STEAMVer,
               # Storage Params
               storageType=storageType, dbpath=emissionDBPath,
               interpolation=interpolationGranularity
            )

emisListPredAct <- estimateEmissions(ships, pred_act, shipIMOList=shipIMOList,
               cl=cl, meanEmissionFactors=TRUE, STEAMVer=STEAMVer,
               # Storage Params
               storageType=storageType, dbpath=emissionDBPath,
               interpolation=interpolationGranularity
            )

emisListPredHist <- estimateEmissions(ships, pred_hist, shipIMOList=shipIMOList,
               cl=cl, meanEmissionFactors=TRUE, STEAMVer=STEAMVer,
               # Storage Params
               storageType=storageType, dbpath=emissionDBPath,
               interpolation=interpolationGranularity
            )

pollutants <- c("SOxME", "NOxME", "CO2ME", "PMME")

rea <- colSums(emisListReal[,pollutants])/10^6
act <- colSums(emisListPredAct[,pollutants])/10^6
his <- colSums(emisListPredHist[,pollutants])/10^6
avg <- colSums(emisListAvg[,pollutants])/10^6


results <- rbind(rea, act, his, avg)
