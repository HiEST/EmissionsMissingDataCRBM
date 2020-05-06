# Emissions
library(data.table) #Rbindlist

source("tools/interpolation.R")
source("jalkanen2009/equations2009.R")
source("tools/parallelization.R")
source("tools/db.R")

# ... are parameters for storeData function
singleShipEstimation <- function(shipIMO, AISData, IHSData, interpolation = 1,
                                 maxTimeGap = 24*60*60, STEAMVersion = 1,
                                 meanEmissionFactors = TRUE,
                                 ...)
{
    # Select data
    dAIS <- AISData[AISData$imo == shipIMO, ]
    parameters <- IHSData[IHSData$LRIMOShipNO == shipIMO, ]

    if (nrow(parameters) > 1) {
        cat("[MAIN] Warning: duplicated parameters, using the first found.\n")
        parameters <- parameters[1, ]
    }

    # Data interpolation
    if (!is.null(interpolation)) {
        dAIS <- fillSerieLinearly(dAIS,
                                     "fechahora",
                                     c("latitude", "longitude", "sog"),
                                     sampleTime = interpolation,
                                     maxTimeGap = maxTimeGap)
    }

    # Process ship emissions
    cat("[Jalkanen 2009] Executing\n")
    emFunct <- estimateShipEmissions2009

    em = tryCatch({
        emR <- emFunct(dAIS, parameters, interpolation)

        emR$emissions <- cbind(shipIMO, emR$emissions)

        if (meanEmissionFactors) {
            emR$emissionFactors <- cbind(shipIMO, data.frame(t(colMeans(emR$emissionFactors))))
        } else {
            emR$emissionFactors <- cbind(shipIMO, emR$emissionFactors)
        }

        emR$STEAMVersion <- data.frame(STEAMVersion=STEAMVersion)
        emR
    }, warning = function(w) {
        message("Warning: ", w, "\n Returning NA.")
        error <- data.frame(shipIMO=shipIMO, type="warning", message=w$message)
        emR <- list(emissions = NA, emissionFactors = NA, STEAMVersion = NA, error = error)
    }, error = function(e) {
        message("Error: ", e, "\n Returning NA.")
        error <- data.frame(shipIMO=shipIMO, type="error", message=e$message)
        emR <- list(emissions = NA, emissionFactors = NA, STEAMVersion = NA, error = error)
    })


    # Store OR return
    # return(em)
    storeData(em, ...)
}


shipSubsetEstimation <- function(shipIMOList, AISData, IHSData, cl=NULL, ...)
{
    l <- condParLApply(cl, shipIMOList, singleShipEstimation, AISData=AISData,
                    IHSData=IHSData, ...)


    # Mark the ones that don't have an error field
    valid <- sapply(l, function(x) !"error" %in% names(x))

    if(sum(!valid) > 0) {
      message("The following errors were found when estimating emissions:")
      sapply(l[!valid], function(err) {
        message("    ", err$ship, ": ", err$error);
      })
    }

    # Extract result
    message("Extracting emission results")
    em <- lapply(l[valid], function(x) x$emissions)
    #df <- do.call("rbind", em[!is.na(em)]) # Bind not null emissions
    message("Merging data")
    df <- rbindlist(em[!is.na(em)])
    message("Transforming data into a data.frame")
    #df <- setDF(df)
    class(df) <- "data.frame"
    return(df)
}

estimateEmissions <- function(ships, IHSData, cl,
    shipIMOList = NULL,
    coordNames = c("longitude", "latitude"),
    vars = c("SOxME", "SOxAE", "NOxME", "NOxAE", "CO2ME", "CO2AE"),
    ...) # ... Processing and Storage Parameters
{

    # Use ships that are present in IHS and AIS data.
    if (is.null(shipIMOList)) {
        AISIMO <- unique(ships$imo)
        shipIMOList <- intersect(AISIMO, IHSData$LRIMOShipNO) # All available ships on IHS datset
    }

    message("Processing ", length(shipIMOList), " ships");

    est <- shipSubsetEstimation(shipIMOList, ships, IHSData, cl, ...)

    return(est)
}


