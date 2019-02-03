library(zoo)

fillSerieLinearly<- function(serie, timeAttribute, dataAttributes, sampleTime=1,
                              maxTimeGap = 60*60*24) { # 60*60*24 = 24 horas 
    
    cat("[Interpolation] Expansion phase\n")
    #Expansion
    # TODO: WARNING. All the timestamps are in GMT+1. It may not represent the
    # real hour in Spain (CEST).
    times.init <- as.POSIXct(serie[[timeAttribute]], origin = "1970-01-01", tz = "GMT+1") #Create timestamps
    imputed <- zoo(serie[,dataAttributes],times.init) #Create zoo data with the created timestamps        
    imputed <- merge(imputed, zoo(, seq(min(times.init), max(times.init), "sec"))) #Merge data with timestamps to the second    

    cat("[Interpolation] Fill phase\n")
    #Fill
    imputed <-na.approx(imputed, maxgap = maxTimeGap) #Fill NA values with linear interpolation.

    cat("[Interpolation] All samples to the sec: ", nrow(imputed), " rows\n")

    imputed <- na.omit(imputed) # Remove non-interpolated data. 

    cat("[Interpolation] After removing NA samp: ", nrow(imputed), " rows\n")
    #Sampling
    if(sampleTime <= 1) {
        # TODO: Change "fechahora" for other name? 
        #as.data.frame(imputed, stringsAsFactors=FALSE)
        imputed <- cbind(fechahora=as.integer(index(imputed)),as.data.frame(coredata(imputed), stringsAsFactors=FALSE))
    } else {
        cat("[Interpolation] Sampling phase\n")
        idx <- as.integer(index(imputed))%%sampleTime == 0
        imputed <- cbind(fechahora=as.integer(index(imputed)[idx]),as.data.frame(coredata(imputed)[idx,], stringsAsFactors=FALSE))
        cat("[Interpolation] After sampling: ", nrow(imputed), " rows\n")
    }
    return(imputed)
}
