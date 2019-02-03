library(parallel)

condParLApply  <- function(cl, data, fun, ...) { 
    if (is.numeric(cl)) {
        message("Running in parallel (mclapply with ", cl, " processes)")
        mclapply(data, fun, mc.cores = cl, ...)
    } else if ("cluster" %in% class(cl)) {
        message("Running in parallel using a predefined cluster.")
        parLapply(cl, data, fun, ...) 
    } else {
        lapply(data, fun, ...)
    } 
}
 # Build variable rasters
    #if(is.null(cl)) {
    #    rasters <- apply(subs, MARGIN=2, buildRasterAggFunct, coords=xy, raster=r)
    #} else {
    #    message("raster::createRasterBrick: building rasters in parallel")
    #    rasters <- parApply(cl, subs, MARGIN=2, buildRasterAggFunct, coords=xy, raster=r)
    #}

condParApply  <- function(cl, data, fun, type="FORK", ...) {
    if (is.numeric(cl)) {
        message("Running in parallel using a temporal cluster of ", cl, " cores.")
        cluster <- makeCluster(cl, type=type)
        res <- parApply(cluster, data, fun, ...) 
        stopCluster(cluster)
        res
    } else if ("cluster" %in% class(cl)) {
        message("Running in parallel using a predefined cluster.")
        parApply(cl, data, fun, ...) 
    } else {
        apply(data, fun, ...)
    } 
} 
