#' ---
#' jupyter:
#'   jupytext:
#'     formats: ipynb,r:spin
#'     text_representation:
#'       extension: .r
#'       format_name: spin
#'       format_version: '1.0'
#'       jupytext_version: 0.8.6
#'   kernelspec:
#'     display_name: R
#'     language: R
#'     name: ir
#' ---

#' # Install requiered packages

#install.packages(c("marima","NMOF","parallel"), repos='http://cran.us.r-project.org')

#' # Loading libraries

source("marima-tools.R")

#' # Calculate all models

#' ## Load data

load("AISPreprocessedData.data")

Sys.setenv(OPENBLAS_NUM_THREADS=40, OMP_NUM_THREADS=40)

#' ## Data and parameter preparation

##Parameters
levels <- list(AR=0:2,MA=0:2,penalty=1:2,meansAdjusted=0:1)
sp <- split(ships, ships$id)
vars <- c("rotationGPS", "sog","bathymetry3")

#ncores <- detectCores()
ncores <- 20

#' ## Execution

cluster <- makeCluster(ncores,type="FORK")  

#r <- parLapply(cluster,sp, gridMARIMA, v=vars, l=levels) #Using parallel in at this level
marimaRes <- lapply(sp, gridMARIMA, v=vars, l=levels, cluster) #Using parallel in gridSearch

stopCluster(cluster)  

save(marimaRes, file="MARIMAResults.data")

#' # Model analysis

load(file="MARIMAResults.data")

plotMARIMA(marimaRes[[4]]$model)

#' ## Analysis

plotMARIMA(marimaRes[[1]]$model$model$DATA)

errors <- unlist(sapply(r, function(x) if(is.null(x$model)) x$message))

errors

#' ## One-step Forecasting

cluster <- makeCluster(ncores,type="FORK")  

fore <- parLapply(cluster, marimaRes, 
        function(x) {
            ret <- NULL
            if (!is.null(x$model)) {
                    ret <- stepwiseForecast(x$model$model, t(x$model$model$DATA))
            }

            return(ret)
        }
)

stopCluster(cluster)

save(fore, file="~/data/MARIMA1StepFore.data")

load("~/data/MARIMA1StepFore.data")

is.null(fore[[3]])

#Filter Null
fore2 <- fore[!sapply(fore, is.null)] 

nr <- sapply(fore2, function(x){
    if (sum(x$nrmse) != Inf) {
        x$nrmse
    } else {
        rep(NA, length(x$nrmse))
    }
})


nr

nr[(nr > 10)] <- NA

rowMeans(nr, na.rm = TRUE)

mean(nr)

plotForecast(fore[[4]],3)

plot(f[2,], type="l", col="red")
lines(mod$model$DATA[2,], col="blue")
