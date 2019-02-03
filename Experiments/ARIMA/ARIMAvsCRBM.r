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

load("MARIMA1StepFore.data") # MARIMA forecast
load("MSE-CRBM.data") # Error CRBM

#' # Get valid models

#' Some of the ARIMA models had convergence issues, therefore we filter them

errorTh <- 10 #These models are broken

notnullARIMA  <- sapply(fore, function(x) {
        !is.null(x) && (sum(is.na(x$nrmse)) + sum(x$nrmse > errorTh, na.rm=TRUE)) == 0                
    }
)

notnullCRBM  <- sapply(error.crbm, function(x) {
        !is.null(x) && (sum(is.na(x$nrmse)) + sum(x$nrmse > errorTh, na.rm=TRUE)) == 0                
    }
)

sum(notnullARIMA)

sum(notnullCRBM)

sum(notnullCRBM & notnullARIMA)

e.arima <- fore[notnullCRBM & notnullARIMA]

correct <- names(e.arima)

e.crbm <- error.crbm[(names(error.crbm) %in% correct)]

#' # Preprocess

#' ## MSE

mse.crbm <- lapply(e.crbm, function(x)x$mse)
mse.crbm <- as.data.frame(do.call("rbind", mse.crbm))
names(mse.crbm) <-  c("CRBM.rotationGPS","CRBM.sog","CRBM.bathymetry")

mse.arima <- lapply(e.arima, function(x)x$mse)
mse.arima <- as.data.frame(do.call("rbind", mse.arima))
names(mse.arima) <-  c("MARIMA.rotationGPS","MARIMA.sog","MARIMA.bathymetry")

mse <- cbind(mse.arima, mse.crbm)

#' ## NRMSE

nrmse.crbm <- lapply(e.crbm, function(x)x$nrmse)
nrmse.crbm <- as.data.frame(do.call("rbind", nrmse.crbm))
nrmse.crbm$model <- "CRBM"
#names(nrmse.crbm) <-  c("CRBM.rotationGPS","CRBM.sog","CRBM.bathymetry")
names(nrmse.crbm) <-  c("rotationGPS","sog","bathymetry", "model")

nrmse.arima <- lapply(e.arima, function(x)x$nrmse)
nrmse.arima <- as.data.frame(do.call("rbind", nrmse.arima))
nrmse.arima$model <- "ARIMA"
names(nrmse.arima) <-  c("rotationGPS","sog","bathymetry", "model")

nrmse <- rbind(nrmse.arima, nrmse.crbm)

#' ### Remove Infs

nrmse <- nrmse[-which(is.infinite(nrmse[,3])),]

str(nrmse)

#' # Comparison

#' ## Mean and Median

print("CRBM")
print("Mean")
nrmse.crbm <- nrmse[nrmse$model == "CRBM",-4]
colMeans(nrmse.crbm)
print("Mode:")
qu <- apply(nrmse.crbm,2,quantile)
qu[3,]

print("MARIMA")
print("Mean")
nrmse.arima <- nrmse[nrmse$model == "ARIMA",-4]
colMeans(nrmse.arima)
print("Mode:")
qu <- apply(nrmse.arima,2,quantile)
qu[3,]

#' ## Boxplots

colMeans(nrmse)
qu <- apply(nrmse,2,quantile)
qu[3,]

library(reshape2)
test.m <- melt(nrmse)

library(ggplot2)
ggplot(test.m, aes(x = variable, y = value, fill = model)) +
  theme(text = element_text(size=15)) +
  coord_cartesian(ylim = c(0, 1.5)) +
  xlab("Variable")+
  ylab("NRMSE")+
  geom_boxplot() +
  scale_fill_manual(values = c("yellow", "orange"))
