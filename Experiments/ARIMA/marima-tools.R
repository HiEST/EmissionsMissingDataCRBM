library(marima)
library(NMOF)
library(parallel)


#####FUNCTIONS

plotMARIMA <- function(marimaList) {
  mod <- marimaList$model
  #Data vs prediction
  plot(mod$DATA[1,], col="red", type="l", main="Real vs Predicted")
  lines(mod$fitted[1,], col="blue")
  ###
  plot(mod$DATA[2,], col="red", type="l", main="Real vs Predicted")
  lines(mod$fitted[2,], col="blue")
  
  plot(mod$residuals[1,], type="l", col="red", main="Residuals")
  plot(mod$residuals[2,], type="l", col="blue", main="Residuals")
}

buildMARIMA <- function(data, AR,MA,penalty=1,meansAdjusted=1) {
#Build ARIMA model
  Model2   <- define.model(kvar=ncol(data), ar=AR, ma=MA)
  Marima2  <- marima(data, means=meansAdjusted, ar.pattern=Model2$ar.pattern, 
                     ma.pattern=Model2$ma.pattern, Check=FALSE, Plot="none", penalty=penalty)

  MSE <- rowSums(Marima2$residuals^2, na.rm=TRUE)/(ncol(Marima2$residuals)-1) #First value is not predicted.
  RMSE <- sqrt(MSE)
  sd <- apply(data,2,sd)
  NRMSE <- RMSE/sd
  
  return(list(model=Marima2,mse=MSE,rmse=RMSE, nrmse=NRMSE))
}

rmseMARIMA <- function(data, params) {
  if (params[1] == 0 & params[2] == 0) {
    warning("MARIMA requieres AR or MA to be different to 0.")
    return(Inf)
  }
  
  AR <- params[1]
  MA <- params[2]
  penalty <- params[3]
  meansAdjusted <- params[4]

  jnrmse <- tryCatch(
      {     
          m <- buildMARIMA(data,AR,MA,penalty=penalty, meansAdjusted = meansAdjusted)
          sum(m$nrmse) #Sum of both RMSE
      }, error = function(e) {            
          warning(paste("Modeling failed:", e$message))
          Inf          
      }
  )    
    
  return(jnrmse)
}

gridMARIMA <- function(data, v, l, cluster=NULL) { 
  message("Modeling ", data$id[1])    


  df <- data[,v]  
  
  g <- gridSearch(rmseMARIMA,levels, data=df, printDetail=FALSE, cl=cluster) #Send Cluster to gridSearch
  
  ##Best model
  param <- g$minlevels  
  ret <- tryCatch(
    {     
      bestmarima <- buildMARIMA(df,param[1],param[2],param[3],param[4])      
      list(model=bestmarima, grids = g)
    },    
    error = function(e) { list(model=NULL, message=e$message) }
  )  
  return(ret)      
}


stepwiseForecast <- function(model, s) {
  f <- arma.forecast(series=s, marima=model, nstart=nrow(s)-1, nstep=1, check = FALSE )
  
  MSE <- rowSums(f$residuals^2, na.rm=TRUE)/(ncol(f$residuals)-1) #First value is not predicted.
  RMSE <- sqrt(MSE)
  sd <- apply(s,2,sd)
  NRMSE <- RMSE/sd
    
  return(list(data=s, forecast=f$forecast, mse = MSE, rmse = RMSE, nrmse = NRMSE))
    
}

plotForecast <- function(f, col) {
  plot(f$data[,col], type="l", col="blue", main="Real vs 1-Step forecasted data")
  lines(f$forecast[col,], col="red")
}


test <- function() {
  #####MAIN
  load("/home/agutierrez/data/AIS-Preprocessed-1week-2017-5-22.data")
  
  ##Parameters
  levels <- list(AR=0:1,MA=0:1,penalty=0:1,meansAdjusted=0:1)
  sp <- split(ships, ships$id)
  vars <- c("rotationGPS", "sog")
  
  ##Execute
  cl <- makeCluster(detectCores(),type="FORK") #ToDo this out.
  r <- lapply(sp[1:3], gridMARIMA, v=vars, l=levels, cluster=cl)
  stopCluster(cl)
  
  ##Get messages from failed models
  errors <- unlist(sapply(r, function(x) if(is.null(x$model)) x$message))
  
  ##Analysis
  plotMARIMA(r[[1]]$model)
}



