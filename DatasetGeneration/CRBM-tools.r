#' ## General use functions

#' ### Set-up Input Structures
#'
#' A Time Series has:
#' * Batchdata: a matrix [Time x Dimensions], normalized as $\frac{batchdata - mean}{stdev}$
#' * SeqLen: The length of the time serie
#' * Data_Mean: The means for each dimension, to be able to regularize later
#' * Data_StD: The stdevs for each dimension, to be able to regularize later
#'
#' generate_data() return a list of series:
#' * list_batchdata : List of normalized batchdatas
#' * list_seqlen : List of seqlens for batchdata 'i'
#' * list_data_mean : List of means for each dimension in batchdata 'i'
#' * list_data_std : List of standard deviations for each dimension in batchdata 'i'

#Precondition: Data is in variable dataset.
generate_data <- function (targets, reqcols, normalize=TRUE)
{
    list_batchdata <- list_seqlen <- list_data_mean <- list_data_std <- list();
    for (i in 1:length(targets))
    {
        current.target <- targets[[i]];
        current.series <- dataset[dataset[[id_var]] == current.target, reqcols];
        
        data_mean <- colMeans(current.series);
        data_std <- apply(current.series, 2, sd); data_std[data_std <= 0] <- 1;

        if (normalize) {
            batchdata <- t((t(current.series) - data_mean) / data_std);
        } else {
            batchdata <- current.series;
        }


        list_batchdata[[i]] <- batchdata;
        list_seqlen[[i]] <- nrow(batchdata);
        list_data_mean[[i]] <- data_mean;
        list_data_std[[i]] <- data_std;
    }

    series <- list(batchdata = list_batchdata, seqlen = list_seqlen, data_mean = list_data_mean, data_std = list_data_std);
    return(series);
}


# width = Window width
generate_window_data <- function(serie, width, datacols) {
    res <- serie[width:nrow(serie),]

    for (r in 1:nrow(res)) {
         for (dc in datacols) {
             for (i in (width-1):0) {
                 colname <- paste(dc,(i+1),sep="")
                 res[r, colname] <- serie[r+i,dc]
             }
         }
    }

    res[,datacols] <- NULL

    return(res)
}

#' ### Plotting functions
#'
#' Functions to plot:
#'  - plot_gen_series: Original serie vs predicted.
#'  - plot_simulation: Plot serie originated by "simulation"

#plot_labels are the labels of all the dimensions orderded.
plot_gen_series <- function(series, list_gen_series, i, delay, plot_labels)
{
    batchdata <- series$batchdata[[i]];
    generated_series <- list_gen_series[[i]];

    data_mean <- series$data_mean[[i]];
    data_std <- series$data_std[[i]];

    # Plot the Series
    options(repr.plot.width=8, repr.plot.height=2);
    par(mar=c(1.1,2.1,2.1,2.1));

    plot_dimension <- function (dim.plot, label.plot)
    {
        plot.true <- batchdata[,dim.plot] * data_std[dim.plot] + data_mean[dim.plot];
        plot.pred <- generated_series[,dim.plot] * data_std[dim.plot] + data_mean[dim.plot]; ## Simultaneous Serie = 1, all time, dimension

        plot(plot.true, col = "blue", type = "l", lty = 2, xlab = "", ylab = label.plot, xlim = c(-5,length(plot.true)), ylim = c(min(plot.true, plot.pred),max(plot.true, plot.pred)));
        lines(plot.pred, col = "green");
        abline(v = delay, col = "red");
        legend("topleft", legend = c("True", "Predicted"), col = c("blue","green"), lty = c(2,1), cex = 0.75, y.intersp = 4);   
    }
    
    for (i in 1:length(plot_labels)) plot_dimension(i, plot_labels[i]);
}

plot_simulation <- function(simulation, batchdata=NA, showDelay = T)
{
    options(repr.plot.width=8, repr.plot.height=3);

    if (is.na(batchdata)) {
        batchdata <- simulation$batchdata
    }
    reconstruction.matrix <- simulation$reconstruction;

    par(mar=c(1.1,2.1,2.1,2.1));
    for(i in 1:ncol(batchdata)) {
        if (showDelay) { 
            plot(batchdata[,i], type = "l", col = "blue", ylab = "");
            lines(reconstruction.matrix[,i], col = "green");
        } else {
            idx <- (crbm$delay+1):nrow(batchdata)
            plot(batchdata[idx,i], type = "l", col = "blue", ylab = "");
            lines(reconstruction.matrix[idx,i], col = "green");
        }
    }
}


#' ### Prediction functions

generate_serie <- function(batchdata, n_gibbs)
{
	if (nrow(batchdata) < crbm$delay) return(NULL);
    
    samples.aux <- nrow(batchdata) - crbm$delay
    data_idx <- crbm$delay + 1
    hist_idx <- data_idx - 1:crbm$delay
   
    #generated_series.aux <- predict_crbm(crbm, orig_data, orig_history, n_samples = samples.aux, n_gibbs = n_gibbs);
                

    series <- batchdata[c(data_idx,hist_idx),]

    gen <- forecast.crbm(crbm, series, n_samples = samples.aux, n_gibbs = n_gibbs)


    rbind(batchdata[1:crbm$delay,], gen$generated)
}

predict_series_crbm <- function(crbm, series, n_gibbs = 30, n_threads = 1)
{
	# Parallellization (if indicated)
	if (n_threads > 1)
	{
		library(parallel);
		cl <- makeCluster(n_threads, type='FORK');
		l <- parLapply(cl,series$batchdata, generate_serie, n_gibbs=n_gibbs);
		stopCluster(cl);
	} else {
		l <- lapply(series$batchdata, generate_serie, n_gibbs=n_gibbs);
	}

	l;
}


# Regular prediction but adding batchdata to the object for convenience.
predict_simulation <- function(crbm, batchdata) {
    sim <- predict(crbm, batchdata)
    if (is.null(sim)) {
        sim <- list()
        sim$activation <- array(-1,c(nrow(batchdata),crbm$n_hidden));       
        sim$reconstruction <- array(-1,c(nrow(batchdata),ncol(batchdata)));       
        sim$nullSim <- TRUE
    }        
    sim$batchdata <- batchdata
    sim
}

#' #### Error evaluation

eval.error <- function(series, list_gen_series)
{
    error.mse <- list();
    error.mae <- list();
    len.err <- list();
    
    max.dims <- max(sapply(series$batchdata, function(x) dim(x)[2]));
    for (i in 1:max.dims) { error.mse[[i]] <- 0; error.mae[[i]] <- 0; len.err[[i]] <- 0; }
    
    for (i in 1:length(list_gen_series))
    {
        data <- series$batchdata[[i]];
        if (nrow(data) < crbm$delay) next;
            
        gen_series <- list_gen_series[[i]];

        data_mean <- series$data_mean[[i]];
        data_std <- series$data_std[[i]];
        
        for(j in 1:(dim(data)[2]))
        {
            value.true <- data[,j];
            value.pred <- gen_series[,j];

            value.true.reg <- (value.true * data_std[j]) + data_mean[j];
            value.pred.reg <- (value.pred * data_std[j]) + data_mean[j];
            
            error.mse[[j]] <- error.mse[[j]] + mean((value.pred.reg - value.true.reg)^2, na.rm = TRUE);
            error.mae[[j]] <- error.mae[[j]] + mean(abs(value.pred.reg - value.true.reg), na.rm = TRUE);
            len.err[[j]] <- len.err[[j]] + 1;
        }
    }
    
    for (i in 1:length(error.mse))
    {
        print(paste("Dimension: ", i, sep = ""));
        print(paste("MSE: ", round(error.mse[[i]] / len.err[[i]],3), sep = ""));
        print(paste("MAE: ", round(error.mae[[i]] / len.err[[i]],3), sep = ""));
    }        
}

#' #### Simulation forecast functions

sigmoid_func <- function(mat)
{
	1 / (1 + exp(-mat));
}

sample_bernoulli <- function(mat)
{
	dims <- dim(mat);
	array(rbinom(n = prod(dims), size = 1, prob = c(mat)), dims);
}

## Operator to add dimension-wise vectors to matrices
`%+%` <- function(mat, vec)
{
	retval <- NULL;
	tryCatch(
		expr = { retval <- if (dim(mat)[1] == length(vec)) t(t(mat) + vec) else mat + vec; },
		warning = function(w) { print(paste("WARNING: ", w, sep = "")); },
		error = function(e) { print(paste("ERROR: Cannot sum mat and vec", e, sep = "\n")); }
	);
	retval;
}


#' #### Error evaluation for simulation

error_simulation_single <- function(simulation)
{            
    if (!is.null(simulation)) {
        batchdata <-  simulation$batchdata
        if (crbm$delay + 1 < nrow(batchdata)) { #At least 2 elements
            
            real <- batchdata[(crbm$delay+1):nrow(batchdata),]
            pred <- simulation$reconstruction[(crbm$delay+1):nrow(batchdata),]

            MSE <- as.data.frame(t(colSums((real-pred)^2, na.rm=TRUE)/nrow(pred))) #First value is not predicted.
            RMSE <- sqrt(MSE)
            sd <- apply(real,2,sd)
            NRMSE <- RMSE/sd

            # Add prefixes
            colnames(MSE) <- paste("MSE_",colnames(MSE), sep="")
            colnames(RMSE) <- paste("RMSE_",colnames(RMSE), sep="")
            colnames(NRMSE) <- paste("NRMSE_",colnames(NRMSE), sep="")

            return(cbind(MSE,RMSE, NRMSE))
        }
    }     
    #else {
    #return(data.frame(mse=NA, rmse=NA, nrmse=NA))
    return(NA)
    #}
}

error_simulation <- function(simulationList) {
    simulation.error  <- lapply(simulationList, error_simulation_single)
    # Append all rows and expand NAs given the colnames
    simulation.error <- do.call("rbind", simulation.error)
}


# #' Clustering

closest.cluster <- function(x, kc)
{
    cluster.dist <- apply(kc$centers, 1, function(y) sqrt(sum(`^`(x - y, 2))));
    which.min(cluster.dist)[1];
}

detect_phases_test <- function (target, indices, kc, phase)
{
    if (target >= length(indices)) return(NULL);

    batchdata <- series_test$batchdata[[target]];
    data_mean <- series_test$data_mean[[target]];
    data_std <- series_test$data_std[[target]];
    
    if (nrow(batchdata) <= crbm$delay)
    {
        message("Warning: Serie smaller than delay");
        return(NULL);
    }
        
    par(mar=c(1.1,2.1,1.1,2.1), mfrow = c(2,1));
    
    
    # Detected Phases
    phase.segment <- phase[indices[target]:(indices[target + 1] - 1)];
    ##plot.cluster <- c(rep(0,crbm$delay),phase.segment) + 1; 
    plot.cluster <- phase.segment; 

    cols <- rainbow(length(kc$size) + 1)[plot.cluster];

    
    # Plot True Resources
    for (i in 1:crbm$n_visible)
    {
        plot.true <- (batchdata[,i] * data_std[i]) + data_mean[i];

        plot(plot.true, col = "blue", type = "l", lty = 1, xlab = "", ylab = "",
             ylim = c(min(plot.true),max(plot.true))
        );
        abline(v = crbm$delay, col = "red")
    }
    barplot(plot.cluster, col = cols, border = NA );

    return(plot.cluster)
}



#' # Create phase data


detect_phases_series_train <- function (target, indices, phase)
{    
    if (target >= length(indices)) return(NULL);
    #message(paste("Detecting phase for target: ", as.character(target)))
    
    batchdata <- series_train$batchdata[[target]]; #TRAIN
    data_mean <- series_train$data_mean[[target]];
    data_std <- series_train$data_std[[target]];
    
    if (nrow(batchdata) <= crbm$delay)
    {
        message("Warning: Serie smaller than delay");
        return(rep(NA, nrow(batchdata)));
    }
        
    
    # Detected Phases
    phase.segment <- phase[indices[target]:(indices[target + 1] - 1)];
    plot.cluster <- c(rep(0,crbm$delay),phase.segment) + 1; 
    return(plot.cluster)
}

detect_phases_series <- function (target, crbm, series, indices, phase)
{    
    if (target >= length(indices)) return(NULL);
    message(paste("Detecting phase for target: ", str(target)))
    
    batchdata <- series$batchdata[[target]]; #TEST
    data_mean <- series$data_mean[[target]];
    data_std <- series$data_std[[target]];
    
    if (nrow(batchdata) <= crbm$delay)
    {
        message("Warning: Serie smaller than delay");
        return(rep(NA, nrow(batchdata)));
    }
        
    
    # Detected Phases
    phase.segment <- phase[indices[target]:(indices[target + 1] - 1)];
    #plot.cluster <- c(rep(0,crbm$delay),phase.segment) + 1; 
    return(phase.segment+1)
}

produce_data_phases_train <- function(target, activations, indices, kc, 
                                      tr_targets) {    
    phase <- apply(activations, 1, closest.cluster,kc=kc);        
        
    pclust <- detect_phases_series_train(target, indices, phase);
    cbind(dataset[which(dataset[,id_var] == tr_targets[target]),],cluster=as.factor(pclust))   #So we have the unused vars!                
}

produce_data_phases <- function(target, crbm, series, indices, kc, activations,
                                t_targets) {
    phase <- apply(activations, 1, closest.cluster,kc=kc);        
        
    pclust <- detect_phases_series(target, crbm, series, indices, phase)
    cbind(dataset[which(dataset[,id_var] == t_targets[target]),],cluster=as.factor(pclust))   #So we have the unused vars!                
}




#' # Activation dataset creation

#Precondition: Data is in variable dataset.
generate_data_full <- function (targets, dataset)
{
    list_batchdata <- list_seqlen <- list();
    for (i in 1:length(targets))
    {
        current.series <- dataset[dataset[[id_var]] == targets[[i]], ];
        
        
        list_batchdata[[i]] <- current.series;
        list_seqlen[[i]] <- nrow(current.series);
    }

    series <- list(batchdata = list_batchdata, seqlen = list_seqlen)
    return(series);
}

merge_data <- function(indices, data, activations, reqcols) {                
    datalist <- list()
    for (i in 1:(length(indices)-1))
    {            
        if (length(reqcols) > 0) {
            current.series <- data$batchdata[[i]][, reqcols];                 
        } else {
            current.series <- data$batchdata[[i]];                 
        }
        
        if (nrow(current.series) > 0) {
            current.activations <- activations[indices[i]:(indices[i+1]-1), ] #Select our activations from i to i+1 (-1)
            #At this point we sould have the data readly for merging

            datalist[[i]] <- cbind(current.series,activations=current.activations) #list of all the data.
        }
    }
    return(datalist)
}
