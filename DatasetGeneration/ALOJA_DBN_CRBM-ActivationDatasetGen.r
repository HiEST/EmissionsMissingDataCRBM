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

#' This file provides an script version to create the activations dataset using script arguments.

#' # Environment setup
#'
#' Here we force OpenMP and OpenBLAS to use n_cores threads


n_cores <- 40
Sys.setenv(OPENBLAS_NUM_THREADS=n_cores, OMP_NUM_THREADS=n_cores)


#' The following packages are requiered for this to work. RRBM package can be found here: https://github.com/josepllberral/machine-learning-tools
library(rrbm)
library(parallel)
library(optparse)

#' Load auxiliary functions used for this notebook. They are in a different R
#' script for the sake of readability 
source("CRBM-tools.r")

ihsPath <- "IHSTestData.txt"

option_list = list(
    make_option(
               c("-n", "--n_hidden"), type= "numeric", default=10,
               help="Number of hidden units. Default 10."
    ),
    make_option(
               c("-d", "--delay"), type= "numeric", default=20,
               help="Delay. Number of samples to include in CRBMs window. Default 20"
    ),
    make_option(
               c("-e", "--training_epochs"), type= "numeric", default=300,
               help="Number of training epochs. Default 300."
    ),
    make_option(
               c("-b", "--batch_size"), type= "numeric", default=200,
               help="Batchdata size. Default 200."
    ),   
    make_option(
               c("-m", "--momentum"), type= "numeric", default=0.1,
               help="Momentum for CRBM. Default 0.1."
    ),
    make_option(
               c("-l", "--learning_rate"), type= "numeric", default=1e-6,
               help="Learning rate for CRBM. Default 1e-6."
    ),
    make_option(
               c("-i", "--data_file"), type= "character", default="AISPreprocessedData.data",
               help="Input data.", dest="dataFile"
    ),  
    make_option(
               c("-f", "--fast_test"), type= "numeric", default=-1,
               help="Whether to do a fast test of the script using the N random series or not"
    ),
    make_option(
               c("-o", "--output"), type= "character", default="",
               help="Output file for dataset. Default uses date and parameters to generate filename."
    ),
    make_option(
               c("--crbm_output"), type= "character", default="",
               help="Output file for CRBM. Default uses date and parameters to generate filename."
    )
)

#' CRBM Params

args = commandArgs(trailingOnly=TRUE)

# If there is only one parameter, this is being executed in Jupyter with IRKernel.
# In this case you should change --data_file default data path
if (length(args) == 1) {
    args = c()
}

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser, args, print_help_and_exit=T)

opt

attach(opt)

#' # Parameters setup

#' Select variables

id_var <- "imo"
reqcols <- c("rotationGPS","sog","bathymetry3")
plot_labels <-c("Rotation GPS","Speed Over Ground", "3-leveled Bathymetry")#

#' Data and result paths
today <- as.character(Sys.Date())
cols <- paste(reqcols,collapse="-")

 # You may need to change to somewhere you want the results if you're using Jupyter
if (crbm_output == "") {
    print("CRBM save path not set. Setting default:")
    (crbm_output <- paste("Results/",today,"-shipcrbm","-",cols,"-","d_",delay,"-h_",n_hidden,"-t_",training_epochs,".Rdata",sep=""))
}

#' Create folders for output

dir.create(path = dirname(output))
dir.create(path = dirname(crbm_output))


#' # Data preparation 
#'
#' Dataframe to time series:
#'
#' * Each "id_exec" is a time serie.
#' * Each "id_exec" at "time t" has resource usages (dimensions)
#'
#' After getting the time series, split them into Training and Test

load(dataFile); dataset <- ships
targets <- unique(dataset[,id_var]);
if (fast_test != -1) {
    cat("Test mode. Limiting the data to the", fast_test, "random series\n")
    set.seed(1337)
    targets <- sample(targets, size=fast_test)

    crbm_output <- paste(crbm_output, ".test", sep="")
    output <- paste(output, ".test", sep="")
}


#' ## Train-Test split
#'
#' Random seed set to get repetible partitioning between train and test
#' Notice that we split by series, not by samples! The individuals in this
#' problem are the series, not each sample. Training split contains 66% and test split 34%.

set.seed(1337) 
all_idx <- 1:length(targets);
tr_idx <- sample(all_idx, ceiling(length(all_idx) * 0.66));
tt_idx <- all_idx[!all_idx %in% tr_idx];
tr_targets <- targets[tr_idx];
tt_targets <- targets[tt_idx];

#' ## Generate Data for training and test
#' In order to use our dataset with the *rrbm package*, we will modify the data
#' structure for conveniende:
#'
#' generate_data() return a list of series:
#' * list_batchdata : List of normalized batchdatas
#' * list_seqlen : List of seqlens for batchdata 'i'
#' * list_data_mean : List of means for each dimension in batchdata 'i'
#' * list_data_std : List of standard deviations for each dimension in batchdata 'i'

series_train <- generate_data(tr_targets, reqcols);
series_test <- generate_data(tt_targets, reqcols);

#' Then we append all the data by series
batchdata <- do.call("rbind",series_train$batchdata)

#' # CRBM Training and validation

#' ## Training


if (!file.exists(crbm_output)) { 
    crbm <- train.crbm(batchdata, series_train$seqlen,
          learning_rate = learning_rate,
          training_epochs = training_epochs, 
          batch_size = batch_size,
          n_hidden = n_hidden, 
          delay = delay,
          momentum = momentum
    );

    # Save CRBM result
    #
    # In order to avoid recomputing, we save the CRBM as a checkpoint of the whole
    # problem.

    save(crbm,file=crbm_output)
} else {
    print("Model exists. Loading it...")
    load(crbm_output)
}

#' # Create Activations dataset

print("Generating activations")

train.simulations <- lapply(series_train$batchdata, function(data) predict_simulation(crbm, data));
test.simulations <- lapply(series_test$batchdata, function(data) predict_simulation(crbm, data));

# Activations training
activations.training <- lapply(train.simulations, function(x) x$activation)
indices.training <- c(1,sapply(activations.training,nrow))
for (i in 2:length(indices.training)) {
    indices.training[i] <- indices.training[i]+indices.training[i-1]
}
activations.training <- do.call("rbind", activations.training)                                                     

# Activations test
activations.test <- lapply(test.simulations, function(x) x$activation)
indices.test <- c(1,sapply(activations.test,nrow))
for (i in 2:length(indices.test)) {
    indices.test[i] <- indices.test[i]+indices.test[i-1]
}
activations.test <- do.call("rbind", activations.test)                                                     

full_series_train <- generate_data_full(tr_targets, dataset);
full_series_test <- generate_data_full(tt_targets, dataset);


reqcols <- c("imo", "fechahora", "type")

data.activation.train <- merge_data(indices.training, full_series_train, activations.training, reqcols)
data.activation.test <- merge_data(indices.test, full_series_test, activations.test, reqcols)

data.activation <- c(data.activation.train, data.activation.test)

data.activation.df <- do.call("rbind", data.activation)

#'  # Activation dataset with Main Engine power
#'

print("Appending Main Engine Power")

ihs <- read.table(ihsPath, sep="\t", header=T)

ihsSub <- ihs[,c("LRIMOShipNO","installedPowerME")]
ihsAct <- merge(ihsSub, data.activation.df, by.x="LRIMOShipNO", by.y="imo", all.y=T) # Right join

names(ihsAct)[names(ihsAct) == 'LRIMOShipNO'] <- 'imo'

#' # Add original features

print("Adding original features")
reqcols <- c("fechahora","imo","rotationGPS","sog","bathymetry3")
datacols <- c("rotationGPS","sog","bathymetry3")
ship_list <- split(dataset, dataset$imo)
ship_list['0'] <- NULL # Remove Ships without IMO


cl <- makeCluster(n_cores)
clusterExport(cl, varlist=c("generate_window_data", "datacols"))
historyDataset <- parLapply(cl,ship_list, fun=generate_window_data, width=opt$delay, datacols=datacols)
stopCluster(cl)
historyDataset <- do.call("rbind",historyDataset)

complete_data <- merge(historyDataset, ihsAct)

if (opt$output == "") {
    all_file_path <- paste("Datasets/",today,"-activation+original","-",cols, "-","d_",delay,"-h_",n_hidden,"-t_",training_epochs, ".csv",sep="");
} else {
    all_file_path <- opt$output
}

all_file_path


print("Saving dataset")

write.table(complete_data, file=all_file_path, sep=",", row.names=F)

colnames(historyDataset)
