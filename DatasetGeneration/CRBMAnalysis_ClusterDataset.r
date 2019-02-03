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

#' # Environment setup
#'
#' Here we force OpenMP and OpenBLAS to use n_cores threads

n_cores <- 40

Sys.setenv(OPENBLAS_NUM_THREADS=n_cores, OMP_NUM_THREADS=n_cores)


#' Load package
library(rrbm)
library(parallel)

#' Load auxiliary functions used for this notebook. They are in a different R
#' script for the sake of readability 
source("CRBM-tools.r")


#' # Parameters setup

id_var <- "imo"
reqcols <- c("rotationGPS","sog","bathymetry3")
plot_labels <-c("Rotation GPS","Speed Over Ground", "3-leveled Bathymetry")#

#' CRBM Params
delay = 20
n_hidden = 30
training_epochs = 300


#' Data and result paths. Change these paths with the location of your data and the wanted result path.
dataPath <- ""
resultPath <- "Results/"
dataFile <- file.path(dataPath,"AISPreprocessedData.data")

today <- as.character(Sys.Date())
cols <- paste(reqcols,collapse="-")
(crbmSavePath <- paste(resultPath,today,"-v2-shipcrbm","-",cols,"-","d_",delay,"-h_",n_hidden,"-t_",training_epochs,".data",sep=""))

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


#' ## Train-Test split
#'
#' Random seed set to get repetible partitioning between train and test
#' Notice that we split by series, not by samples! The individuals in this
#' problem are the series, not each sample. 66% Training, 33% Test

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

crbm <- train.crbm(batchdata, series_train$seqlen,
      learning_rate = 1e-6,
      training_epochs = training_epochs,
      batch_size = 200,
      n_hidden = n_hidden, 
      delay = delay,
      momentum = 0.1
);

#' Save CRBM result
#'
#' In order to avoid recomputing, we save the CRBM as a checkpoint of the whole
#' problem.

save(crbm,file=crbmSavePath)

crbmSavePath

#' ## Validate the training process using series generation
#'
#' Here we will validate the CRBM training in two ways: with series generation
#' and simulation. Each concept will be introduced in its corresponding section.

#' ### Generate series from $delay+1$ samples
#'
#' In this step what we will do is to produce a series giving only the first
#' $delay+1$ samples, a data point plus its history, and produce a series with 
#' the same length as the original series. Then we will calculate the difference
#' between the two series with error functions. 


list_gen_series <- predict_series_crbm(crbm, series_train, n_threads=detectCores()-1);
list_gen_series_test <- predict_series_crbm(crbm, series_test, n_threads=detectCores()-1);


plot_gen_series(series_train, list_gen_series, 2, crbm$delay, plot_labels);

plot_gen_series(series_test, list_gen_series_test,8, crbm$delay, plot_labels);

#' ### Evaluate error

eval.error(series_train, list_gen_series);

eval.error(series_test, list_gen_series_test);


#' ## Validate the training process with series simulation
#'
#' Now instead of using just the first window of data + history we use a sliding
#' window (1 step window with size delay+1) to forecast the series. This is
#' called **simulation**.

#' First we test it with one series.
sim <- predict_simulation(crbm, series_test$batchdata[[4]])
plot_simulation(sim)

#' Now that we have seen how it works, we will do it for every series.
#'
#' **WARNING**: Do not try to parallelize the following code. It already uses BLAS, which parallelizes to some extent. If you try to use parallel, it will crash.
train.simulations <- lapply(series_train$batchdata, function(data) predict_simulation(crbm, data));

#' Errors will be NA

test.simulations <- lapply(series_test$batchdata, function(data) predict_simulation(crbm, data));

train.error.simulation <- error_simulation(train.simulations)

train.error.simulation

cbind(
    mean=colMeans(train.error.simulation, na.rm=T),
    sd=apply(train.error.simulation,2, sd, na.rm=T)
)

test.error.simulation <- error_simulation(test.simulations)

train.error.simulation

cbind(
    mean=colMeans(train.error.simulation, na.rm=T),
    sd=apply(train.error.simulation,2, sd, na.rm=T)
)

today <- as.character(Sys.Date())
cols <- paste(reqcols,collapse="-")
(simSavePath <- paste(resultPath,today,"-shipcrbm-simulations-",cols,"-","d_",delay,"-h_",n_hidden,"-t_",training_epochs,".data",sep=""))
save(train.simulations,test.simulations,train.error.simulation,test.error.simulation, file=simSavePath)

#' # Generating data for clustering

#' ## Training simulation

activations.training <- lapply(train.simulations, function(x) x$activation)

indices.training <- c(1,sapply(activations.training,nrow))
for (i in 2:length(indices.training)) {
    indices.training[i] <- indices.training[i]+indices.training[i-1]
}

activations.training <- do.call("rbind", activations.training)                                                     

#' ## Testing simulation

activations.test <- lapply(test.simulations, function(x) x$activation)

indices.test <- c(1,sapply(activations.test,nrow))
for (i in 2:length(indices.test)) {
    indices.test[i] <- indices.test[i]+indices.test[i-1]
}

activations.test <- do.call("rbind", activations.test)                                                     

#' # Clustering

clusterK <- 5
set.seed(1337)
kc <- kmeans(activations.training, clusterK);

kc$withinss
kc$betweenss

cl <- makeCluster(detectCores()-1, type="FORK");
phase <- parApply(cl, activations.test, 1, closest.cluster,kc=kc);
stopCluster(cl);

id <- 34
pclust <- detect_phases_test(id, indices.test, kc, phase);

#plot
ship <- cbind(dataset[which(dataset[,id_var] == tt_targets[id]),],pclust)
pal <- rainbow(clusterK)
par(mar=c(1,1,1,1), mfrow = c(1,1));
plot(ship$latitude, ship$longitude, col=pal[pclust], pch=16)

#' # Create data from clustering

#Train
cl <- makeCluster(n_cores, type = "SOCK")
clusterExport(cl, varlist = c("detect_phases_series", "closest.cluster",
                              "dataset","id_var"))
d_tr <- parLapply(cl,1:length(tr_targets), produce_data_phases, crbm=crbm, series=series_train,
                  indices=indices.training, kc=kc,
                  activations = activations.training,
                  t_targets = tr_targets)
stopCluster(cl)

names(d_tr) <- tr_targets

#Test
cl <- makeCluster(n_cores, type = "SOCK")
clusterExport(cl, varlist = c("detect_phases_series", "closest.cluster",                    
                              "dataset","id_var"))
d_tt <- parLapply(cl,1:length(tt_targets), produce_data_phases, crbm=crbm, series=series_test,
                  indices=indices.test, kc=kc,
                  activations = activations.test,
                  t_targets = tt_targets)
stopCluster(cl)

str(d_tt)

names(tt_targets) <- tt_targets

ships_train <- do.call("rbind", d_tr) #Merging data
ships_test <- do.call("rbind", d_tt) #Merging data

str(ships_train)
str(ships_test)

kmeansSavePath <- paste(resultPath,"kmeansDataset/",today,"-kmeansresult","-",cols,  "-k_",clusterK, "-","d_",delay,"-h_",n_hidden,"-t_",training_epochs,sep="");

write.table(ships_train, file=paste(kmeansSavePath,"-train.csv", sep=""), sep=";", row.names=F)
write.table(ships_test, file=paste(kmeansSavePath,"-test.csv", sep=""), sep=";", row.names=F)
