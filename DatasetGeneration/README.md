# Dataset generation

## ALOJA_DBN_CRBM-ActivationDatasetGen

Script that generates the activation features from CRBM. It can handle the
following arguments on command line:
- -n/--n_hidden: Number of hidden units of the CRBM, i.e. number of activations.
- -d/--delay: Number of samples to be used as CRBM's window.
- -e/--training_epochs: Number of training epochs.
- -b/--batch_size: Number of samples per training mini-batch.
- -m/--momentum: Momentum for the optimization algorithm.
- -l/--learning_rate: Learning rate for the optimization algorithm.
- -i/--data_file: Input data.
- -f/--fast_test: Whether to run everything using the whole dataset or N samples.
- -o/--output: Output file path for the dataset.
- --crbm_output: Path to save CRBM model.

If it is run in Jupyter it will use the default parameters; follow the notebook
instructions to find the configuration suitable for you.


## CRBMAnalysis_ClusterDataset

Script that generates the clustered dataset. Please follow the notebook
instructions or the R code comments to modify the input variables.
