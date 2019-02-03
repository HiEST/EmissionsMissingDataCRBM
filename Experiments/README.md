# Experiments

In this document we will cover the following experiments:
+ [ARIMA](#arima): Code to train the multivariate ARIMAs
+ [NavStatus](#navigation-status): Code and notes for checking the navigational status correlations
+ [ShipTypePrediction](#ship-type-prediction): Code used to predict the ship type
+ [MainEnginePrediction](#main-engine-prediction): Code used to predict the main engine power


## ARIMA

- MARIMAExperiments: This script generates one ARIMA model per ship trace and
  save the results so you can analyze it later.
- ARIMAvsCRBM: This script processes the previously generated ARIMA models'
  error and the loaded CRBM error (both Mean Squared Error) and returns a table
  and plots for comparison.


## Navigation status

The first part of this experiment experiment can be done doing a confusion 
matrix of the K-Means dataset with the navstatus and cluster variables.
navstatuscluster scrip can do this.

The second part can be seen with our visualization tool at http://patrons.bsc.es/ 
selecting dataset "CRBMResults", ship 206 and varible "cluster". All the ships 
and variables can be explored with it.


## Ship type prediction

get_classification_results iterates over datasets, models and hyperparameters to
obtain the best result. This is done using sklearn's GridSearchCV that enables
us to do grid search of hyperparameters and evaluate them using
cross-validation.

## Main engine prediction

MainEngine_regression_loop works in the same way as [Ship type prediction](#ship-type-prediction).

