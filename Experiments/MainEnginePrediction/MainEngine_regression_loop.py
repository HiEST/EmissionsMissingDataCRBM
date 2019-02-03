# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.3'
#       jupytext_version: 0.8.6
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %load_ext autoreload
# %autoreload 2

# +
import sklearn
from sklearn import *
import numpy as np
import pandas as pd
import datetime
import os
import time

from math import sqrt

from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import GroupKFold 

from sklearn.pipeline import Pipeline

from sklearn.externals import joblib

from argparse import ArgumentParser
# -

# Import custom pipeline classes from _pipefunctions.py_

from pipefunctions import *
from auxiliar_functions import *

# # Parse parameters

parser = ArgumentParser()
parser.add_argument("-d", "--input_data", metavar="FILE",
        help = "Input data for the experiment.")
parser.add_argument("-o", "--output_folder", metavar="FILE",
        help = "Output folder path for experiment results.")
parser.add_argument("-f", "--jupyter_config", metavar="FILE",
        help = "STUB. This is here to ignore jupyter's config.")

args = parser.parse_args()

if args.jupyter_config is not None: 
    print("Executing on Jupyter. Setting default params...")
    input_data="crbmdata.csv"
    output_folder="/tmp/MainEngine_regression_loop-Jupyter"
else: 
    input_data = args.input_data
    output_folder = args.output_folder

# Create output folder

if not os.path.exists(output_folder):
    os.makedirs(output_folder)

results_file=output_folder+"/result.csv"

# # Read data

# Read dataset
df = pd.read_csv(input_data)
# -

# # Preprocess

# First we define the pipeline we want to execute

preprocess = Pipeline([
        ("dropNA", Droper()), 
        ('logaritmizer', Logaritmizer(inputColumn='installedPowerME', outputColumn='logInstalledPowerME')),
        ('stringCast', StringCaster(column='type')),
        ('dummizer', Dummizer(inputColumns=['type'], outputPrefix='binType'))
        # ('binner', Binner(inputColumn='logInstalledPowerME', outputColumn='binLogInstalledPowerME', bins=10)),
        # ('binnerMid', BinnerMid(inputColumn='binLogInstalledPowerME', outputColumn='binmidLogInstalledPowerME')),
        # ('stringCastBin', StringCaster(column='binmidLogInstalledPowerME'))
        ])


# And then we execute it with transform

df = preprocess.transform(df)


# ## Split train/test

# +
imolist = df['imo'].drop_duplicates()
nships = len(imolist)
print(nships)
# Sample IMOs
np.random.seed(2)
trainimo = np.random.choice(imolist, int(nships*0.8), replace=False)

# Get data by IMO
df_train =  df.loc[df['imo'].isin(trainimo)]
df_test =  df.loc[~df['imo'].isin(trainimo)]

# Train/Test groups
group_train = df_train['imo']
group_test = df_test['imo']
# -

df_train.shape

df_test.shape

list(df_train.columns)

# # Training


# ## General parameters

# +
target = 'logInstalledPowerME'
ptn = PandasToNumpyXY(response=target)

k = 3
cv = GroupKFold(k)
# -

# ## Datasets

# +
# Features
binType = [col for col in df.columns if 'binType_' in col]
f_act = [col for col in df.columns if 'activations' in col]
f_rot = [col for col in df.columns if ('rotationGPS' in col and 
                                       'rotationGPSA' not in col and
                                       'rotationGPSW' not in col)]
f_sog = [col for col in df.columns if 'sog' in col]
f_bat = [col for col in df.columns if 'bathymetry' in col]

# Datasets
# feat_type = binType 
# feat_all =  f_act + f_rot + f_sog + f_bat + binType
feat_activation =  f_act + binType
feat_history = f_rot + f_sog + f_bat + binType
# -

#
# ## Average params

avg = Meanizer()

# ## Lasso params 

lasso_grid = {'alpha': [0.0001,0.001,0.01,0.1]}
lasso = sklearn.linear_model.Lasso()

# ## Gradient Boosting params

gb_grid = {
    'max_depth': [3,5],
    'min_samples_split': [2,5],
    'n_estimators': [50, 100, 150, 200],
    'learning_rate': [0.0001,0.001, 0.01, 0.1]
}
gb = sklearn.ensemble.GradientBoostingRegressor(n_estimators=200)

# ## Random Forest

# +
rf_grid = {
    'max_features': ["auto", "sqrt", "log2"],
    'n_estimators': [200, 1000],
    'max_depth': [5,10, None]
}

rf = sklearn.ensemble.RandomForestRegressor(n_estimators=200, n_jobs=-1)
# -

# ## SVM

# The results of the SVM were not got enough for the execution time that it takes

# +
# svm_grid = None

# svm = sklearn.svm.SVR(kernel='linear')
# -

# ##  Final Structure


params = dict()

# +
params['Global average'] = dict({
                        'target':target,
                        'grid':None,
                        'features':None,
                        'model':avg
                 })

params['Type average'] = dict({
                        'target':target,
                        'grid':None,
                        'features':feat_type,
                        'model':avg
                 })

params['Lasso Activations'] = dict({
                        'target':target,
                        'grid':lasso_grid,
                        'features':feat_activation,
                        'model':lasso
                 })

params['Lasso History'] = dict({
                        'target':target,
                        'grid':lasso_grid,
                        'features':feat_history,
                        'model':lasso
                 })

params['GB Activations'] = dict({
                        'target':target,
                        'grid':gb_grid,
                        'features':feat_activation,
                        'model':gb
                 })

params['GB History'] = dict({
                        'target':target,
                        'grid':gb_grid,
                        'features':feat_history,
                        'model':gb
                 })

params['RF Activations'] = dict({
                        'target':target,
                        'grid':rf_grid,
                        'features':feat_activation,
                        'model':rf
                 })

params['RF History'] = dict({
                        'target':target,
                        'grid':rf_grid,
                        'features':feat_history,
                        'model':rf
                 })

# +
# params['SVM Activations'] = dict({
#                         'target':target,
#                         'grid':svm_grid,
#                         'features':feat_activation,
#                         'model':svm
#                  })
# 
# params['SVM History'] = dict({
#                         'target':target,
#                         'grid':rf_grid,
#                         'features':feat_history,
#                         'model':svm
#                  })
# -

#
# # Model fitting loop

res = pd.DataFrame()

for ke in params:
    p = params[ke]
    modelname = ke
    target = p['target']
    grid = p['grid']
    if p['features'] is not None:
        featuresResponse = p['features'] + [target]
    else:
        featuresResponse = [target]
    model = sklearn.base.clone(p['model']) # Create a new model from base
    

    X_tr, y_tr = ptn.transform(df_train[featuresResponse])
    X_te, y_te = ptn.transform(df_test[featuresResponse])

    n_jobs = -1

    if (grid is not None):
        gr =  GridSearchCV( model, param_grid=grid, n_jobs = n_jobs, cv=cv)
        gr.fit(X_tr, y_tr, groups=group_train)
        mean_time = np.mean(gr.cv_results_['mean_fit_time'])
        start_time = time.time()
        e = predict_results(gr, X_tr, y_tr, X_te, y_te, group_train, group_test, modelname)
        predict_time = time.time() - start_time
        model = gr
    else:
        start_time = time.time()
        model.fit(X_tr, y_tr)
        mean_time = time.time() - start_time
        start_time = time.time()
        e = predict_results(model, X_tr, y_tr, X_te, y_te, group_train, group_test, modelname)
        predict_time = time.time() - start_time
        
    
    
    print(
        """{}: 
        - Mean fit time: {}
        - Predict time: {}
        - Train Median MAE: {}
        - Test Median MAE: {}
        - Train Mean MAE: {}
        - Test Mean MAE: {}
        """.format(ke, mean_time, predict_time,
                   e['TrainMedianMAE'],e['TestMedianMAE'],
                   e['TrainMeanMAE'],e['TestMeanMAE']))
    
    pres = pd.DataFrame({**e, 'mean_time':mean_time, 'predict_time':predict_time})
    res = res.append(pres)
    
    # Save Model
    joblib.dump(model, output_folder+'/'+modelname.replace(" ", "_")+".sav")
    
    # Save metrics - If file doesn't exist, put header.
    pres.to_csv(results_file, index=False, mode="a", 
           header=(not os.path.isfile(results_file)))


res

# # Compare best models results

m = joblib.load('models/RF_History.sav')
X_tr, y_tr = ptn.transform(df_train[feat_history+[target]])
X_te, y_te = ptn.transform(df_test[feat_history+[target]])
pred_tr = m.predict(X_tr)
pred_te = m.predict(X_te)
getErrorMeasures(np.exp(y_te), np.exp(pred_te), group=df_test['imo'], agg_funct='median')

m = joblib.load('models/RF_Activations.sav')
X_tr, y_tr = ptn.transform(df_train[feat_activation+[target]])
X_te, y_te = ptn.transform(df_test[feat_activation+[target]])
pred_tr = m.predict(X_tr)
pred_te = m.predict(X_te)
getErrorMeasures(np.exp(y_te), np.exp(pred_te), group=df_test['imo'], agg_funct='median')
