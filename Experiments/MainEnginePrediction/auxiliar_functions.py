# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.3'
#       jupytext_version: 0.8.5
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
#   language_info:
#     codemirror_mode:
#       name: ipython
#       version: 3
#     file_extension: .py
#     mimetype: text/x-python
#     name: python
#     nbconvert_exporter: python
#     pygments_lexer: ipython3
#     version: 3.6.7
# ---
import numpy as np
import pandas as pd

from sklearn import metrics 

from math import sqrt

# Compare prediction and baseline with respect to the real value
def comparePredBase(real, pred, base):
    predDiff = real-pred
    baseDiff = real-base
    
    comp = (abs(predDiff) < abs(baseDiff))
    betterThanBase = sum(comp)/len(comp)
    return(betterThanBase)


# Return the different error measures
def getErrorMeasures(real, pred, prefix='', group=None, agg_funct='mean',
        individual_measures = False):
    if group is not None:
        df = pd.DataFrame({'real': real, 'pred': pred, 'group':group})
        group = df.groupby(group).agg(agg_funct)
        pred = group.pred.values
        real = group.real.values
    errors = dict()
    errors[prefix+'MAE'] = [metrics.mean_absolute_error(real, pred)]
    MSE = metrics.mean_squared_error(real, pred)
    errors[prefix+'RMSE'] = [sqrt(MSE)]

    if (individual_measures):
        # Individual measures
        pairs = [ metrics.mean_absolute_error([r],[p]) for r,p in zip(real,pred)]
        errors[prefix+'max_ind'] = np.max(pairs)
        errors[prefix+'mean_ind'] = np.mean(pairs)
        errors[prefix+'std_ind'] = np.std(pairs)
    return(errors)

def fitModel(model, X_tr, y_tr, groupTrain, useGroupFit=True): 
    if useGroupFit:
        model.fit(X_tr,y_tr, groups = groupTrain)
    else:
        model.fit(X_tr,y_tr)

    return model, errors


# Warning: This function always treats prediction as numeric!
def predict_results(model, X_tr, y_tr, X_te, y_te, groupTrain, groupTest,
        modelName, resultLog=True): 

    y_tr_pred = model.predict(X_tr)
    y_te_pred = model.predict(X_te)

    if (type(y_tr_pred[0]) is np.str_):
        y_tr = y_tr.astype('float')
        y_te = y_te.astype('float')
        y_tr_pred = y_tr_pred.astype('float')
        y_te_pred = y_te_pred.astype('float')

    if(resultLog):
        y_tr_pred = np.exp(y_tr_pred)
        y_te_pred = np.exp(y_te_pred)
        y_tr = np.exp(y_tr)
        y_te = np.exp(y_te)

    etrmed = getErrorMeasures(y_tr, y_tr_pred, group=groupTrain,
            prefix='TrainMedian', agg_funct='median')
    etemed = getErrorMeasures(y_te, y_te_pred, group=groupTest, prefix='TestMedian',
            agg_funct='median')

    etr = getErrorMeasures(y_tr, y_tr_pred, group=groupTrain, prefix='TrainMean')
    ete = getErrorMeasures(y_te, y_te_pred, group=groupTest, prefix='TestMean')
    errors = {'model':modelName, **etrmed, **etemed, **etr, **ete}

    return errors

