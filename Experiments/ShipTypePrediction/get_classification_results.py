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

import sklearn
from sklearn import *
import numpy as np
import pandas as pd
from sklearn.preprocessing import LabelEncoder
import json
import pdb
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import confusion_matrix
import os

os.makedirs("resultsfinal")
os.makedirs("resultsfinal/predictions")
###### Load the data #########
train = pd.read_csv("data/train_AIS.csv");
test = pd.read_csv("data/test_AIS.csv");
train_crbm = pd.read_csv("data/train_crbm_AIS.csv");
test_crbm = pd.read_csv("data/test_crbm_AIS.csv");

######### encoding class names as integers  from 0 to C-1 for C classes  #########
encoder = LabelEncoder()
encoder.fit(train["type"])
train["type"] = encoder.transform(train["type"])
test["type"] = encoder.transform(test["type"])
train_crbm["type"] = encoder.transform(train_crbm["type"])
test_crbm["type"] = encoder.transform(test_crbm["type"]);
pd.DataFrame(encoder.classes_).to_csv("./resultsfinal/encoder_classes.json")

###### Define target name #########
target = ["type"]

###### Define feature names #########
features = ["rotationGPS20", "bathymetry320", "sog20"]

features_hist = ["rotationGPS" + str(i) for i in range(1,21)] + \
                ["sog" + str(i) for i in range(1,21)] + \
                ["bathymetry3" + str(i) for i in range(1,21)] 

features_crbm = ['activations.' + str(i) for i in range(1,11)]

###### Define train and test sets for the 3 experiments ######
X_tr = np.array(train[features])
y_tr = np.array(train[target[0]])
X_tr = (X_tr - np.mean(X_tr, 0)) / np.std(X_tr, 0)
X_te = np.array(test[features])
y_te = np.array(test[target[0]])
X_te = (X_te - np.mean(X_te, 0)) / np.std(X_te, 0)

X_tr_hist = np.array(train[features_hist])
y_tr = np.array(train[target[0]])
X_tr_hist = (X_tr_hist - np.mean(X_tr_hist, 0)) / np.std(X_tr_hist, 0)
X_te_hist = np.array(test[features_hist])
y_te  = np.array(test[target[0]])
X_te_hist = (X_te_hist - np.mean(X_te_hist, 0)) / np.std(X_te_hist, 0)

X_tr_crbm = np.array(train_crbm[features_crbm])
y_tr_crbm = np.array(train_crbm[target[0]])
#X_tr_crbm = (X_tr_crbm - np.mean(X_tr_crbm, 0)) / np.std(X_tr_crbm, 0)
X_te_crbm = np.array(test_crbm[features_crbm])
y_te_crbm  = np.array(test_crbm[target[0]])
#X_te_crbm = (X_te_crbm - np.mean(X_te_crbm, 0)) / np.std(X_te_crbm, 0)

results_acc = {"original_features":{"train":{}, "test":{}},
               "hist_features":{"train":{}, "test":{}},
               "crbm_features":{"train":{}, "test":{}}}

results_auc = {"original_features":{"train":{}, "test":{}},
               "hist_features":{"train":{}, "test":{}},
               "crbm_features":{"train":{}, "test":{}}}

###### Metrics used #########
def accuracy(y, y_hat):
    return  sum(y == y_hat)/float(len(y))

###### Define models to experiment with #########
models = [("LogisticRegression", sklearn.linear_model.LogisticRegression()),
          ("MLPClassifier", sklearn.neural_network.MLPClassifier()),
          ("KNeighborsClassifier",sklearn.neighbors.KNeighborsClassifier())]

###### Define the three data parts used #########
data_splits = (("original_features", X_tr, y_tr, X_te, y_te),
               ("hist_features", X_tr_hist, y_tr, X_te_hist, y_te),
               ("crbm_features", X_tr_crbm,y_tr_crbm, X_te_crbm, y_te_crbm))

###### We can add a grid and make model=grid to do grid search #########
grid_params = {"LogisticRegression":{ "C":[0.8, 0.9, 1, 1.1, 1.2] },
                "MLPClassifier":{"hidden_layer_sizes":[(100,), (200,), (300,), (400,), (500,)]},
                "KNeighborsClassifier":{"n_neighbors":[5, 10, 15]} }

###### Train the different models #########
for modelname,current_model in models:
    g_params = grid_params[modelname]
    print("\n\n\tMODEL:", modelname)
    
    for dataname, Xtr, ytr, Xte, yte in data_splits:
        print("\n\t\tWorking with data: ", dataname, " shape of Xtr", Xtr.shape)
        model = sklearn.model_selection.GridSearchCV(current_model, g_params, n_jobs=-1)
        model.fit(Xtr, ytr)
        
        results_acc[dataname]["train"][modelname] = accuracy(model.predict(Xtr), ytr)
        results_acc[dataname]["test"][modelname] = accuracy(model.predict(Xte), yte)
        cv_results = pd.DataFrame(model.cv_results_)
        cv_results.to_json('./resultsfinal/resultsfinal_cv_' + modelname + "_" + dataname +'.json')
        print("\t\tBest model of the grid selected. Results in train and test saved")

        conf_mat_tr = confusion_matrix(ytr, model.predict(Xtr))
        conf_mat_te = confusion_matrix(yte, model.predict(Xte))

        pd.DataFrame(conf_mat_tr).to_csv('./resultsfinal/predictions/conf_mat_tr_' + modelname + "_" + dataname +'.json')
        pd.DataFrame(conf_mat_te).to_csv('./resultsfinal/predictions/conf_mat_te_' + modelname + "_" + dataname +'.json')
        print("\t\tConfusion Matrix saved")

        pd.DataFrame(model.predict(Xtr)).to_csv('./resultsfinal/predictions/y_tr_hat_' + modelname + "_" + dataname +'.json')
        pd.DataFrame(model.predict(Xte)).to_csv('./resultsfinal/predictions/y_te_hat_' + modelname + "_" + dataname +'.json')
        print("\t\tModel predictions saved")
        
        auc_tr = sklearn.metrics.f1_score(ytr, model.predict(Xtr), average="weighted")
        auc_te = sklearn.metrics.f1_score(yte, model.predict(Xte), average="weighted")
        pd.DataFrame({"train": [auc_tr], "test": [auc_te]}).to_csv('./resultsfinal/predictions/f1_weighted' + modelname + "_" + dataname +'.json')
        print("\t\tData saved for: ", dataname)
        del(model)


###### Train the different models #########
final_results = pd.DataFrame(results_acc)
final_results.to_json("./resultsfinal/all_results.json")
