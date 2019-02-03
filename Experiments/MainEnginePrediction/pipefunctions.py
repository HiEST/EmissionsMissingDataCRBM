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

import sklearn
import numpy as np
import pandas as pd

# # Pipeline classes definition

# ## Droper class

class Droper(sklearn.base.BaseEstimator, sklearn.base.TransformerMixin):
    def fit(self, X, y):
        return(self)

    def transform(self, X, y=None):
        return(X.dropna())

# ## Logaritmizer & Exponentizator

class Logaritmizer(sklearn.base.BaseEstimator, sklearn.base.TransformerMixin):
    def __init__(self, inputColumn, outputColumn=None):
        self.inputColumn = inputColumn
        self.outputColumn = outputColumn

    def fit(self, X, y):
        return(self)

    def transform(self, X, y=None):
        lg = np.log(X[self.inputColumn])
        if self.outputColumn is None:
            X[self.inputColumn] = lg
        else:
            X = X.assign(___Log___ = lg)
            X = X.rename(columns={'___Log___': self.outputColumn})
        return(X)

class Exponentizator(sklearn.base.BaseEstimator, sklearn.base.TransformerMixin):
    def __init__(self, inputColumn, outputColumn=None):
        self.inputColumn = inputColumn
        self.outputColumn = outputColumn

    def fit(self, X, y):
        return(self)

    def transform(self, X, y=None):
        ex = np.exp(X[self.inputColumn])
        if self.outputColumn is None:
            X[self.inputColumn] = ex
        else:
            X = X.assign(___Exp___ = ex)
            X = X.rename(columns={'___Exp___': self.outputColumn})
        return(X)


# ## Shuffler

# We use the shuffle's random_state to fix a seed to get reproducible results

class Shuffler(sklearn.base.BaseEstimator, sklearn.base.TransformerMixin):
    def __init__(self, seed):
        self.seed = seed
   
    def fit(self, X, y):
        return(self)

    def transform(self, X, y=None):
        if y is None:
            X = sklearn.utils.shuffle(X, random_state=self.seed)
            return(X)
        else:
            X, y = sklearn.utils.shuffle(X, y, random_state=self.seed)
            return(X, y)

# ## Dummizer

class Dummizer(sklearn.base.TransformerMixin):
    def __init__(self, inputColumns, outputPrefix):
        self.columns = inputColumns
        self.prefix = outputPrefix

    def fit(self, X, y):
        return self

    def transform(self, X, y=None):
        # Transform to string
        X=pd.get_dummies(X, columns=self.columns, prefix=self.prefix)
        return X


# ## StringCaster

class StringCaster(sklearn.base.TransformerMixin):
    def __init__(self, column):
        self.column = column

    def fit(self, X, y):
        return self

    def transform(self, X, y=None):
        X[self.column] = X[self.column].astype(str)
        return X

# ## Binner

class Binner(sklearn.base.TransformerMixin):
    def __init__(self, bins, inputColumn, outputColumn=None):
        self.inputColumn = inputColumn
        self.outputColumn = outputColumn
        self.bins = bins

    def fit(self, X, y):
        return(self)

    def transform(self, X, y=None):
        bin = pd.cut(X[self.inputColumn], bins=self.bins)
        if self.outputColumn is None:
            X[self.inputColumn] = bin
        else:
            X = X.assign(___Bin___ = bin)
            X = X.rename(columns={'___Bin___': self.outputColumn})
        return(X)

class BinnerMid(sklearn.base.TransformerMixin):
    def __init__(self, inputColumn, outputColumn=None):
        self.inputColumn = inputColumn
        self.outputColumn = outputColumn

    def fit(self, X, y):
        return(self)

    def transform(self, X, y=None):
        bin = [i.mid for i in X[self.inputColumn]] 
        if self.outputColumn is None:
            X[self.inputColumn] = bin
        else:
            X = X.assign(___Bin___ = bin)
            X = X.rename(columns={'___Bin___': self.outputColumn})
        return(X)



# ## Response splitter: PandasToNumpyXY

class PandasToNumpyXY(sklearn.base.BaseEstimator, sklearn.base.TransformerMixin):
    def __init__(self, response):
        self.response = response
   
    def fit(self, X, y):
        return(self)

    def transform(self, X, y=None):
        y = X[self.response]
        Xc = X.copy()
        del Xc[self.response] # This has effect over the global X! We need a copy
        X = Xc.values
        y = y.values
        
        return(X,y)        


# # Regressors

# ## Meanizer - Model that always predict the average

class Meanizer(sklearn.base.BaseEstimator, sklearn.base.RegressorMixin):

    def fit(self, X, y):
        if X.shape[1] == 0: # Normal mean
            # Remove duplicates to do the mean?
            self.mean = y.mean()
        else:
            keys = np.apply_along_axis(str, 1, X) # Transform data to keys

            df = pd.DataFrame({'key': keys, 'value': y})
            df = df.groupby('key').agg('mean')

            self.mean =  dict(zip(df.index.values, df['value']))

        return self

    def predict(self, X, y=None):
        if isinstance(self.mean, dict): # Grouped average
            keys = np.apply_along_axis(str, 1, X) # Transform data to keys
            y = [self.mean[k] for k in keys]
        else:
            y = np.repeat(self.mean, X.shape[0]) # Repeat mean ncol times
        return(y)


