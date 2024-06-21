###########################################################################################

# Machine Learning Script. For each player, run a set of regression and ridge models
# and store the results. Feature selection is automated currently, subject to change later.
# Features are initially chosen based on the instance reliability matrix created in 
# the R processing script. 

###########################################################################################

import sys
import time
import numpy as np
import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.multioutput import MultiOutputRegressor
from sklearn.linear_model import LinearRegression, RidgeCV
from sklearn.model_selection import cross_validate, LeaveOneOut
from sklearn.feature_selection import SelectFromModel
from sklearn.metrics import root_mean_squared_error, make_scorer

# This script takes a while to run (about 8 minutes, about 90 seconds per player)
time_total_start = time.time()
pd.set_option('expand_frame_repr', False)

# Leave-one-out cross-validated root mean square error is main performance metric
loo = LeaveOneOut()
data_raw = pd.read_csv('C:/Users/dcfra/GR_capstone_public/data/full_data.csv').groupby(['ATHLETE','DATE','HALF']).mean().reset_index() # average over jumps (no duplicates)
instance = pd.read_csv("C:/Users/dcfra/GR_capstone_public/plots_summaries/instance.csv")
scale = lambda x: StandardScaler().fit_transform(x)
names = list(data_raw['ATHLETE'].unique())
y_vars = ['AAL','SPEEDMAX','JUMPS','PHYSIOLOAD','EXERTIONS','DISTANCE']

# function to process CV output. gives average RMSE value out of all folds
def response_cv(cv,var):
        if type(var) == str:
            return -np.mean(cv[f'test_{var}'])
        else:
            return [-np.mean(cv[f'test_{var_i}']) for var_i in var]

# helper function for cross_validate       
def rmse_score(y_true, y_pred,output_index):
    return root_mean_squared_error(y_true[:, output_index], y_pred[:, output_index])

# main performance scoring metric. Applied to all six responses
scorers = {
    f'{y_vars[i]}': make_scorer(rmse_score, output_index=i, greater_is_better=False)
    for i in range(len(y_vars))
}

# Learning process identical across players
for name in enumerate(names):
    time_athlete_start = time.time()

    print(f'\nAthlete: {name[1]}')
    print('Processing Data...')

    row_cutter = pd.Index(data_raw["ATHLETE"] == name[1])
    # first 10 columns are player/game info and responses. Selects "reliable" metrics for
    # respective player using instance matrix from R script
    column_cutter = pd.Index([True for _ in range(10)] + list(instance.iloc[:,name[0]] == 1))
    data = data_raw.iloc[row_cutter,column_cutter]
    n = data.shape[0]

    Y = np.array(scale(data[y_vars])) 
    indicators = pd.get_dummies(data[['HALF']], dtype=float) #  Date not included
    features = data.drop(pd.Index(y_vars + ['ATHLETE','DATE','HALF']), axis=1) 
    x_vars = list(indicators.columns) + list(features.columns)

    X_full = np.c_[np.array(indicators), scale(features)] # all available features
    X_one = np.ones((n,1)) # intercept only
    X_red = np.c_[np.array(indicators), scale(np.array(data.TIME).reshape(-1,1))] # kinexon features only (time and half indicator)

    print('Running regression models...')
    # intercept-only regression model is the main baseline
    cv_one = cross_validate(MultiOutputRegressor(LinearRegression()), 
                            X_one,Y,cv = loo,scoring = scorers, 
                            return_train_score = False, verbose = 0, n_jobs = -1)
    # if this is better than the full model, then Vald metrics are useless?
    cv_redreg = cross_validate(MultiOutputRegressor(LinearRegression()), 
                            X_red,Y, cv=loo, scoring=scorers,
                            return_train_score=False, verbose = 0, n_jobs = -1)
    cv_fullreg = cross_validate(MultiOutputRegressor(LinearRegression()), 
                                X_full, Y, cv=loo, scoring=scorers, return_train_score=False, verbose = 0, n_jobs = -1)

    print('Getting selected ridge features...')
    # automated hyperparameter tuning for Ridge based on LOOCV RMSE
    model = RidgeCV(alphas = np.logspace(-4,4,50),
                    scoring = 'neg_root_mean_squared_error',
                    cv = loo)
    rfe_model = SelectFromModel(model,prefit=False) # need to look more into this. More black box than I'd like
    selector = rfe_model.fit(X_full, Y)
    X_cut = selector.transform(X_full)
    x_cutnames = list(selector.get_feature_names_out(input_features=x_vars))

    # Most of the processing time lives here    
    print('Cross-validating ridge models...')
    cv_red = cross_validate(model, X_red,Y, cv=loo, scoring=scorers, 
                            return_train_score=False,verbose = 0,n_jobs = -1)
    cv_full = cross_validate(model, X_full,Y, cv=loo, scoring=scorers, 
                            return_train_score=False,verbose = 0,n_jobs = -1)
    cv_cut = cross_validate(model, X_cut,Y, cv=loo, scoring=scorers, 
                            return_train_score=False,verbose = 0,n_jobs = -1)

    print('Collecting results...')
    fit_model = model.fit(X_cut,Y)
    coefs = pd.DataFrame(fit_model.coef_)
    coefs.index = y_vars
    coefs.columns = x_cutnames
    Y_sds = np.std(np.array(data[y_vars]),axis=0).reshape(-1,1)

    rmse_array = np.c_[response_cv(cv_one,y_vars),
                    response_cv(cv_redreg,y_vars),
                    response_cv(cv_fullreg,y_vars),
                    response_cv(cv_red,y_vars),
                    response_cv(cv_full,y_vars),
                    response_cv(cv_cut,y_vars)
                    ]

    rmse_frame = pd.DataFrame(rmse_array,index = y_vars,
                            columns = ["Intercept Only Regression",
                                        "Kinexon Only Regression",
                                        "Full Model Regression",
                                        "Kinexon Only Ridge",
                                        "Full Model Ridge",
                                        "Reduced Model Ridge" ])

    output_string = '\n\n'.join([
        f'Name: {name[1]}',
        f'n: {Y.shape[0]}',
        f'p Full: {len(x_vars)}, p Reduced: {len(x_cutnames)}',
        f'Ridge Alpha: \n{fit_model.alpha_}',
        f'Reliable Features: \n{x_vars}',
        f'Selected Features: \n{x_cutnames}',
        f'RMSE Values (Average LOOCV values): \n{rmse_frame.T}',
        f'Coefficients * SD: \n{(coefs * Y_sds).T}',
        "###############################################################################################\n\n"
    ])

    with open('C:/Users/dcfra/GR_capstone_public/plots_summaries/machine_learning_output.txt','a') as writefile:
            writefile.write(output_string)
    
    time_athlete_end = time.time()
    print(f'{name[1]} output written. Time elapsed: {round((time_athlete_end - time_athlete_start) / 60,2)} minutes')

time_total_end = time.time()
print(f'\nScript complete. Total time elapsed: {round((time_total_end - time_total_start) / 60,2)} minutes')
