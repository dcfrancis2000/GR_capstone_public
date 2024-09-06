import sys
import time
import numpy as np
import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.multioutput import MultiOutputRegressor
from sklearn.linear_model import LinearRegression, MultiTaskElasticNetCV
from sklearn.model_selection import cross_validate, LeaveOneOut
from sklearn.metrics import mean_squared_error, make_scorer

time_total_start = time.time()
pd.set_option('expand_frame_repr', False)

loo = LeaveOneOut()
data_raw = pd.read_csv('C:/Users/dcfra/GR_capstone_public/data/full_data.csv') 
instance = pd.read_csv("C:/Users/dcfra/GR_capstone_public/results/instance.csv")

names = list(data_raw['ATHLETE'].unique())
y_vars = ['AAL', 'SPEEDMAX', 'JUMPS', 'PHYSIOLOAD', 'EXERTIONS', 'DISTANCE']

def response_cv(cv):
    rmse_values = [-np.mean(cv[f'test_{var_i}']) for var_i in y_vars]
    rmse_aggregate = np.sqrt(np.sum(np.array([cv[f'test_{var_i}']**2 for var_i in y_vars])) / len(y_vars))
    rmse_values.append(rmse_aggregate)
    return rmse_values

def rmse_score(y_true, y_pred, output_index):
    return np.sqrt(mean_squared_error(y_true[:, output_index], y_pred[:, output_index]))

scorers = {
    f'{y_vars[i]}': make_scorer(rmse_score, output_index=i, greater_is_better=False)
    for i in range(len(y_vars))
}

# Initialize lists to collect data for all players
results_rmse = []
results_coefs = []
results_alpha = []
results_ratio = []
results_n = []

for idx, name in enumerate(names):

    row_cutter = pd.Index(data_raw["ATHLETE"] == name)
    # first 10 columns are player/game info and responses. Selects "reliable" metrics for
    # respective player using instance matrix from R script
    column_cutter = pd.Index([True for _ in range(9)] + list(instance.iloc[:, idx] == 1))
    data = data_raw.iloc[row_cutter, column_cutter]

    n = data.shape[0]
    Y = np.array(data[y_vars]) 
    X = data.drop(pd.Index(y_vars + ['ATHLETE', 'DATE']), axis=1) 
    X_one = np.ones((n, 1))
    x_vars = list(X.columns)
        
    model_reg = MultiOutputRegressor(LinearRegression())
    ratios = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0]
    model_elastic = MultiTaskElasticNetCV(
         alphas = np.logspace(-4,4,50),
         cv = loo,
         max_iter = 100000,
         tol = 1e-3,
         l1_ratio = ratios,
         n_jobs = -1, 
         selection = 'random',
         random_state = 0)
    
    cv_reg = cross_validate(model_reg, X_one, Y,
                            cv=loo, scoring=scorers, 
                            return_train_score=False, verbose=0, n_jobs=-1)
    
    cv_elastic = cross_validate(model_elastic, X, Y,
                                cv=loo, scoring=scorers, 
                                return_train_score=False, verbose=0, n_jobs=-1) 
    
    fit_model = model_elastic.fit(X,Y)
    coefs = pd.DataFrame(fit_model.coef_)
    coefs.index = y_vars
    coefs.columns = x_vars
    coefs = coefs.reset_index().melt(id_vars='index', var_name='Feature', value_name='Coefficient')
    coefs['Player'] = name
    
    results_coefs.append(coefs)
    results_alpha.append(fit_model.alpha_)
    results_n.append(n)
    results_ratio.append(fit_model.l1_ratio_)

    rmse_array = np.c_[response_cv(cv_reg),
                       response_cv(cv_elastic)]
    
    rmse_frame = pd.DataFrame(rmse_array,index = y_vars + ['Aggregate'],
                            columns = ["Intercept", "Elastic"]) 

    rmse_frame = rmse_frame.reset_index().melt(id_vars='index', var_name='Model', value_name='RMSE')
    rmse_frame['Player'] = name
    results_rmse.append(rmse_frame)  

rmse_df = pd.concat(results_rmse)
coefs_df = pd.concat(results_coefs)

rmse_df.to_csv('C:/Users/dcfra/GR_capstone_public/results/rmse_results.csv', index=False)
coefs_df.to_csv('C:/Users/dcfra/GR_capstone_public/results/coefs_results.csv', index=False)
pd.DataFrame([results_alpha,
              results_n,
              results_ratio]).to_csv("C:/Users/dcfra/GR_capstone_public/results/model_results.csv",index=False)
