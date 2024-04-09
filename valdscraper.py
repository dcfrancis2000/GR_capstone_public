"""

Collects all test and trial data on the VALD platform for the five starter players. 
Each trial is extracted from the Vald API (online) directly from each test ID on each player.
Since requests.get() is being called 100+ times, it may take a little while to run. Diagnostics also provided in the console. 

"""

# Setup
import sys
sys.path.insert(1,'C:/Users/dcfra/sports/train_trax') # link to TT repo
from datetime import datetime, date
import pandas as pd
import requests

from api_vald import VALDAPI # Authentication and player list
vald = VALDAPI()
token = vald.get_fdapi_token()
base_url = 'https://fdapi.valdperformance.com'
team_id = "09fe9002-647a-4394-9686-bfb1454fa238" # constant across URLs
headers = {'Authorization': token}
start_date = '2023-011-07T00:00:00Z' # can be used to filter test IDs, but exceptions will be raised for a date this constrained

# NOTE: assumes base_url, team_id, headers are global
def extract_trials(test_id): # pulls trial data given test ID
    print(f'test {test_id}')
    trial_link = f'{base_url}/v2019q3/teams/{team_id}/tests/{test_id}/trials' # response is trial data
    trial_resp = requests.get(trial_link,headers=headers) 
    trial_rows = pd.json_normalize(trial_resp.json())

    for i in range(0,trial_rows.shape[0]): # for each trial in test
        trial = pd.DataFrame(trial_rows['results'][i])
        trial_n = trial.shape[0]
        trial_data = { # formatting extremely specific on this level to make my life easier later
            'test_id': [test_id for _ in range(trial_n)],
            'trial_id': [trial_rows['id'][i] for _ in range(trial_n)],
            'trial_date': [trial_rows['recordedUTC'][i] for _ in range(trial_n)],
            'metric_name': [list(trial['definition'][a].values())[3] for a in range(trial_n)],
            'metric_description': [list(trial['definition'][a].values())[2] for a in range(trial_n)],
            'value': trial['value'],
            'time': trial['time'],
            'limb': [trial_rows['limb'][i] for _ in range(trial_n)],
            'asym': [list(trial['definition'][a].values())[6] for a in range(trial_n)]
            }
        if i == 0: 
            test_data = pd.DataFrame(trial_data)
        else:
            test_data = pd.concat([test_data, pd.DataFrame(trial_data)], axis = 0, ignore_index = True)
    
    return test_data

def get_test_list(athlete_id, datefrom): # gets lists of tests for a specific player ID  
    test_link = f'{base_url}/v2019q3/teams/{team_id}/athlete/{athlete_id}/tests/1/' # there is always a first page no matter the actual number
    params = {'modifiedFrom': datefrom}
    response = requests.get(test_link, headers=headers, params = params)
    data = pd.json_normalize(response.json())
    data_items = data.explode('items')
    data_tests_df = pd.DataFrame(list(data_items['items']))
    data_tests_df = data_tests_df[data_tests_df['testType'] == 'CMJ']
    data_tests = list(data_tests_df['id'])
    npage = data['totalPages'][0] # multiple pages for the tests
    if npage > 1: # repeating above steps for each page
        for page in range(2, npage + 1):
            newlink = f'{base_url}/v2019q3/teams/{team_id}/athlete/{athlete_id}/tests/{page}'
            newresponse = requests.get(newlink, headers=headers)
            newresponse_items = pd.json_normalize(newresponse.json()).explode('items')
            newresponse_df = pd.DataFrame(list(newresponse_items['items']))
            newresponse_df = newresponse_df[newresponse_df['testType'] == 'CMJ']
            newresponse_tests = list(newresponse_df['id'])
            data_tests.extend(newresponse_tests)
    return data_tests

def get_test_data(id_list, index): # recursively binds trial data by row for each test ID in the list
    if index == 0: 
        return extract_trials(id_list[index])
    else:
        return pd.concat([extract_trials(id_list[index]), get_test_data(id_list, index - 1)],
                          ignore_index = True, axis = 0) 
    
def main():
    ath = vald.get_fdapi_athlete_list(fdapi_token = token)
    bb_df = ath.loc[ath.team_name == "Men's Basketball"]
    bb_id = list(bb_df.athlete_id.unique())
    
    # starting 5 player IDs
    bb_id_top5 = list(bb_df[bb_df["name"].isin(['Brendan Wenzel','Cam Manyawu','Sam Griffin','Akuel Kot','Mason Walters'])]['athlete_id'])

    for i in range(0,5):
        id = bb_id_top5[i]
        print(f'Scraping Player {id}...')
        temp_tests = get_test_list(id,None)
        data = get_test_data(temp_tests, len(temp_tests) - 1)
        data['athlete_id'] = id
        print(f"""Number of unique tests in player dataset: {len(data['test_id'].unique())} \n""")
        if i == 0:
            vald_full = data
        else:
            vald_full = pd.concat([vald_full, data]) # bind each player's test/trial data by row

    vald_full = vald_full.iloc[:,[9,0,1,2,3,4,5,6,7,8]] # player ID first column
    vald_full.to_csv(r"C:\Users\dcfra\GR_capstone\data\vald_scraped_4-09.csv")

    # diagnostics
    print('\nDataset scraped with zero errors. Good job')
    print(f"""
          Shape: {vald_full.shape} \n
          Unique Players: {len(vald_full['athlete_id'].unique())} \n
          Unique Tests: {len(vald_full['test_id'].unique())} \n
          Unique Trials: {len(vald_full['trial_id'].unique())} \n
    """)

if __name__ == "__main__":
    main()
    
