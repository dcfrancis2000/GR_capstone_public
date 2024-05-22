import requests
import pandas as pd
from datetime import datetime, timedelta
import json
import sys
import regex as re
sys.path.insert(1,'C:/Users/dcfra/sports/train_trax') # link to TT repo
from api_kinexon import KinexonAPI

kin = KinexonAPI()
bb_id = kin.get_all_current_athletes().iloc[[0,1,6,7,9]]

base_url = kin.base_url
team_id = kin.team_id
api_key = kin.api_key  # Removed the comma here
mindate = datetime(2023,11,1).strftime('%Y-%m-%d %H:%M:%S') # '2023-11-01 00:00:00'
maxdate = datetime(2024,4,1).strftime('%Y-%m-%d %H:%M:%S') # '2024-04-01 00:00:00'
url = f'{base_url}/teams/{team_id}/sessions-and-phases'
params = {'min': mindate, 'max': maxdate, 'apiKey': api_key}
auth = (kin.username, kin.password)
response = requests.get(url, params=params, auth=auth)

api = response.json()
apidata = pd.json_normalize(api)

condition1 = list(apidata['description'].str.startswith('@'))
condition2 = apidata['type'] == 'Game'

nested_data = apidata[pd.Index([not x for x in condition1]) & pd.Index(condition2)]
nested_data = nested_data.iloc[pd.Index([a not in ["BYU", "UTEP", "Texas", "Furman", "C of C " , "Saint Louis"] for a in list(nested_data['description'])])]

gamedata = []

r = re.compile('Segment')
for player_id in list(bb_id['id']):    
    player = bb_id[bb_id['id'] == player_id]
    player_name = ' '.join([list(player['first_name'])[0],list(player['last_name'])[0]])
    print(f'Getting data for {player_name} ({player_id})')
    sessions = list(nested_data['session_id'])
    for i in range(0,len(sessions)):
        sess = sessions[i]
        if sess == 30:
            continue
        else:
            phases = pd.DataFrame(list(nested_data['phases'])[i])
            phase_list = list(phases['type'])
            print(f'Session {sessions[i]}, length {len(phase_list)}:')
            if(len(phase_list) > 1):
                segment_index = [i for i, item in enumerate(phase_list) if re.search(r,item)]
                segs = phases.iloc[segment_index]

            for j in range(0,segs.shape[0]):

                seg = list(segs['id'])[j]
                print(f'Phase {seg}:')

                url = f"""{base_url}/statistics/player/{player_id}/phase/{seg}"""

                params = {'apiKey': api_key}
                segreq = requests.get(url,params=params,auth=auth)
                segdata = pd.DataFrame(segreq.json(), index = [0])
                if segdata.shape[1] > 0:
                    row_dict = {
                        'playerid': player_id,
                        'playername': player_name,
                        'playerpos': list(player['function'])[0],
                        'dtstart': list(phases[phases['id'] == seg]['start_phase'])[0],
                        'dtstop': list(phases[phases['id'] == seg]['end_phase'])[0],
                        'sessionid': sess,
                        'sessiondesc': list(nested_data['description'][nested_data['session_id'] == sess])[0],
                        'phaseid': seg,
                        'phasename': list(segs['type'][segs['id'] == seg])[0],
                        'aal': segdata['accel_load_accum'].iloc[0],
                        'speedmax': segdata['speed_max'].iloc[0],
                        'jumps': segdata['event_count_jump'].iloc[0],
                        'physioload': segdata['physio_load'].iloc[0],
                        'exertions': segdata['event_count_exertion_category1'].iloc[0] + segdata['event_count_exertion_category2'].iloc[0],
                        'distancetotal' : segdata['distance_total'].iloc[0]
                    }
                    gamedata.append(row_dict)
                else:
                    print(f'data not found')
fulldata = pd.DataFrame(gamedata)

fulldata.to_csv(r'C:\Users\dcfra\GR_capstone\data\kinexon_scraped_5-2.csv',index = False)

print('\nDataset scraped with zero errors. Good job')
print(f"""
      Shape: {fulldata.shape} \n
      Unique Players: {len(fulldata['playerid'].unique())} \n
      Unique Sessions: {len(fulldata['sessionid'].unique())} \n
      Unique Phases: {len(fulldata['phaseid'].unique())} \n
""")