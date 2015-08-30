# Step 1 : import data from FRED databases

from fredapi import Fred
fred = Fred(api_key = 'c3bc005d1a70992b2b3f3ef97d896c32')

query = fred.search('france')
query = query[(query['frequency_short'] == 'Q')]
query = query[(query['observation_start'] <= '1970-01-01, 00:00:00') & (query['observation_end'] > '2013-01-01, 00:00:00')]

import pandas as pd 
series = list()
for series_id in list(query['id']):
	new_serie = fred.get_series(series_id, observation_start = '1970-01-01, 00:00:00')
	series.append(new_serie)
	print(series_id + ': done')

df = pd.concat(series, axis = 1)
df.columns = list(query['id'])
print(df.head())
print(query['title'])

df.to_csv('/Users/nicolassaleille/Dropbox/ofpr/data/fred/france_query_2.csv')

# modele de prevision du PIB



