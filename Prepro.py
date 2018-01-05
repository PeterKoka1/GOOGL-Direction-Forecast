import pandas as pd
import numpy as np
from alpha_vantage.timeseries import TimeSeries

def alpha_vantage(api_key):
    ts = TimeSeries(key=api_key,
                    output_format='pandas')
    googl, meta_data = ts.get_daily(symbol='GOOGL',
                                     outputsize='full')

    drop_labs = [
        'open',
        'high',
        'low',
    ]
    googl.drop(drop_labs, axis=1, inplace=True)
    googl.to_csv('GOOGL.csv')

def preds():
    df = pd.read_csv('GOOGL.csv', index_col='Date')
    preds = pd.DataFrame()
    for i in np.arange(start=1,stop=6):

        if preds.empty:
            preds = pd.DataFrame(data=df['close'].pct_change(1))
            preds.columns = ['today']
        else:
            lags = pd.DataFrame(df['close'].pct_change(i))
            lags.columns = ['lag{}'.format(i)]
            preds = preds.join(lags)

    preds = preds.join(df['volume'])
    preds = preds.join(df['close'])
    preds.dropna(axis=0,inplace=True)
    dirtest = []
    for i,j in enumerate(preds['close']):
        if i-1 >= 0:
            if preds['close'][i] > preds['close'][i-1]:
                dirtest.append(1)
            elif preds['close'][i] < preds['close'][i-1]:
                dirtest.append(0)
            elif preds['close'][i] == preds['close'][i-1]:
                dirtest.append(2)
    trial = pd.DataFrame(dirtest, columns=['dir'])
    preds.drop(preds.index[0], inplace=True)
    preds = preds.join(trial.set_index(preds.index))

    preds.to_csv('Direction_Preds.csv')
