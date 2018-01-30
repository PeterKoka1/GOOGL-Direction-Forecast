import pandas as pd
import numpy as np
import requests
import bs4 as bs
import pickle
import os
from os.path import exists
from alpha_vantage.timeseries import TimeSeries
from alpha_vantage.techindicators import TechIndicators

def update_ticks():
    url = 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
    r = requests.get(url)
    soup = bs.BeautifulSoup(r.text, 'lxml')
    SP500 = soup.find('table', {'class':'wikitable sortable'})

    tickers = []
    for ticker in SP500.find_all('tr')[1:]:
        stock = ticker.find_all('td')[0].text
        tickers.append(stock)

    with open('SP500ticks.pickle', 'wb') as f:
        pickle.dump(tickers, f)

    return tickers

def alpha_vantage(api_key, request_again):
    if request_again == True:
        tickers = update_ticks()
    else:
        with open('SP500ticks.pickle','rb') as f:
            tickers = pickle.load(f)

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
    # googl.to_csv('GOOGL.csv')

    path = 'stock_dfs01_18'
    if not os.path.exists(path):
        os.makedirs(path)

    for stock in tickers:
        if not exists('{}.csv'.format(stock)):
            try:
                ts = TimeSeries(key=api_key,
                                output_format='pandas')
                ticker, meta_data = ts.get_daily(symbol=stock,
                                                 outputsize='full')
                drop_labs = [
                    'open',
                    'high',
                    'low',
                    'volume',
                ]
                ticker.drop(drop_labs, axis=1, inplace=True)
                ticker.rename(columns={'close': stock}, inplace=True)
                ticker.to_csv('{}/{}.csv'.format(path,stock))
                print("Adding {} to dir".format(stock))
            except:
                s = "{} error".format(stock)
                print(s)

def preds(api_key):
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
    preds['dir'] = (preds['close'] > preds['close'].shift(1)).shift(-1)

    filename = 'GOOGL-lags.csv'
    try:
        preds.to_csv('{}'.format(filename))
        print('GOOGL lags saved successfully as "{}"'.format(filename))
    except:
        print('Unable to save "{}"'.format(filename))

    ti = TechIndicators(key=api_key, output_format='pandas')
    sma, _ = ti.get_sma(symbol='GOOGL', interval='daily', series_type='close')
    ema, _ = ti.get_ema(symbol='GOOGL', interval='daily', series_type='close')
    wma, _ = ti.get_wma(symbol='GOOGL', interval='daily', series_type='close')
    trima, _ = ti.get_trima(symbol='GOOGL', interval='daily', series_type='close')
    kama, _ = ti.get_kama(symbol='GOOGL', interval='daily', series_type='close')
    macd, _ = ti.get_macd(symbol='GOOGL', interval='daily', series_type='close')
    rsi, _ = ti.get_rsi(symbol='GOOGL', interval='daily', series_type='close')
    adx, _ = ti.get_adx(symbol='GOOGL', interval='daily')
    apo, _ = ti.get_apo(symbol='GOOGL', interval='daily', series_type='close')
    stoch, _ = ti.get_stoch(symbol='GOOGL', interval='daily')
    ppo, _ = ti.get_ppo(symbol='GOOGL', interval='daily')
    mom, _ = ti.get_mom(symbol='GOOGL', interval='daily', series_type='close')
    cci, _ = ti.get_cci(symbol='GOOGL', interval='daily')
    aroon, _ = ti.get_aroon(symbol='GOOGL', interval='daily', series_type='close')
    mfi, _ = ti.get_mfi(symbol='GOOGL', interval='daily', series_type='close')
    dx, _ = ti.get_dx(symbol='GOOGL', interval='daily', series_type='close')
    ad, _ = ti.get_ad(symbol='GOOGL', interval='daily')
    obv, _ = ti.get_obv(symbol='GOOGL', interval='daily')

    filename2 = 'GOOGL-techs.csv'
    try:
        preds_test = pd.concat(
            [sma, ema, wma, trima, kama, macd, rsi, adx, apo, stoch, ppo, mom, cci, aroon, mfi, dx, ad, obv],
            axis=1
        )
        pd.DataFrame(preds_test).to_csv('{}'.format(filename2))
        print('Google techs saved as successfully as "{}"'.format(filename2))
    except:
        print('Unable to save "{}"'.format(filename2))

def aggregateSP500():

    with open('SP500ticks.pickle', 'rb') as f:
        tickers = pickle.load(f)

    filename = 'JoinedCloses01_18.csv'

    aggregTicks = pd.DataFrame()
    try:
        for stock in tickers:
            try:
                df = pd.read_csv('stock_dfs01_18/{}.csv'.format(stock))
                if aggregTicks.empty and '2004-08-19' in df['Date'].tolist():
                    idx = df.index[df['Date'] == '2004-08-19'].tolist()
                    dfupd = df.iloc[idx[0]:].set_index(['Date'])
                    aggregTicks = dfupd
                else:
                    if '2004-08-19' in df['Date'].tolist():
                        idx = df.index[df['Date'] == '2004-08-19'].tolist()
                        dfupd = df.iloc[idx[0]:].set_index(['Date'])
                        aggregTicks['{}'.format(stock)] = dfupd
            except FileNotFoundError:
                print("No csv for {}.csv".format(stock))
        aggregTicks.to_csv('{}'.format(filename))
        print('Aggregated CSV saved successfully as "{}"'.format(filename))
    except:
        print('Unable to save "{}"'.format(filename))

def finaldf():

    lags = pd.read_csv('GOOGL-lags.csv')
    techs = pd.read_csv('GOOGL-techs.csv').set_index(['Unnamed: 0'])
    techs.index.names = ['Date']
    techs = techs.iloc[0:3378]
    techs.reset_index(inplace=True)
    jc = pd.read_csv('JoinedCloses01_18.csv')
    googl = pd.read_csv('GOOGL.csv')

    filename = 'GOOGL-Final.csv'
    try:
        finaldf = pd.DataFrame(pd.concat([lags, techs, googl, jc], axis=1))
        ogs = finaldf.shape
        finaldf.dropna(axis=0, inplace=True)
        ups = finaldf.shape
        print('{} data points lost'.format(ogs[0]-ups[0]))
        finaldf.to_csv('{}'.format(filename))
        print('{} successfully saved!'.format(filename))
    except: 
        print('Unable to save {}'.format(filename))
