import pandas as pd
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings("ignore")

def back_test():
    df = pd.read_csv('final.csv')
    df.drop(labels=['Unnamed: 0'], axis=1, inplace=True)
    df.rename(columns={'col1': 'trade_sig', 'col2': 'close', 'Delt.1.arithmetic': 'daily_rets'}, inplace=True)
    df['ports_rets'] = df['daily_rets'] * df['trade_sig'].shift(1)
    df['cum_rets'] = df['ports_rets'].cumsum()
    plt.plot(df['cum_rets'])
    plt.title("$GOOGL SVM Portfolio")
    plt.show()
    print(df.head(20))
    print("\nBacktest $GOOGL Buy-Only SVM Portfolio:\n")
    print("Backtest period from 2015-05-27 to 2018-01-18")
    print("Returns: {}".format(
        str(df['cum_rets'][667]*100)[0:5]) + "%"
    )
    try:
        pd.DataFrame(df['cum_rets']).join(df['daily_rets']).to_csv('Returns.csv')
        print("'Returns.csv' successfully saved!")
    except:
        print("Unable to save csv.")

back_test()