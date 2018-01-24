import pandas as pd
import warnings
warnings.filterwarnings("ignore")

def back_test():
    df = pd.read_csv('final.csv')

    df.drop(labels=['Unnamed: 0'], axis=1, inplace=True)
    df.rename(columns={'col1': 'trade_sig_svm', 'col2': 'trade_sig_lr', 'col3': 'trade_sig_qda',
                       'col4': 'close', 'Delt.1.arithmetic': 'daily_rets'}, inplace=True)
    df['ports_rets_svm'] = df['daily_rets'] * df['trade_sig_svm'].shift(1)
    df['cum_rets_svm'] = df['ports_rets_svm'].cumsum()
    df['ports_rets_lr'] = df['daily_rets'] * df['trade_sig_lr'].shift(1)
    df['cum_rets_lr'] = df['ports_rets_lr'].cumsum()
    df['ports_rets_qda'] = df['daily_rets'] * df['trade_sig_qda'].shift(1)
    df['cum_rets_qda'] = df['ports_rets_qda'].cumsum()
    print("\nBacktest $GOOGL Long-Short SVM Portfolio:\n")
    print("Backtest period from 2015-07-08 to 2018-01-16")
    print("SVM Returns: {}".format(
        str(df['cum_rets_svm'][636]*100)[0:5]) + "%"
    )
    print("Log Reg Returns: {}".format(
        str(df['cum_rets_lr'][636] * 100)[0:5]) + "%"
          )
    print("QDA Returns: {}".format(
        str(df['cum_rets_qda'][636] * 100)[0:5]) + "%"
          )
    try:
        final_preds = pd.DataFrame(df['daily_rets'].cumsum())
        final_preds['SVM'] = df['cum_rets_svm']
        final_preds['QDA'] = df['cum_rets_qda']
        final_preds['LogReg'] = df['cum_rets_lr']
        final_preds.to_csv('Returns.csv')
        print(final_preds.head(10))
        print("'Returns.csv' successfully saved!")
    except:
        print("Unable to save csv.")

back_test()
