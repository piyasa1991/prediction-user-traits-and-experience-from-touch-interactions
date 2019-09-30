import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import warnings; warnings.filterwarnings('ignore')

pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

def readDataFromCsv(file):
    """ Read csv from files
    
    Parameters
    ----------
    file: str
        Filename to be read
    
    Returns
    ----------
    pandas.DataFrame
        Returns the dataframe containing the dataset
    """
    print ("Reading the file from: ",file)
    df = pd.read_csv(file,index_col=False)
    return df

def joinDataset(df1, df2, left_keys, right_keys):
    """Read csv from files
    
    Parameters
    ----------
    df1: pandas.DataFrame
        1st dataframe
    df2: pandas.DataFrame
        2nd dataframe
    left_keys: list
        keys from left table on which joining will be performed
    right_keys: list
        keys from right table on which joining will be performed
    
    
    Returns
    ----------
    pandas.DataFrame
        Returns the dataframe
    """
    import pandas as pd
    df_result= pd.merge(df1, df2, left_on=left_keys, right_on=right_keys)
    print("Join operation successful !")
    return df_result
    
