def computeStats(df, groupbyCols=['sessionNr','App','Cond','user_id']):
    """Calculate the summarization of the data
    
    Parameters
    ----------
    df: pandas.DataFrame
        dataframe
    groupbyCols: list
        columns on which group by operation to be performed
        
    Returns
    ----------
    pandas.DataFrame
        Returns the dataframe
    
    """
    import pandas as pd
    df_temp =df.copy()
    df1= df_temp.groupby(by=groupbyCols).mean()
    print("Null values present in the data after computing mean:%s" %df1.isnull().values.any())
    compute(df1,"mean")
    df2= df_temp.groupby(by=groupbyCols).median()
    print("Null values present in the data after computing median:%s"%df2.isnull().values.any())
    compute(df2,"median")
    df3= df_temp.groupby(by=groupbyCols).skew()
    print("Null values present in the data after computing skew:%s"%df3.isnull().values.any())
    compute(df3,"skew")
    df4= df_temp.groupby(by=groupbyCols).apply(pd.DataFrame.kurt)
    print("Null values present in the data after computing kurtosis:%s"%df4.isnull().values.any())
    compute(df4,"kurt")
    df5= df_temp.groupby(by=groupbyCols).quantile()
    print("Null values present in the data after computing interquartile range:%s"% df5.isnull().values.any())
    compute(df5,"quantile")
    df6= df_temp.groupby(by=groupbyCols).std()
    print("Null values present in the data after computing standard deviation:%s"% df6.isnull().values.any())
    compute(df6,"std")
    df7= df_temp.groupby(by=groupbyCols).mad()
    print("Null values present in the data after computing mean absolute deviation:%s"% df7.isnull().values.any())
    compute(df7,"mad")
    df8= df_temp.groupby(by=groupbyCols).max()
    print("Null values present in the data after computing max:%s"% df8.isnull().values.any())
    compute(df8,"max")
    df9= df_temp.groupby(by=groupbyCols).min()
    print("Null values present in the data after computing min:%s"% df9.isnull().values.any())
    compute(df9,"min")
    df10= df_temp.groupby(by=groupbyCols).count()
    print("Null values present in the data after computing count:%s"% df10.isnull().values.any())
    compute(df10,"count")
    
    result = pd.concat([df1,df2,df3,
                        df4,
                        df5,df6,df7,df8,df9,df10], axis=1, sort=False)
    
    result.reset_index(inplace=True)
    result['user_id']=pd.to_numeric(result['user_id'])
    result['sessionNr']=pd.to_numeric(result['sessionNr'])
    print('Is the stats summ null after concating: ',result.isnull().values.any())
    print('%d number of rows and %d columns present in the data'%(result.shape[0],result.shape[1]))
    return result


def compute(df,name):
    """calculate the hit and miss rate
    Parameters
    ----------
    df: pandas.DataFrame
        Dataframe on which the process will be performed
    name: str
        Name of the dataframe
    """
    import pandas as pd
    for col in df.columns:
        df.rename(index=str,columns={col:col+"_"+name},inplace=True)
    if 'user_id'+'_'+name in df.columns:
        df.drop('user_id'+'_'+name,axis=1,inplace=True)
    if 'sessionNr'+'_'+name in df.columns:
         df.drop('sessionNr'+'_'+name,axis=1,inplace=True)
    if 'App'+'_'+name in df.columns:
         df.drop('App'+'_'+name,axis=1,inplace=True)
    if 'Cond'+'_'+name in df.columns :
        df.drop('Cond'+'_'+name,axis=1,inplace=True)
        
def calcHitRate(df,name='Study1'):
    """calculate the hit and miss rate
    Parameters
    ----------
    df: pandas.DataFrame
        Dataframe on which the process will be performed
    name: str
        Name of the dataframe
        
    Returns
    ----------
    pandas.DataFrame
        Returns the dataframe
    """
    sLength = len(df)
    df['hit_rate']=pd.Series(np.zeros(sLength), index=df.index)
    ids = df[(df['button_pressed']==True)&(df['correct_answer']==True)]['id']
    #create views to work on
    df_temp1 = df[df['id'].isin(ids)]
    df_temp2 = df[~df['id'].isin(ids)]
    df_temp1['hit_rate'] = df_temp1['hit_rate'].replace(0,1)
    df = pd.concat([df_temp1,df_temp2],sort=False)
    
    view = df[['sessionNr','hit_rate']]
    view.groupby(by='sessionNr').mean()
    #create dataframe for this
    df_11 = view.groupby(by='sessionNr').mean()
    df_11.reset_index(inplace=True)
    df_11['sessionNr']=pd.to_numeric(df_11['sessionNr'])
    return df_11
