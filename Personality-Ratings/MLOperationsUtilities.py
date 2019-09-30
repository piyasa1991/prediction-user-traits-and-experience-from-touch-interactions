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
    import pandas as pd
    print ("Reading the file from: ",file)
    df = pd.read_csv(file)
    return df

def loadDataset(paths=['../datasets/files_generated/Personality/study1_features_data.csv',
                      '../datasets/files_generated/Personality/study2_features_data.csv'],target='Neuroticism'):
    """ prepares the data and loads it
    
    Parameters
    ----------
    paths: array
        Filenames to be read
    target: str
        perosnality label to be specified
    
    Returns
    ----------
    pandas.DataFrame
        Returns the dataframe containing the dataset
    """
    for path in paths:
        if 'study1' in path:
            df = readDataFromCsv(path)
            df= df.select_dtypes (['int64','float64']).drop(['VP','age','user_id'],axis=1)
            print('The shape of the data  currently in study1: ',df.shape)
            X_study1,y_study1= df.drop(['Neuroticism', 'Extraversion', 
                                        'Openness', 'Agreeableness','Conscientiousness'],axis=1),df[target]
        elif 'study2'in path:
            df = readDataFromCsv(path)
            df = df.select_dtypes(['int64','float64']).drop(['user_id','UserId','VP','Age','Handedness_Score'],axis=1)
            print('The shape of the data  currently in study2: ',df.shape)
            X_study2,y_study2=df.drop(['Neuroticism', 'Extraversion', 'Openness', 'Agreeableness','Conscientiousness'],axis=1),df[target]
        else:
            df = pd.read_csv(path,index_col=0)
            X,y=df.drop(['Neuroticism', 'Extraversion', 
                                        'Openness', 'Agreeableness','Conscientiousness','user_id'],axis=1),df[target]
    # concat both the studies
    if(len(paths)>1):
        X = pd.concat([X_study1,X_study2])
        y= pd.concat([y_study1,y_study2])
    
    print('The shape of the data after concating both the studies {}'.format(X.shape))
    print('The shape of the target after concating both the studies {}'.format(y.shape))
    assert df.isnull().values.any()==False, 'Please check for null values'
    df_result={'data':X,'target':y}
    return df_result