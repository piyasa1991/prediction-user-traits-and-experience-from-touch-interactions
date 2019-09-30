def normality_test_1(df_features):
    '''Shapiro-Wilk Test'''
    from numpy.random import seed
    from numpy.random import randn
    from scipy.stats import shapiro
    # seed the random number generator
    seed(1)

    # normality test
    result_shapiro=[]
    print('***********Shapiro-Wilk Test*************')
    for i,col in enumerate(df_features.select_dtypes('float64').columns):
    #     print('Running test for column:{}'.format(col))
        stat, p = shapiro(df_features[col])
    #     print('Statistics=%.3f, p=%.3f' % (stat, p))
        # interpret
        alpha = 0.05 # 5% is the threshold
        if p > alpha:
            #Sample looks Gaussian (fail to reject H0)
            text='gaussian'
        else:
            #Sample does not look Gaussian (reject H0)
            text = 'not-gaussian'
        result_shapiro.append(text)
    #     print('Column: {} | Interpretation: {}'. format(col,text))

    print('finished')
    print('*'*100)


    '''Anderson-Darling Test'''
    from numpy.random import seed
    from numpy.random import randn
    from scipy.stats import anderson
    result_anderson=[]
    # seed the random number generator
    seed(1)
    print('***********Anderson-Darling Test*************')
    # normality test
    for i,col in enumerate(df_features.select_dtypes('float64').columns):
        result = anderson(df_features[col])
    #     print('Statistic: %.3f' % result.statistic)
        for j in range(len(result.critical_values)):
            sl, cv = result.significance_level[j], result.critical_values[j]
            if result.statistic < result.critical_values[j]:
                 #Sample looks Gaussian (fail to reject H0)
                text='gaussian'
    #             print('%.3f: %.3f, data looks normal (fail to reject H0)' % (sl, cv))
            else:
                #Sample does not look Gaussian (reject H0)
                text = 'not-gaussian'
    #         print('%.3f: %.3f, data does not look normal (reject H0)' % (sl, cv))
    #         print('Column: {} | Interpretation: {}'. format(col,text))
        result_anderson.append(text)
    print('finished')
    print('*'*100)

    data = {'Features':df_features.select_dtypes('float64').columns,
            'Shapiro-Wilk Test':result_shapiro,
            'Anderson-Darling Test':result_anderson}
    normality_test_result=pd.DataFrame(data=data)
    normality_test_result
    cols = normality_test_result[(normality_test_result['Shapiro-Wilk Test']=='not-gaussian') 
                          & (normality_test_result['Anderson-Darling Test']=='not-gaussian')]['Features']
    print('Out of %d columns only %d columns did not passed the test' %(len(df_features.select_dtypes('float64').columns),len(cols)))
    print(cols.tolist())
    
def normality_test(df):
    results ={}
    for col in df.select_dtypes('float64').columns:
        scores={}
    #     print('*****'+col+'****')
        alpha=0.05
        data = df[col].values
        stats = describe(data)
        z_skew,_=skewtest(data)
        z_kurt,_ =kurtosistest(data)
        scores['Skewness']=stats.skewness
        scores['Z skewness']=z_skew
        scores['SE skewness']=stats.skewness/z_skew
        scores['Kurtosis']=stats.kurtosis
        scores['SE kurotsis']=stats.kurtosis/z_kurt
        scores['Z kurtosis']=z_kurt
        
        k2 , p = normaltest(data)
        scores['D’Agostino and Pearson’s Statistics']=k2
        scores['D’Agostino and Pearson’s p value']=p
        scores['Normality']=p>alpha
        results[col]=scores
    return pd.DataFrame(data=results).T