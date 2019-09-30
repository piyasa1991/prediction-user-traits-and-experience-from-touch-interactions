import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

#################################  Functions for EDA   ##########################################################################
def generateHeatMap(df):
    corr_df= df.corr()
    # Determines the correlation between the variables.
    fig, ax =  plt.subplots(figsize=(25,25)) 
    sns.set()
    ax = sns.heatmap(df.corr(), annot=True, linewidths = 0.5, cmap='viridis')

def generateBoxplot(df,dims):
    l = df.columns.values
    number_of_columns=12
    number_of_rows = len(l)-1/number_of_columns
    plt.figure(figsize=(number_of_columns,5*number_of_rows))
    for i in range(0,len(l)):
        plt.subplot(number_of_rows + 1,10,i+1)
        sns.set_style('whitegrid')
#         print(i)
        sns.boxplot(df[l[i]],color='green',orient='v')
        plt.tight_layout()
#     fig, ax = plt.subplots(figsize=dims)
#     # df_study1_subset = df
#     ax = sns.boxplot(ax=ax, data=df,orient='v')
#     plt.xticks(rotation=90)
#     plt.show()

def plotDistribution(df):
    import warnings
    warnings.filterwarnings('ignore')
    l = df.columns.values
    number_of_columns=12
    number_of_rows = len(l)-1/number_of_columns
    plt.figure(figsize=(2*number_of_columns,5*number_of_rows))
    for i in range(0,len(l)):
        plt.subplot(number_of_rows + 1,4,i+1)
        sns.distplot(df[l[i]],kde=True) 
#     sns.kdeplot(df_temp[l[i]],shade=True)
