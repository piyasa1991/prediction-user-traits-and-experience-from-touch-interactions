# Technical Details
The purpose of the thesis a comparison study of different machine learning techniques for predicting usability ratings and personality ratings from the touch interactions of the smartphones.
- The usability ratings used are : PQ and ATT
- Personality ratings used are : Big-5 traits (OCEAN)

This project uses six learning algorithms in total to perform regression task
1. *Linear Regression*
2. *Lasso Regression*
3. *Elastic Net Regression*
4. *MARS*
5. *Support vector machines*
6. *Random forest*

Two dataset have been provided for the study
- STUDY1
- STUDY2
- UX-Ratings
- UserTraits

### Exploratory Data Analysis
- Distribution of different variables in STUDY1
- Distribution of different variables in STUDY2
- Distribution of user ratings with respect to different usability - Impaired and Normal
- Distribution of user traits with respect to sex

### Data cleaning
Experimental errors and outliers have been removed. Outliers have been removed using the following technique
- Robust covariance estimators using *Mahalanobis distance*

### Feature Engineering
- UX prediction: Statistical derivatives with respect to session
- User Traits prediction: Statistical derivatives with respect to user


### Data Transformation
- It is also used to make the distribution normally distributed. Used Quantile Normalization to normalize the dataset. 
- Normality test was conducted to ensure that the dataset were transformed

### Dimensionality reduction
- Feature extraction using *principal component anaylsis PCA* (with n_components = 3, 80% variance and 95% variance)
- Feature selection using *Genetic algorithm*

### Requirements
- Python(>=3)
- R (caret)
- pyEarth
- Pandas
- Scikit-learn
- Seaborn
- Matplotlib
- Jupyter Notebook
