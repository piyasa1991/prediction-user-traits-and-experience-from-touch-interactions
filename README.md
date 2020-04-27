# Comparison of different Machine Learning techniques to predict user experience and user traits from touch interactions

## Abstract
Determining the user experience and personality is often time consuming and expensive process as it requires traditional measures such as filling out questionnaires. Hence there increased a need to automate the evaluation process in determining these ratings. Recent works have shown that quality ratings and personality dimensions can be estimated using touch interactions from smartphones which do not require additional effort in recording it. This paper thus provides a comparative study in understanding which methods works best with the touch data. The touch interactions were recorded from a tablet input where the subjects used a cognitive training application. The users were asked to spell the words correctly in the application by tapping and dragging the letters to their denoted placeholders shown in the screen. The application was presented in three forms to the user, one with the normal usability and the other two with the impaired usability name TinyIcons and Freezing.
The experience of the user is measured using AttrakDiff Mini questionnaire and captured in quality dimensions and the personality traits were assessed using the NEO-FFI and captured in Big-5 traits. The statistical features derived from different touch behaviors were converted into principal components using PCA and were fed into many regression algorithms. The models were built using 10 times cross-validation on two different data sets. Our findings showed that pragmatic quality, attractiveness, and the Big-5 traits performed well when the algorithms learned from the principal components. The models explained a mean $R^2$ of 0.58 and 0.46 for PQ and ATT, respectively. Our results also showed that personality can not only be classified but can also be predicted as real values where extraversion is predicted best and achieved a mean $R^2$ of 0.72. Important touch behaviors are analyzed which can in future help to uncover more about the user perception and their personality. Hence, this study can also help choose suitable methods when building an automatic analytical tool for estimating user experience and their traits.

### Technical Details
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
