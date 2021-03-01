# Medical Insurance Cost Prediction

Medical expense is a crucial issue for every US citizen as it is relatively high amongst first-world countries. Medical needs are unaffordable to most of the population without medical insurance. Therefore medical insurance is a huge business opportunity for insurance companies. 

<p align="center">
  <img src="https://www.expatbyexpat.com/public/img/big/Internationalmedicalinsuranceforexpatfamilyjpeg_5cbfdf50e15ef.jpeg">
</p>

The aim of this study is to analyze and predict the medical insurance premium (charges) for individuals in the United States. A US insurance company wants to understand how much medical insurance premium should be charged. I use exploratory data analysis to check the quality of the data and highlight variables linked to the medical charges.

The dataset is downloaded from https://www.kaggle.com/mirichoi0218/insurance. The dataset contains these variables:

- age
- sex
- bmi
- children
- smoker
- region
- charges

Below are the steps of the project:

- check the data structure and outliers
- remove outliers
- check correlations between variables
- plot graphs to visualize the correlations
- build a model of linear regression
- remove insignificant coefficients 
- examine the residuals
- check if mean is zero, normality, auto-correlation(Durbin Watson test), homoskedasticity(Breush Pagan test), and multicollinearity (variance inflation factor)
- find other outliers using Cook's distance
- create another model without all outliers and do the checks agains
- create covariance matrix taking into account heteroskedasticity
- perform t test on the model
- in-sample and out-of-sample analysis
- prediction
