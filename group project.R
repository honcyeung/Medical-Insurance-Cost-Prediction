###################################################################################
###############            Medical Insurance Cost Prediction         ##############
###################################################################################
### Group members:
# Elvira VERRENGIA
# Hon Ching YEUNG
# Suet Wah CHU

### Aim of the study:
# Medical expense is a crucial topic for every US citizen as 
# medical cost in the US is relatively high in the world. 
# Medical needs are unaffordable to most of the population 
# without medical insurance. Therefore medical insurance industry is 
# a critical product for insurance companies. The aim of this study is 
# to analyze and predict the medical insurance premium (charges) for individuals.

###################################################################################

library(corrgram) 
library(corrplot) 
library(caTools) 
library(magrittr)
library(data.table)
library(ggplot2)
library(zoo)
library(lmtest)
library (car)
library(sandwich)

# Import data 
insurance <- read.csv("insurance.csv") %>% `data.table`

# Check data
summary(insurance)
View(insurance)

# ------------------------------
# Clean the Data
# ------------------------------

# Check if there is NA value in the data
any(is.na(insurance))

# Check the structure of the data set
str(insurance)

# Convert the Categorical variables into factors
insurance$sex <- as.factor(insurance$sex) 
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

# Check outliers
par(mfrow = c(1,3))
boxplot(insurance$age, main = "Age")
boxplot(insurance$bmi, main = "BMI")
boxplot(insurance$children, main = "Children")
par(mfrow = c(1,1))

# Remove outliers in bmi
insurance.wo <- insurance[insurance$bmi < 47.275]

# Check the data without outliers
summary(insurance.wo)
View(insurance.wo)
boxplot(insurance.wo$bmi, main = "BMI without Outliers")

# ------------------------------
# Exploratory Data Analysis
# ------------------------------

# Correlation
# Check the correlation with CorrPlots

# Grab only numeric columns
num.cols <- sapply(insurance.wo, is.numeric)

# Filter to numeric columns for correlation
cor.data <- cor(insurance.wo[, ..num.cols])
cor.data

# Show the correlation between all the numeric variables (including independent and dependent)
corrplot(cor.data, method = "color")
corrgram(insurance.wo , order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)
# the darker the color, the higher the correlation
# the pie charts shows the proportion of the correlation between variables
# Relatively speaking, charges have a stronger correlation to age

# Plot of distribution of charges vs its log
par(mfrow = c(1,2))
plot(density(insurance.wo$charges), main = "Density plot of charges")
plot(density(log(insurance.wo$charges)), main = "Density plot of charges (log)")
par(mfrow = c(1,1))

# Plot for charges vs age
ggplot(data = insurance.wo, aes(age, charges)) + geom_point() + geom_smooth() + ggtitle("Plot of Charges by Age")

# Plot for charges vs age with Smoker
ggplot(data = insurance.wo, aes(age, charges)) + geom_point(aes(colour = factor(smoker))) + ggtitle("Plot of Charges by Age with Smoker")

# Plot for charges vs BMI
ggplot(data = insurance.wo, aes(bmi, charges)) + geom_point(colour = "green") + geom_smooth() + ggtitle("Plot of Charges by BMI")

# Plot for charges vs BMI with Smoker
ggplot(data = insurance.wo, aes(bmi, charges)) + geom_point(aes(colour = factor(smoker))) + ggtitle("Plot of Charges by BMI with Smoker")

# Plot for charges vs sex
ggplot(insurance.wo, aes(x = sex, y = charges)) + geom_jitter(aes(color = factor(sex)))+
  labs(title = 'Scatter plot of Charges by Sex')

# Plot for charges vs smoker
ggplot(insurance.wo, aes(x = smoker, y = charges)) + geom_jitter(aes(color = factor(smoker)))+
  labs(title = 'Scatter plot of Charges by Smoker')

# Plot for charges vs region
ggplot(insurance.wo, aes(x = region, y = charges)) + geom_jitter(aes(color = factor(region)))+
  labs(title = 'Scatter plot of Charges by No of Children')

# Plot for charges vs no. of children
ggplot(insurance.wo, aes(x = children, y = charges)) + geom_jitter(aes(color = factor(children)))+
  labs(title = 'Scatter plot of Charges by No of Children')

# ------------------------------
# Build a model
# ------------------------------

###### First Model ######

model <- lm(charges ~ . , insurance.wo)
summary(model)

# Remove insignificant coefficients one by one
model <- lm(charges ~ age + bmi + children + smoker + region, insurance.wo)
summary(model)
# the adjusted R2 is 0.7487 which means the 74.87% of the data can be explained by the model

### Residuals of the model ###

# Plot of residuals against fitted values
plot(model$residuals~model$fitted)

# Check if the mean is 0
summary(model$residuals)
# 0 mean

## Normality - Shapiro test
shapiro.test(model$residuals)
# P-value < 22e-16 < 5%
# Reject H0 : normally distributed
# the residuals are not normally distributed
# sample size is large enough and hence the model is robust

## Auto-correlation - Durbin Watson test  
# H0 : residuals are not autocorrelated
# H1 : residuals are autocorrelated
durbinWatsonTest (model, max.lag=1)
# accept H0: residuals are not auto-correlated

# Multicollinearity - Variance Inflation Factor(VIF)
vif(model)
# all the VIF are <5, so there is no multicollinearity

## Homoskedasticity - Breush Pagan test
# H0 : homoskedasticity 
# H1 : heteroskedasticity
bptest(model, studentize = FALSE, data = insurance.wo)
# p value < 5%, we reject H0
# heteroskedasticity exists

# Calculate Cook's distance
cooksD <- cooks.distance(model)

# Find other outliers using Cook's distance
n <- nrow(insurance.wo)
plot(cooksD, main = "Cook's Distance for Influential Obs")
abline(h = 4/n, col = "red")

# Identify influential points
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])

# Define new data frame with influential points removed
insurance.wo.removed <- insurance.wo[-influential_obs, ]

# Create a new model without all outliers
model2 <- lm(charges ~ age + bmi + children + smoker + region, insurance.wo.removed)
summary(model2)
summary(model)
# model 2 has a much better R-squared, i.e. 0.8479 > 0.7487
# this model can explain 84.79% of the data

### Regression assumption checking ###

## Check if the mean is 0
summary(model2$residuals)
# 0 mean

## Normality - Shapiro test
shapiro.test(model2$residuals)
# P-value < 2.2e-16 < 5%
# Reject H0 : normally distributed
# the residuals are not normally distributed
# sample size is large enough and hence the model is robust

## Auto-correlation - Durbin Watson test  
# H0 : residuals are not autocorrelated
# H1 : residuals are autocorrelated
durbinWatsonTest (model2, max.lag=1)
# accept H0: residuals are not auto-correlated

# Multicollinearity - Variance Inflation Factor(VIF)
vif(model2)
# all the VIF are <5, so there is no multicollinearity

## Homoskedasticity - Breush Pagan test
# H0 : homoskedasticity 
# H1 : heteroskedasticity
bptest(model2, studentize = FALSE, data = insurance.wo.removed)
# p value < 5%, we reject H0
# heteroskedasticity exists

## Covariance Matrix taking into account heteroskedasticity
vcov <- vcovHC(model2, type = "HC1")

## T test on the model which has heteroskedasticity
coeftest(model2, vcov. = vcov)
# Since heteroskedasticity exists, the t-value and standard errors(robust standard error) are adjusted
# the estimated coefficients remain the same as the original OLS is unbiased, only T-test becomes unreliable under heteroskedasticity
# all the predictors are still significant even the P-values are recalculated

## Interpretation of the coefficients ##
# Age : when age increases by 1 unit, i.e. 1 year older, the insurance premium increase by $252.35, holding other variables constant
# Bmi : when bmi increases by 1 unit, the insurance premium increase by $294.96, holding other variables constant
# Children : when children increases by 1 unit, i.e. one more child, the insurance premium increase by $493.92, holding other variables constant
# Smoker : a smoker pays $24747.34 more than a non-smoker, holding other variables constant
# Region : 
#       1) Northwest : Clients in Northwest pay $1003.73 less than Northeastern clients, holding other variables constant
#       2) Southeast : Clients in Southeast pay $1125.22 less than Northeastern clients, holding other variables constant
#       3) Southwest : Clients in Southeast pay $1094.49 less than Northeastern clients, holding other variables constant

# ------------------------------
# In-sample analysis
# ------------------------------

set.seed(10)

# Split up the dataset, basically randomly assigns a boolean to a new column "sample"
sample <- sample.split(insurance.wo.removed$charges, SplitRatio = 0.70)

train <- subset(insurance.wo.removed, sample == TRUE)
test <- subset(insurance.wo.removed, sample == FALSE)

# Train the model and test its predictability 
model_train <- lm(charges ~ age + bmi + children + smoker + region, insurance.wo.removed)
test$charges.predict <- predict(model_train, test)

# Plot a graph to check the quality of the model
plot(test$charges.predict ~ test$charges)
abline(lm(charges ~ charges.predict, test), col = "red")
# The model predicts most of the test data but not when the charges rise

# ------------------------------
# Out-of-sample analysis
# ------------------------------

# Train the model and test its predictability 
model_train <- lm(charges ~ age + bmi + children + smoker + region, train)
test$charges.predict <- predict(model_train, test)

# Plot a graph to check the quality of the model
plot(test$charges.predict ~ test$charges)
abline(lm(charges ~ charges.predict, test), col = "red")
# The model predicts most of the test data but not when the charges rise

# ------------------------------
# Prediction
# ------------------------------

predictor1 <- data.frame(age = 45, sex = "male", bmi = 40.5, children = 3, smoker = "no", region = "northwest")
predict(model2, predictor1, type = "response", interval = "prediction", level = 0.95)
# fit       lwr      upr
# 12667.94 3885.271 21450.6
# fit is the prediction
# then the confidence interval of the prediction (95%)
# the predicted charge is $12667.94
# at 95% confidence, the predicted charges is between $3885.271 and $21450.6

predictor2 <- data.frame(age = 85, sex = "female", bmi = 36.7, children = 1, smoker = "yes", region = "southwest")
predict(model2, predictor2, type = "response", interval = "prediction", level = 0.95)
# fit       lwr      upr
# 45309.77 36494.78 54124.76
# the predicted medical insurance premium is $45309.77
# at 95% confidence, the predicted charges is between $36494.78 and $54124.76

