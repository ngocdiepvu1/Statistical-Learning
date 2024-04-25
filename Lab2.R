# Clear workspace
rm(list = ls())

# Set working directory
setwd("~/Work/Teaching/StatisticalLearning/Labs")


library(ISLR2)
library(MASS) #Very large collection of datasets and functions


############################
# Simple linear regression #
############################

# The Boston dataset contains data on 506 neighborhoods in Boston
# We will try to predict the median house value (medv) using 13 predictors
?Boston

# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's

names(Boston)
head(Boston)

# lm(): fits a simple linear regression model
# Basic syntax: lm(y~x, data)

lm.fit = lm(medv~lstat, data=Boston)

# If we don't want to specify the dataset each time use attach
attach(Boston)
lm.fit = lm(medv~lstat)

lm.fit #Limited information
summary(lm.fit) #Detailed information

# Extract coefficients
coef(lm.fit)

# Obtain confidence intervals for the coefficient estimates
confint(lm.fit)

# Obtain confidence intervals and prediction intervals for the prediction
# of medv for different values of lstat
# Although centered around the same point, prediction intervals are wider
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval = "confidence")
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval = "prediction")

# Plot the data with the least squares regression line
plot(lstat, medv)
abline(lm.fit, col="red")

# We can customize the plot
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="blue")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")

# Four diagnostic plots are automatically produced by applying the plot() 
# function directly to the output of lm()
par(mfrow=c(2,2)) #Divide the plotting region into a 2x2 grid of panels
plot(lm.fit)

# Calculate residuals and studentized residuals
par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
# In general, studentized residuals are going to be more effective for detecting
# outlying Y observations than standardized residuals
plot(predict(lm.fit), rstudent(lm.fit))

# Calculate leverage statistics
plot(hatvalues(lm.fit))
# Which observation has the largest leverage statistic?
which.max(hatvalues(lm.fit))


##############################
# Multiple linear regression #
##############################

# lm() function with more parameters in the model
lm.fit = lm(medv~lstat+age)
summary(lm.fit)

# To include all the possible predictors in the model
# Since we don't provide variable names, we must include the data argument
lm.fit = lm(medv~., data=Boston)
summary(lm.fit)

# To access individual components of the summary object we use $
summary(lm.fit)$r.sq #R^2
summary(lm.fit)$sigma #RSE

# vif(): Calculates variance inflation factors (VIFs)
# VIFs quantify the severity of multicollinearity in an ordinary 
# least squares regression analysis
library(car)
vif(lm.fit)

# Include all the possible predictors but one in the model
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

# We could also use the update() function
lm.fit1=update(lm.fit, ~.-age)
summary(lm.fit1)


#####################
# Interaction terms #
#####################

# Instead of manually including the interaction term in the model
# (lstat+age+lstat:age), we can use lstat*age which is a shorthand
# for the former
summary(lm(medv~lstat*age,data=Boston))


############################################
# Non-linear transformations of predictors #
############################################

# Use I due to "^2" having a special meaning in the formula
lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

# Use the anova() function to further quantify the extent to which the 
# quadratic fit is superior to the linear fit
lm.fit = lm(medv~lstat)
# H0: The two models fit the data equally well
# H1: The full model is superior
anova(lm.fit,lm.fit2)

# Let's look at the diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit2)

# poly(): Create polynomials within lm()
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

# Other transformations
summary(lm(medv~log(rm),data=Boston))


##########################
# Qualitative predictors #
##########################

# The Carseats dataset contains data on child car seats in 400 locations
names(Carseats)

# Shelveloc is categorical (Bad, Medium or Good)
# Indicates the quality of the shelving location within a store
# R generates dummy variables for qualitate predictors automatically
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

# contrasts(): Returns the coding that R uses for the dummy variables
attach(Carseats)
contrasts(ShelveLoc)

