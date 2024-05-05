# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/vudiep/Downloads/CSC542_Assignment3")

library(ISLR2)
library(MASS) #Very large collection of datasets and functions
library(caTools)

# Load data and names
data <- read.csv("parkinsons_updrs.csv", header=TRUE)
names <- readLines("parkinsons_updrs.names")

# ATTRIBUTE INFORMATION:
# subject# - Integer that uniquely identifies each subject
# age - Subject age
# sex - Subject gender '0' - male, '1' - female
# test_time - Time since recruitment into the trial. The integer part is the number of days since recruitment.
# motor_UPDRS - Clinician's motor UPDRS score, linearly interpolated
# total_UPDRS - Clinician's total UPDRS score, linearly interpolated
# Jitter(%),Jitter(Abs),Jitter:RAP,Jitter:PPQ5,Jitter:DDP - Several measures of variation in fundamental frequency
# Shimmer,Shimmer(dB),Shimmer:APQ3,Shimmer:APQ5,Shimmer:APQ11,Shimmer:DDA - Several measures of variation in amplitude
# NHR,HNR - Two measures of ratio of noise to tonal components in the voice
# RPDE - A nonlinear dynamical complexity measure
# DFA - Signal fractal scaling exponent
# PPE - A nonlinear measure of fundamental frequency variation

names(data)
head(data)
print(names)

####################################################
# Multiple linear regression model in Assignment 1 #
####################################################

# a. Multiple linear regression model with all variables
lm.fit = glm(motor_UPDRS~., data=training_dataset)
summary(lm.fit)

# b. R^2 and RSE
summary(lm.fit)$r.sq #R^2
summary(lm.fit)$sigma #RSE

summary(data$motor_UPDRS)

#####################
# Question 2: LOOCV #
#####################

library(boot) # library that contains functions for cross-validation

# glm(): functions as lm() if no parameter is passed to the family argument
# glm allows using CV directly
glm.fit = glm(motor_UPDRS~., data=data)
summary(glm.fit)

# cv.glm(): produces a list with several components - all variables
cv.err=cv.glm(data,glm.fit)

# The two numbers in the delta vector contain the cross-validation results
# Standard estimate & bias-corrected
cv.err$delta # 6.304531 6.304524
cv.error$delta[1] # cross-validation error 6.304531

# Repeat the procedure for increasingly complex polynomial fits - only total_UPDRS
cv.error=rep(0,5)
for (i in 1:5) {
  glm.fit = glm(motor_UPDRS~poly(total_UPDRS, i), data=data)
  cv.error[i]=cv.glm(data,glm.fit)$delta[1]
}
cv.error # 6.794456 6.363090 5.652495 5.617618 5.425486

#########################
# Question 3: 5-fold CV #
#########################

# Multiple linear regression with all variables
cv.err.5=cv.glm(data,glm.fit,K=5)
# Standard estimate & bias-corrected
cv.err.5$delta # 6.298328 6.289679
cv.err.5$delta[1] # cross-validation error 6.298328

# With total_UPDRS with increasingly complex polynomial fits
set.seed(8)
cv.error.5=rep(0,5)
for (i in 1:5){
  glm.fit = glm(motor_UPDRS~poly(total_UPDRS, i), data=data)
  cv.error.5[i]=cv.glm(data,glm.fit,K=5)$delta[1]
}
cv.error.5 # 6.793008 6.362945 5.653259 5.614438 5.424380

