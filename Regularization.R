# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/vudiep/Downloads/CSC542_Assignment4")

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

# Count the total number of missing values in the dataset
total_missing <- sum(is.na(data))
print(total_missing)

# Drop motor_UPDRS and total_UPDRS for input variables
input_data = subset(data, select = -c(motor_UPDRS, total_UPDRS))
head(input_data)

######################################################
# Multiple linear regression model from Assignment 1 #
######################################################

# Do not include total_UPDRS as input to the model
dataset = subset(data, select = -c(total_UPDRS))
# Creating a sample diving into the ratio of 80:20 
sample <- sample.split(dataset, SplitRatio = 0.80) 

# 80% for training - 4475 examples
training_dataset <- subset(dataset, sample == TRUE) 
cat("Training Dataset:", nrow(training_dataset), "examples")
head(training_dataset)

# 20% for testing - 1400 examples
testing_dataset <- subset(dataset, sample == FALSE) 
cat("Testing Dataset:", nrow(testing_dataset), "examples")

# Multiple linear regression model with all variables
lm.fit = lm(motor_UPDRS~., data=training_dataset)
summary(lm.fit)

# b. R^2 and RSE
summary(lm.fit)$r.sq #R^2
summary(lm.fit)$sigma #RSE

summary(data$motor_UPDRS)

#####################
# Question 2: Lasso #
#####################
library(glmnet)

x.train = model.matrix(motor_UPDRS~.,training_dataset)[,-1]
head(x_train)
y.train = training_dataset$motor_UPDRS
head(y.train)

x.test = model.matrix(motor_UPDRS~.,testing_dataset)[,-1]
y.test = testing_dataset$motor_UPDRS

x = model.matrix(motor_UPDRS~.,dataset)[,-1]
y = dataset$motor_UPDRS

grid = 10^seq(10,-2,length=100)

# alpha=1 for Lasso
lasso.mod=glmnet(x.train,y.train,alpha=1,lambda=grid)
plot(lasso.mod)

# Use CV to calculate test error
set.seed(1)
cv.out=cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out)
# a. Optimize the parameter lambda using CV
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x.test)

# b. Once lambda.min is obtained calculate the MSE on the test set
mean((lasso.pred-y.test)^2)

# Several coefficients are exactly zero
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef

###################
# Question 3: PLS #
###################
library(pls)

set.seed(1)
# plsr(): PLS, also in the pls library, same syntax as pcr()
pls.fit = plsr(motor_UPDRS~.,data=training_dataset,scale=T,validation="CV")
summary(pls.fit)

# Lowest CV error with M=6
pls.pred=predict(pls.fit,x.test,ncomp=7)
mean((pls.pred-y.test)^2)

# Refit using all the data
pls.fit=plsr(motor_UPDRS~.,data=data,scale=TRUE,ncomp=7)
summary(pls.fit)


