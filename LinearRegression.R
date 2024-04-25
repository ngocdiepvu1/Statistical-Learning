# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/vudiep/Downloads/CSC542_Assignment1")
install.packages("caTools")

library(ISLR2)
library(MASS) #Very large collection of datasets and functions
library(ggplot2)
library(corrplot)
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

####################################################
# Question 1: Descriptive information of variables #
####################################################

# Descriptive information about the variables included in the dataset
summary(input_data)

attach(input_data)
# Histogram of age
hist(age)

# Bar chart of the distribution of sex
ggplot(input_data, aes(x = factor(sex))) +
  geom_bar(fill = "gray", width = 0.3) +
  labs(title = "Distribution of Sex", x = "Sex", y = "Frequency") + 
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female"))

############################################################
# Question 2: Correlation between the different attributes #
############################################################

# Calculate the correlation matrix
correlation_matrix <- cor(subset(input_data, select = -c(subject., age, sex, test_time)))

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color")

##############################################################
# Question 3: Divide input dataset into training and testing #
##############################################################

# Dataset that includes motor_UPDRS for fitting the model
dataset = subset(data, select = -c(total_UPDRS))
# Creating a sample diving into the ratio of 80:20 
sample <- sample.split(dataset, SplitRatio = 0.80) 

# 80% for training
training_dataset <- subset(dataset, sample == TRUE) 
cat("Training Dataset:", nrow(training_dataset), "examples")

# 20% for testing
testing_dataset <- subset(dataset, sample == FALSE) 
cat("Testing Dataset:", nrow(testing_dataset), "examples")

################################################
# Question 4: Multiple linear regression model #
################################################

# a. Multiple linear regression model with all variables
lm.fit = lm(motor_UPDRS~., data=training_dataset)
summary(lm.fit)

# b. R^2 and RSE
summary(lm.fit)$r.sq #R^2
summary(lm.fit)$sigma #RSE

summary(data$motor_UPDRS)
