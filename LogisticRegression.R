# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/vudiep/Downloads/CSC542_Assignment2")
# install.packages("caTools")

library(ISLR2)
library(ggplot2)
library(corrplot)
library(caTools)

# Load data and names
data <- read.csv("breast-cancer-wisconsin.csv", header=FALSE)
names <- readLines("breast-cancer-wisconsin.names")

head(data)
dim(data)
print(names)

# ATTRIBUTE INFORMATION:
# 1. Sample code number            id number
# 2. Clump Thickness               1 - 10
# 3. Uniformity of Cell Size       1 - 10
# 4. Uniformity of Cell Shape      1 - 10
# 5. Marginal Adhesion             1 - 10
# 6. Single Epithelial Cell Size   1 - 10
# 7. Bare Nuclei                   1 - 10
# 8. Bland Chromatin               1 - 10
# 9. Normal Nucleoli               1 - 10
# 10. Mitoses                      1 - 10
# 11. Class:                      (2 for benign, 4 for malignant)

# Add headers
colnames(data) <- c("id", "clump", "cell_size", "cell_shape", "adhesion", 
                    "pithelial_size", 'nuclei', 'chromatin', 'nucleoli',
                    'mitoses', 'class')
# Drop id column
data <- data[, -1]

# Change values for class: 0 for benign, 1 for malignant
data$class <- ifelse(data$class == 2, 0, 
                     ifelse(data$class == 4, 1, data$class))

# Missing attribute values: 16
# There are 16 instances in Groups 1 to 6 that contain a single missing
# (i.e., unavailable) attribute value, now denoted by \"?\" 
# Find and remove those rows with missing values
rows_with_missing_values <- which(apply(data, MARGIN = 1, function(row) any(row == "?")))
print(data[rows_with_missing_values, ])
clean_data <- data[-rows_with_missing_values, ]
# Set value types to integer
clean_data[] <- lapply(clean_data, function(x) as.integer(as.character(x)))

####################################################
# Question 1: Descriptive information of variables #
####################################################

# Descriptive information about the variables included in the dataset
num_samples = dim(clean_data)[1]
print(num_samples)
summary(clean_data)
# Total samples: 683

attach(clean_data)
# Bar chart of the class distribution
ggplot(clean_data, aes(x = factor(class))) +
  geom_bar(fill = "skyblue", width = 0.3) +
  labs(title = "Class Distribution", x = "Class", y = "Frequency") + 
  scale_x_discrete(labels = c("0" = "Benign", "1" = "Malignant"))

# Class distribution:
# Benign: 444 (65.0%)
# Malignant: 239 (34.0%)"
class_distribution <- table(clean_data$class)
print(class_distribution)
class_distribution_percentage <- prop.table(class_distribution) * 100
print(class_distribution_percentage)

# Histograms of data
hist(clump, main="Histogram of Clump Thickness", xlab = "Values", col = "skyblue")
hist(cell_size, main="Histogram of Uniformity of Cell Size", xlab = "Values", col = "skyblue")
hist(cell_shape, main="Histogram of Uniformity of Cell Shape", xlab = "Values", col = "skyblue")
hist(adhesion, main="Histogram of Marginal Adhesion", xlab = "Values", col = "skyblue")
hist(pithelial_size, main="Histogram of Single Epithelial Cell Size", xlab = "Values", col = "skyblue")
hist(nuclei, main="Histogram of Bare Nuclei", xlab = "Values", col = "skyblue")
hist(chromatin, main="Histogram of Bland Chromatin", xlab = "Values", col = "skyblue")
hist(nucleoli, main="Histogram of Normal Nucleoli", xlab = "Values", col = "skyblue")
hist(mitoses, main="Histogram of Mitoses", xlab = "Values", col = "skyblue")

# Set up a 3x3 grid for plots
par(mfrow = c(3, 3))

# Print all columns but without formatted titles
for (col in colnames(clean_data[, -c(10)])) {
  hist(clean_data[[col]], main = col, xlab = col, col = "skyblue", border = "black")
}

# Reset the plotting layout
par(mfrow = c(1, 1))

############################################################
# Question 2: Correlation between the different attributes #
############################################################

# Correlation matrix and heatmap
correlation_matrix <- cor(clean_data[ , -1])
print(correlation_matrix)
corrplot(correlation_matrix, method = "color")

##############################################################
# Question 3: Divide input dataset into training and testing #
##############################################################

# Creating a sample diving into the ratio of 80:20 
sample <- sample.split(clean_data, SplitRatio = 0.80) 

# 80% for training: 547 examples
training_dataset <- subset(clean_data, sample == TRUE) 
cat("Training Dataset:", nrow(training_dataset), "examples")

# 20% for testing: 136 examples
testing_dataset <- subset(clean_data, sample == FALSE) 
cat("Testing Dataset:", nrow(testing_dataset), "examples")

#######################################
# Question 4: Linear Regression Model #
#######################################

# Predict class using all variables except id
# glm(): generalized linear models; similar to lm()
# family=binomial: indicates that it is a logistic regression
glm.fits = glm(class~.,data = training_dataset, family = binomial)
summary(glm.fits)

# coef(): access the coefficients for this fitted model
coef(glm.fits)
summary(glm.fits$coefficients)

# predict(): predict the probability of being benign or malignant
# type="response": outputs probabilities of the form P(Y=1|X)
# Predict on test set, remove class
glm.probs=predict(glm.fits, testing_dataset, type="response")
# Get performance measures
num_test = length(glm.probs)
glm.probs[1:10]

# Initialize vector with 683 elements
glm.pred = rep("0", num_test)
# Assign "1" to probabilities > 0.5
glm.pred[glm.probs >.5]="1"

# table(): provides the confusion matrix
table(glm.pred, testing_dataset$class)
# Fraction of examples for which our prediction was correct
mean(glm.pred==testing_dataset$class)


###################
# Question 6: LDA #
###################

# lda(): Linear Discriminant Analysis, part of the MASS package
# The syntax is exactly the same as for lm()
library(MASS)
lda.fit=lda(class~., data=training_dataset)
lda.fit

# Plot the linear discriminants
plot(lda.fit)

# Predict output for test set
lda.pred=predict(lda.fit, testing_dataset)
# predict() returns the class, posterior probabilities and the linear discriminants
names(lda.pred)

# Calculate performance metrics
lda.class=lda.pred$class
table(lda.class, testing_dataset$class)
mean(lda.class==testing_dataset$class)

