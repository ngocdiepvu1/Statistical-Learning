# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/vudiep/Downloads/CSC542_Assignment5")

library(ISLR2)
library(caTools)
library(randomForest)
library(gbm) # library for boosting
library(tree)

##########################
# Question 1: Regression #
##########################

# Load data and names
regression_data <- read.csv("parkinsons_updrs.csv", header=TRUE)
regression_names <- readLines("parkinsons_updrs.names")

names(regression_data)
head(regression_data)
print(regression_names)

# Drop motor_UPDRS and total_UPDRS for input variables
regression_input_data = subset(regression_data, select = -c(motor_UPDRS, total_UPDRS))
head(regression_input_data)

# Do not include total_UPDRS as input to train the model
regression_dataset = subset(regression_data, select = -c(total_UPDRS))
# Creating a sample diving into the ratio of 80:20 
regression_sample <- sample.split(regression_dataset, SplitRatio = 0.80) 

# 80% for training - 4475 examples
regression_training_dataset <- subset(regression_dataset, sample == TRUE) 
cat("Regression Training Dataset:", nrow(regression_training_dataset), "examples")
head(regression_training_dataset)

# 20% for testing - 1400 examples
regression_testing_dataset <- subset(regression_dataset, sample == FALSE) 
cat("Regression Testing Dataset:", nrow(regression_testing_dataset), "examples")
head(regression_testing_dataset)

###################################
# b. Random Forests (m = root(p)) #
###################################

set.seed(1)
p = ncol(regression_training_dataset)
rf.regression = randomForest(motor_UPDRS~.,data=regression_training_dataset,mtry=sqrt(p),importance=T)

yhat.rf = predict(rf.regression, newdata=regression_testing_dataset)

# MSE = 3.387622
mean((yhat.rf - regression_testing_dataset$motor_UPDRS)^2)

# importance(): view the importance of each variable
# %IncMSE: mean decrease of accuracy in predictions on the OOB samples when a 
# given variable is excluded from the model
# IncNodeImpurity: total decrease in node impurity that results from splits over
# that variable, averaged over all trees (RSS in regr. vs. deviance in class)
importance(rf.regression)

# varImpPlot(): Variance importance plot
varImpPlot(rf.regression)

###############
# b. Boosting #
###############

set.seed(1)
# For regression problem, set the distribution to "gaussian"
# For binary classification,  use "bernoulli"
# n.trees: number of trees we want
# interaction.depth: limits the depth of each tree
boost.regression = gbm(motor_UPDRS~.,data=regression_training_dataset,
                 n.trees=5000, interaction.depth=4)

# In this case, summary() produces the relative influence plot and outputs 
# the relative influence statistics
summary(boost.regression)

# Partial dependence plots: illustrate the marginal effect of the selected variables
# on the response after integrating out the other variables
par(mfrow=c(1,2))

# motor_UPDRS values are decreasing with rm
plot(boost.regression,i="DFA")

# And increasing witb test_time
plot(boost.regression,i="test_time")

# Performance on the test set
yhat.boost=predict(boost.regression,newdata=regression_testing_dataset,n.trees=5000)

# MSE - 0.6214679
mean((yhat.boost -regression_testing_dataset$motor_UPDRS)^2)

##############################
# Question 2: Classification #
##############################

# Load data and names
classification_data <- read.csv("breast-cancer-wisconsin.csv", header=FALSE)
classification_names <- readLines("breast-cancer-wisconsin.names")

# Add headers
colnames(classification_data) <- c("id", "clump", "cell_size", "cell_shape", "adhesion", 
                    "pithelial_size", 'nuclei', 'chromatin', 'nucleoli',
                    'mitoses', 'class')
# Drop id column
classification_data <- classification_data[, -1]

# Change values for class: 0 for benign, 1 for malignant
classification_data$class <- ifelse(classification_data$class == 2, 0, 
                     ifelse(classification_data$class == 4, 1, classification_data$class))

# Missing attribute values: 16
# There are 16 instances in Groups 1 to 6 that contain a single missing
# (i.e., unavailable) attribute value, now denoted by \"?\" 
# Find and remove those rows with missing values
rows_with_missing_values <- which(apply(classification_data, MARGIN = 1, function(row) any(row == "?")))
print(classification_data[rows_with_missing_values, ])
clean_classification_data <- classification_data[-rows_with_missing_values, ]
# Set value types to integer
clean_classification_data[] <- lapply(clean_classification_data, function(x) as.integer(as.character(x)))

names(classification_data)
head(classification_data)
print(classification_names)

# Creating a sample diving into the ratio of 80:20 
classification_sample <- sample.split(clean_classification_data, SplitRatio = 0.80) 

# 80% for training: 520 examples
classification_training_dataset <- subset(clean_classification_data, sample == TRUE) 
cat("Classification Training Dataset:", nrow(classification_training_dataset), "examples")

# 20% for testing: 163 examples
classification_testing_dataset <- subset(clean_classification_data, sample == FALSE) 
cat("Classification Testing Dataset:", nrow(classification_testing_dataset), "examples")

#####################
# a. Decision Trees #
#####################

# Fit the classification tree on the training set
tree.classification <- tree(class ~ ., data = classification_training_dataset)

# Predict the target variable on the testing set
tree.probs <- predict(tree.classification, newdata = classification_testing_dataset)

# Get performance measures
num_test = length(tree.probs)
tree.probs[1:10]

# Initialize vector with 683 elements
tree.pred = rep("0", num_test)
# Assign "1" to probabilities > 0.5
tree.pred[tree.probs >.5]="1"

# table(): provides the confusion matrix
table(tree.pred, classification_testing_dataset$class)

# Fraction of examples for which our prediction was correct - 0.945
mean(tree.pred==classification_testing_dataset$class)
 