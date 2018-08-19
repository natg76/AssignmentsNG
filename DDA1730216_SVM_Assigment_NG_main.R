
##  Natarajan Ganapathi - DDA1730216          #################
##  File name: DDA1730216_SVM_Assignment_NG_main.r ############


## A1: Business / Domain Understanding:

## Looking at the dataset and also on internet, following are the observations obtained.
## 1.  
## 2. This dataset of handwritten images has been used for benchmarking classification algorithms.
## 3. Training set is 60K records and Test set contained 10K records.
## 3. Having observer performance problems with SVM cross validation on my machine, The assignment results are based on the 10K sample for Training.
## 4. Since Scoring is not an issue with large datasets, the test data is used as is. 

## A2:  Data Understanding
# 1. The datasets contained seprate files for training and testing data, both provided in CSV format.
# 2. First column of the train.csv file contains the digit and the remaining columns containing pixel value of 784 pixels (28x28)
# 3. It is expected that the pixels in the corners will not have any vlues and will be mostly blank (0)
# 4. Same thing can be said about the pixels around the sides and corner. 


library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(caTools)
library(ggplot2)
library(gridExtra)



## Step 1:  Loading Data
mn_train <- read.csv("mnist_train.csv",sep = ",", stringsAsFactors = F,header = F)
mn_test <- read.csv("mnist_test.csv",sep = ",", stringsAsFactors = F,header = F)

mn_train_orig <- mn_train

View(mn_train)
View(mn_test)

## 1.1 Rename Columns
## First column contains the digit label and the rest of the columns contains pix values from 1 to 784.

colnames(mn_train)[1] <- "digit_label"
x1 <- colnames(mn_train)[2:785]
colnames(mn_train)[2:785] <- gsub(" ", "", paste("pix_", as.character(as.numeric(substr(x1,2,5))-1), ""))


colnames(mn_test)[1] <- "dig_actual"
x2 <- colnames(mn_test)[2:785]
colnames(mn_test)[2:785] <- gsub(" ", "", paste("pix_", as.character(as.numeric(substr(x2,2,5))-1), ""))

## Keeping Original dataset for EDA uses. 
mn_train_orig <- mn_train

#View(mn_train)

## 1.2 Sanity Checks 

# 1.2.1
# Understanding Dimensions
dim(mn_train)

#Structure of the dataset
str(mn_train)

#Visually check first few rows
head(mn_train)

#Exploring the data
summary(mn_train)

# 1.3 data type conversions: 
# Changing output variable "digit label" to factor type 

mn_train$digit_label <- factor(mn_train$digit_label)
#mn_train_s$digit_label <- factor(mn_train_s$digit_label)
mn_test$dig_actual <- factor(mn_test$dig_actual)

# 1.4 Checking missing value in any of the pixels
sapply(mn_train, function(x) sum(is.na(x)))

# Check for NA in Dataset
sum(is.na(mn_train))


## Step : 2 - Exploratory Data Analysis

## Visualizations to understand the data better
## Print the digit to see how the data is organized
digit <- matrix(as.numeric(mn_train[1,-1]), nrow = 28) #look at one digit
image(digit, col = grey.colors(255))


## Check the intensity of each label

View(mn_train_orig)

mn_train_orig$intensity <- apply(mn_train_orig[,-1], 1, mean) #takes the mean of each row in train

intbylabel <- aggregate (mn_train_orig$intensity, by = list(mn_train_orig$digit_label), FUN = mean)

plot <- ggplot(data=intbylabel, aes(x=Group.1, y = x)) +
  geom_bar(stat="identity")
plot + scale_x_discrete(limits=0:9) + xlab("digit label") + 
  ylab("average intensity")






## Step 3 Data Cleansing & Transformations & Feature Engineering

# 3.1 Check for columns having same value for all records.
unique(mn_train[sapply(mn_train, function(x) length(unique(x)) == 1)])

# 3.2 Remove those columns having same value from data frame
l <- sapply(mn_train, function(x) length(unique(x))>1)
mn_test <- mn_test[l]
mn_train <- mn_train[l]



## 3.3 We are still left with lot of features with majority of values being 0.
## We can remove columns having 0s for more than 90% of records
# Below section commented out as I found that nearZeroVar funtion also removes these variables.

## sapply(mn_train[-1], function(x) sum(x==0)/length(x))
## sapply(mn_train, function(x) sum(x==0)/length(x) < 0.9)
## l2 <- sapply(mn_train, function(x) sum(x==0)/length(x) < 0.9)
## mn_train <- mn_train[l2]
## mn_test <- mn_test[l2]

## Step 3.4
## Use NearZeroVar method to remove ccolumns that have  same value for most of the columns
## Keeping threshhold at 95% for this and identify those columns.

cols_sv <- nearZeroVar(mn_train , freqCut = 95/5, names = TRUE , uniqueCut = 5)
mn_train <- mn_train[, -which(names(mn_train) %in% c(cols_sv))]
mn_test <- mn_test[, -which(names(mn_test) %in% c(cols_sv))]

# cols_sv

## Step 3.5
# Take 10K sample from training set for ksvm training set & 2K sample for Cross Validation
# Sample from Test set is taken as the cross validation is taking a lot of time to run on my machine

set.seed(10)
sample_tr_1 <- sample(1:nrow(mn_train), 10000)

set.seed(15)
sample_tr_2 <- sample(1:nrow(mn_train), 2000)


mn_train_s <- mn_train[sample_tr_1,] 
mn_train_s2 <- mn_train[sample_tr_2,] 


View(mn_test)
View(mn_train)
View(mn_test_s)




## Step 4 - Model Building

## 4.1 Linear SVM 

## KSVM model building. Build with C=1 
model_1 <- ksvm(digit_label ~ ., data = mn_train_s,scale = FALSE,C=1)
# Predicting the model results 
pred_dig_1 <- predict(model_1, newdata = mn_test)
# Confusion Matrix - Finding accuracy, Sensitivity and specificity
cm_l1 <- confusionMatrix(pred_dig_1, mn_test$dig_actual)




## KSVM model building with C = 10
model_10 <- ksvm(digit_label ~ ., data = mn_train_s,scale = FALSE,C=10)
# Predicting the model results 
pred_dig_10 <- predict(model_10, newdata = mn_test)
# Confusion Matrix - Finding accuracy, Sensitivity and specificity

cm_l2 <- confusionMatrix(pred_dig_10, mn_test$dig_actual)


cm_l1$overall
cm_l2$overall


## The above two models shows that Linear SVM mode at C=10 is slightly better compared to C=1.



# 4.3 Hyperparameter tuning and Cross Validation  - Linear - SVM 

tc <- trainControl(method="cv", number=5)
metric <- "Accuracy"
set.seed(7)
lgrid <- expand.grid(C=seq(1, 10, by=1))

# Performing 5-fold cross validation
lin.svm <- train(digit_label ~ ., data=mn_train_s, method="svmLinear", metric=metric, 
                 tuneGrid=lgrid, trControl=tc)

print(lin.svm)
plot(lin.svm)

## Inference from Cross Validation
# The 5 fold cross validation shows that accuracy is highest at C=1, and becomes low at c=4 and stays flat from C=7 onwards.
# The difference in accuracy of cross validation and ksvm could be due to the data samples used for training and scoring
# But the impact of C on accuracy is conflicting with what I got in ksvm
# It is better if TA can address this while providing solution.

## Modeling 4.4 - Non-Linear SVM using Kernels.


## Non Linear SVM models - RBF Kernel

model_nl_rbf_1 <- ksvm(digit_label ~ ., data = mn_train_s, scale = FALSE, kernel = "rbfdot")

pred_RBF_1 <- predict(model_nl_rbf_1, newdata = mn_test)

#confusion matrix - RBF Kernel
cm_nl1 <- confusionMatrix(pred_RBF_1,mn_test$dig_actual)

print(cm_nl1)

## RBF Kernel 


# Making grid of "sigma" and C values. 
set.seed(80)
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))
grid


# Performing 5-fold cross validation for 
# Sys.time()
fit.svm_radial <- train(digit_label ~ ., data=mn_train_s2, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=tc)
# Sys.time()

# Sys.time()
fit.svm_radial2k <- train(digit_label ~ ., data=mn_train_s2, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=tc)
# Sys.time()


# Printing cross validation result
print(fit.svm_radial)
# Best tune at sigma = 0.01 & C=2, Accuracy - 0.935

# Plotting model results
plot(fit.svm_radial2k)



## Conclusions:
## In my case, due to the way samples have been take, linear SVM at C=10 produced best results compared to all others
## However when running the Caret for Linear model