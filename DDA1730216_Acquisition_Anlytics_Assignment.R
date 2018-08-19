## Acquistion Analytics Assignment - Natarajan Ganapathi - DD1730216 


#-------------------------------------------------------
# Section 0: Preliminary Tasks
#-------------------------------------------------------

## Only minimal code from Data Analysis is included here as the most of the code for EDA is reused from Lecture for the first 2 Sections

library(MASS)
library(car)
library(e1071)
library(dplyr)
library(ggplot2)
library(cowplot)

## Data Loading and preliminary checks

# Loading bank marketing data in the working directory. 

bank_data <- read.csv("bank_marketing.csv")
str(bank_data)
summary(bank_data)
dim(bank_data)

# Checking missing values
sum(is.na(bank_data))
# No missing values

# Checking for Duplicate data 
 
sum(duplicated(bank_data))
nrow(unique(bank_data))
View(bank_data[duplicated(bank_data),])

# There are 12 duplicate records in the file. Removing the same. We can assume they belong to different people. 
# However for the purpose of assignment and based on lectures so far, I am assuming that they are indeed duplicates and hence removing them.

bank_data <- unique(bank_data)

# Creating Unique Prospect ID for the remaning data

bank_data$prospectId <- seq.int(nrow(bank_data))
View(bank_data)
#-------------------------------------------------------
# Section 1: Business Understanding: Prospect Profiling
#-------------------------------------------------------


# Checking the overall response rate of prospects

response_rate <- nrow(subset(bank_data, response == "yes"))/nrow(bank_data)
response_rate

# 0.1126627 is the overall response rate 


## 1.1 -- Univariate Analysis -- Already performed using the Summary Function. Hence not included here.

## UniVariate plots for continuous variables

## 1.1.1 Age

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Outlier treatement in Age 
quantile(bank_data$age,seq(0,1,0.01))
boxplot(bank_data$age)

# Box plot shows quiet a no of outliers. Capping it to 71.
bank_data[(which(bank_data$age>71)),]$age <- 71

# Binning the age variable and store it into "binning.age" for the purpose of analysis.
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)


# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "prospects_responded","total_prospects")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))

#-------------------------------------------------------
# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, prospects_responded,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
# Bank_data_age20 <- subset(bank_data,age <20)

# View(Bank_data_age20)
# summary(Bank_data_age20)

# Observations:
# Age groups 30-40 and 

## Also removeing age bin which is not required anymore.
bank_data$binning.age <- NULL


# 1.1.2 - Writing A REUSABLE function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + 
	geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
	geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}

# 1.1.3 : job

levels(bank_data$job)
plot_response(bank_data$job, "job")

# 1.1.4: Marital status
summary(bank_data$marital)
levels(bank_data$marital)[4] <- "married"
plot_response(bank_data$marital,"marital")

# Let's see the education variables
plot_response(bank_data$education,"Education")

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Replotting education
plot_response(bank_data$education,"Education_levels")

# 1.1.5: Default 
table(bank_data$default)
plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

# 1.1.6: housing variables 
summary(bank_data$housing)
plot_response(bank_data$housing, "Housing")

# 1.1.7: "loan"
summary(bank_data$loan)
plot_response(bank_data$loan, "Loan Status")

## End of EDA on Client related variables (demographic)

## Observations From EDA Sofar:

####### 1.2 Campaign Related Variables

# 1.2.1: Contact 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

# 1.2.2: "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

# 1.2.3: "day_of_week" variable
plot_response(bank_data$day_of_week,"day_of_week")

# 1.2.4: "duration" variable: No Analysis done as it will not be used in the model. 

# 1.2.5:  "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 
summary(bank_data$campaign)

# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)
quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()

#1.2.6:  "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary
summary(bank_data$pdays)
levels(bank_data$pdays)

# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")

# Number of prospects under each category
table(bank_data$pdays)


# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")

# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)
plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")


## End of EDA on Campaign related varibanles

#1.3.1:  Start of Socio economic variables

# 1.3.1 emp.var.rate :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)


### Data Cleansing and EDA Ends 


###-----------------------------------------------------###
###---------  Assigment Ask 2: Model Building ----------###
###-----------------------------------------------------###

## Expectations:
# Model without duration variable. 


# bank_data$duration <- NULL



##---------Logistic Regression----------#

# Loading Required Packages

library(caret)
library(caTools)
#install.packages("dummies")
library(dummies)

# Creating dummy variables
# Converting target variable to integer so that dummy variables are not created automatically.
View(bank_data)

bank_data$response <- as.integer(bank_data$response)
summary(bank_data$response)

bank_data_bkp <- bank_data  # backup dataset, to use if needed.
bank_data <- dummy.data.frame(bank_data)

# Converting the response variableback to factor.
bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

# splitting into train and test data

set.seed(100)
split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)
train <- bank_data[split_indices, ]
test <- bank_data[!split_indices, ]

# Load required Libraries 

library(MASS)
library(car)

## Removing duration & prospectId from modelling as required.
## Also removing ProspectId from the model through it appears as significant if you include in the model as it does not make sense to include the Unique ID variable.


logistic_1 <- glm(response ~ . - duration - prospectId, family = "binomial", data = train)

summary(logistic_1)


## Calling Stepwise Regression to remove variables for multi-cillinearity and insighnificance

logistic_2 <- stepAIC(logistic_1, direction = "both")

# StepAIC has removed the variables and resulted in the new model.

##  logistic_2 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
##       educationPrimary_Education + educationSecondary_Education + 
##        housingno + contactcellular + monthaug + monthdec + monthjun + 
##       monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
##       campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
##       `previousNever contacted` + poutcomefailure + emp.var.rate + 
##       cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
##       `jobblue-collar`, family = "binomial", data = train)


a <- vif(logistic_2)
a[a>10]
summary(logistic_2)

# Removing euribor3m from the model

logistic_3 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                    educationPrimary_Education + educationSecondary_Education + 
                    housingno + contactcellular + monthaug + monthdec + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    `previousNever contacted` + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

a <- vif(logistic_3)
a[a>10]
summary(logistic_3)

## Rmoving `previousNever contacted` due to

logistic_4 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                    educationPrimary_Education + educationSecondary_Education + 
                    housingno + contactcellular + monthaug + monthdec + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

a <- vif(logistic_4)
a[a>4]
summary(logistic_4)

## All high VIF variables are showing singifincant. So removing least significant variable housingno ( p highest and > .05)

logistic_5 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                    educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthaug + monthdec + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

a <- vif(logistic_5)
a[a>4]
summary(logistic_5)

## All high VIF variables are showing singifincant. So removing least significant variable Jobadmin. ( p highest and > .05)

logistic_6 <- glm(formula = response ~ age  + jobretired + jobstudent + 
                    educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthaug + monthdec + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

a <- vif(logistic_6)
a[a>2]
summary(logistic_6)

## All high VIF variables are showing singifincant. So removing least significant variable monthdec ( p highest and p < .05, but max)

logistic_7 <- glm(formula = response ~ age  + jobretired + jobstudent + 
                    educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthaug + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

summary(logistic_7)
a <- vif(logistic_7)
a[a>2]




## All high VIF variables are showing singifincant. So removing least significant variable primary education ( p highest and p < .05, but max)

logistic_8 <- glm(formula = response ~ age  + jobretired + jobstudent + educationSecondary_Education + 
                    contactcellular + monthaug + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

summary(logistic_8)

a <- vif(logistic_8)
a[a>2]


## All high VIF variables are showing singifincant. So removing least significant variable secondary education ( p highest and p < .05, but max)

logistic_9 <- glm(formula = response ~ age  + jobretired + jobstudent + 
                    contactcellular + monthaug + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

summary(logistic_9)

data.frame(logistic_9$coefficients)

a <- vif(logistic_9)
a[a>2]

cor(bank_data$nr.employed, bank_data$emp.var.rate)
cor(bank_data$cons.price.idx, bank_data$cons.conf.idx)

#Removing age

logistic_10 <- glm(formula = response ~ jobretired + jobstudent + 
                    contactcellular + monthaug + monthjun + 
                    monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar`, family = "binomial", data = train)

summary(logistic_10)

data.frame(logistic_9$coefficients)

a <- vif(logistic_10)
a[a>2]

# Removing day_of_weekfri

logistic_11 <- glm(formula = response ~ jobretired + jobstudent + 
                     contactcellular + monthaug + monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     + poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)

summary(logistic_11)

data.frame(logistic_10$coefficients)

a <- vif(logistic_10)
a[a>2]

cor(bank_data$nr.employed, bank_data$emp.var.rate)
cor(bank_data$cons.price.idx, bank_data$cons.conf.idx)

# Little confused here as to which one to remove here, between nr.employed & emp.var.rate?
# Removing emp.var.rate as it shows the highest correlation with a similar variable(employee based) i.e. nr.employed

logistic_12 <- glm(formula = response ~ jobretired + jobstudent + 
                     contactcellular + monthaug + monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     + poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)

summary(logistic_12)

data.frame(logistic_12$coefficients)

a <- vif(logistic_12)
a[a>2]

# Removing cons.price.idx


logistic_13 <- glm(formula = response ~ jobretired + jobstudent + 
                     contactcellular + monthaug + monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     + poutcomefailure + cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)

summary(logistic_13)

data.frame(logistic_12$coefficients)

a <- vif(logistic_12)
a[a>2]

# Removing monthaug

logistic_14 <- glm(formula = response ~ jobretired + jobstudent + 
                     contactcellular +  monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     + poutcomefailure + cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)

summary(logistic_14)



# Removing monthaug

logistic_15 <- glm(formula = response ~ jobstudent + 
                     contactcellular +  monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     + poutcomefailure + cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)

summary(logistic_15)

vif(logistic_15)

logistic_final <- logistic_15

## Logistic_15 will be the final model as no more multi collinearity and all variables highly significant. 

##-------------- Final Model Summary : --------------##

# Call:
# glm(formula = response ~ jobstudent + contactcellular + monthjun + 
#     monthmar + monthmay + monthnov + day_of_weekmon + campaign + 
#     pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
#     +poutcomefailure + cons.conf.idx + nr.employed + `jobblue-collar`, 
#     family = "binomial", data = train)

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.0760  -0.3896  -0.3322  -0.2596   2.9070  

# Coefficients:
#                                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    53.0632314  1.4808125  35.834  < 2e-16 ***
# jobstudent                      0.4052878  0.1038213   3.904 9.47e-05 ***
# contactcellular                 0.5163975  0.0608922   8.481  < 2e-16 ***
# monthjun                        0.2655078  0.0742623   3.575 0.000350 ***
# monthmar                        0.9002512  0.1173428   7.672 1.69e-14 ***
# monthmay                       -0.6693722  0.0578725 -11.566  < 2e-16 ***
# monthnov                       -0.3730627  0.0763768  -4.885 1.04e-06 ***
# day_of_weekmon                 -0.2718764  0.0540050  -5.034 4.80e-07 ***
# campaign                       -0.0414094  0.0116293  -3.561 0.000370 ***
# pdaysContacted_in_first_10days  1.2821306  0.0843375  15.202  < 2e-16 ***
# pdaysContacted_after_10days     1.1985219  0.1665934   7.194 6.28e-13 ***
# poutcomefailure                -0.5662434  0.0658292  -8.602  < 2e-16 ***
# cons.conf.idx                   0.0149576  0.0039527   3.784 0.000154 ***
# nr.employed                    -0.0106152  0.0002891 -36.717  < 2e-16 ***
# `jobblue-collar`               -0.2291183  0.0588115  -3.896 9.79e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 20293  on 28822  degrees of freedom
# Residual deviance: 16002  on 28808  degrees of freedom
# AIC: 16032

# Number of Fisher Scoring iterations: 6

##-------------- END of Final Model Summary : --------------##


## Assignment Ask 2.1: Predict Reponses for test data set

## Model Evaluation and Optimal Cutoff points
## Predicting probabilities of responding for the test data

test$pred_prob <- predict(logistic_final, newdata = test, type = "response")

predictions_logit <- predict(logistic_final, newdata = test, type = "response")

# Setting initial probability to 0.5 and check confusion matrix

predicted_response <- factor(ifelse(predictions_logit >= 0.5, "yes", "no"))
confmat1 <- confusionMatrix(predicted_response, test$response)
confmat1

##	Confusion Matrix and Statistics
##	
##	          Reference
##	Prediction    no   yes
##	       no  10811  1113
##	       yes   150   279
##	                                         
##	               Accuracy : 0.8978         
##	                 95% CI : (0.8923, 0.903)
##	    No Information Rate : 0.8873         
##	    P-Value [Acc > NIR] : 0.0001062      
##	                                         
##	                  Kappa : 0.2675         
##	 Mcnemar's Test P-Value : < 2.2e-16      
##	                                         
##	            Sensitivity : 0.9863         
##	            Specificity : 0.2004         
##	         Pos Pred Value : 0.9067         
##	         Neg Pred Value : 0.6503         
##	             Prevalence : 0.8873         
##	         Detection Rate : 0.8752         
##	   Detection Prevalence : 0.9653         
##	      Balanced Accuracy : 0.5934         
##	                                         
##	       'Positive' Class : no  


# This results in poor specificity. Trying to find optimal cutoff below.

## Assumption:
## Though the objective of the model is to capture high responders in the least number of calls, it might be better to choose a cutoff value, where sensitivity is high.
## But I am not sure whether we should always choose an optimal cutoff based on all the three metrics.
## For the purpose of assignment, I am going ahead with the prescribed approach of choosing the optimal cutoff based on all three metrics.



## Assigment Ask 2.3:  
## Find the optimal probability cut-off and report the relevant evaluation metrics

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)

summary(test$response)
summary(predicted_response)

levels(test$response)
levels(predicted_response)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs chart for Sensitivity, Specificity & Accuracy 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=11),seq(0,1,length=11),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

## Optimal cutoff point seems to be somewhere at .09

# Let's choose a cutoff value of 9% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.09, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
cm_values <- c(acc, sens, spec)
cm_values

## Answer 2.3 : Optimal values and metrics at cutoff 
## ---------------------------------------------------
## Optimal Cutoff Probability = 9% i.e (.09)
##    Accuracy Sensitivity Specificity 
##   0.7723630   0.6709770   0.7852386 



## -------- Assigment Ask 3:  -------- ##
## Create a data frame with the variables prospect ID, actual response, predicted response, predicted probability of response, duration of call in seconds, and cost of call
## While creating the data frame, calculate the cost of call for each prospect in a new column


# test$prospectId <- seq.int(nrow(test)) already created in the bank_data

test$pred_response <- predicted_response

## Creating a new data frame with required columns

prospect_df <- test[, c("prospectId", "response",  "pred_response", "pred_prob", "duration")]

# Renaming Columns
colnames(prospect_df) <- c("ProspectId", "ActualResponse",  "PredictedResponse", "PredictedProbability", "Duration")
View(prospect_df)

# Calculating 
prospect_df$CostOfCall <- 0.033*(prospect_df$Duration) + 0.8


# sorting the probabilities in decreasing order 
prospect_df <- prospect_df[order(prospect_df$PredictedProbability, decreasing = T), ]

#Downloading the data 
write.csv(prospect_df,"response_predictions_logistic.csv")



## -------- Assigment Ask 4:  Identify % prospect target -------- ##

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift

summary(prospect_df$ActualResponse)

prospect_df$ActualResponse <- as.factor(ifelse(prospect_df$ActualResponse=="yes",1,0))
prospect_df$PredictedResponse <- as.factor(ifelse(prospect_df$PredictedResponse=="yes",1,0))


LG = lift(prospect_df$ActualResponse, prospect_df$PredictedProbability, groups = 10)
View(LG)

## LG dataframe

> LG

##
##  bucket total totalresp Cumresp  Gain Cumlift
##    
##       1  1236      599     599   43.0    4.30
##       2  1235      249     848   60.9    3.05
##       3  1235      116     964   69.3    2.31
##       4  1236       62    1026   73.7    1.84
##       5  1235       92    1118   80.3    1.61
##                                 ------
##       6  1235       80    1198   86.1    1.43
##       7  1236       70    1268   91.1    1.30
##       8  1235       47    1315   94.5    1.18
##       9  1235       41    1356   97.4    1.08
##      10  1235       36    1392   100     1.00


# Looking at the Lift/Gain table produced above, to meet a business objective of 80% response rate, we need to target upto 5th decile (50% of customers).
# With this result, it looks like not an optimal model. Would have expected the coverage in top 20% or 2nd decile itself.
# Not sure if it has to do with the cutoff probability value chosen by me.
# Appreciate comments from TA regarding this.



## -------- Assigment Ask 5: Create Lift Charts  -------- ##

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")






## Questions / Doubts:

### Questions to TAs to Respond:
### 1. Given the objective is to have higher sensitivity, Is it OK to choose the initial probability itself. i.e. at 0.5 prob, I got a sensitivity of 0.98. 
###    Is that OK to choose in this context? However for the purpose of assignment, I chose the optiaml value based on all three metrics (Accuracy, Sensitivity, Specificity)

### 2. What are the issues if I choose a model with higer sensitivity but lower specificity.
### 3. By choosing such a low probability as a cutoff value, are we not defeating the purpose of probablistic model itself?
###    Chossing such a low p value, are we not simply tweaking the outcome to suit the      

