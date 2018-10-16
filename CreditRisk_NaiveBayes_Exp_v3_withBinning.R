
# Clean the workspace
rm(list=ls())

# Load Packages
library("caTools")
library("e1071")
library("ggplot2")
library("caret")
#install.packages("pROC")
library("pROC")
library("corrplot")
#install.packages("naivebayes")
library("naivebayes")

#library("ROCR")



master_data <- read.csv("master_file.csv")
str(master_data)
View(master_data)


## Perform ChiSquare for test of independance




## Check NAs in master file
sum(is.na(master_data))
colSums(is.na(master_data))

summary(master_data$Performance.Tag)

master_data$Performance.Tag <- factor(master_data$Performance.Tag)
master_data$Presence.of.open.home.loan <- factor(master_data$Presence.of.open.home.loan)
master_data$Presence.of.open.auto.loan <- factor(master_data$Presence.of.open.auto.loan)

str(master_data)


mdn<- Filter(is.numeric, master_data)
str(mdn)
View(mdf)


## Correlation Plots to understand relationship between numeric variables

mdn$Application.ID <- NULL
colnames(mdn) <- c("Age", "Dependents", "Income", "CurRes", "CurComp", "DPD90_6", "DPD60_6", "DPD30_6", "DPD90_12", "DPD60_12", "DPD30_12", "CCU", "Trades6M", "Trades12M", "PLT6M", "PLT12M", "INQ6M", "INQ12M", "OSBal", "TotTrades")

par(cex = 0.7)
crb_data_mat <- cor(mdn, use="pairwise.complete.obs")
corrplot.mixed(round(crb_data_mat,2), tl.cex = 1/par("cex"))
corrplot(crb_data_mat)
par(cex = 1)

par("cex")
View(crb_data_mat)


## Chi Square test on categorical variables for test of independence.
## H0:  Variables are independent
## if p value is insignificant, i.e. p < 0.05, we reject H0. i.e. variables are not independent.

mdf <- Filter(is.factor, master_data)
str(mdf)
colnames(mdf)

## Chi-Square Test between Categorical Variables 
## To remove the variables that are dependent and also understand the variables impacting the target variable


chi_tab <- table(mdf$Gender, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Education, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Marital.Status..at.the.time.of.application., mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Profession, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Type.of.residence, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Presence.of.open.home.loan, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Presence.of.open.auto.loan, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value


### Between Gender & other categorical variables
# c("Gender", "Marital.Status..at.the.time.of.application.", "Education","Profession", "Type.of.residence","Presence.of.open.home.loan", "Presence.of.open.auto.loan")

chi_tab <- table(mdf$Gender, mdf$Education)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Gender, mdf$Type.of.residence)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Gender, mdf$Marital.Status..at.the.time.of.application.)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Gender, mdf$Profession)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Gender, mdf$Presence.of.open.home.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Gender, mdf$Presence.of.open.auto.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

### Between Marital Status and other Variables

chi_tab <- table(mdf$Marital.Status..at.the.time.of.application., mdf$Education)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Marital.Status..at.the.time.of.application., mdf$Type.of.residence)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Marital.Status..at.the.time.of.application., mdf$Profession)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Marital.Status..at.the.time.of.application., mdf$Presence.of.open.home.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Marital.Status..at.the.time.of.application., mdf$Presence.of.open.auto.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

### 


### Between Education and other Variables

chi_tab <- table(mdf$Education, mdf$Type.of.residence)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Education, mdf$Profession)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Education, mdf$Presence.of.open.home.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Education, mdf$Presence.of.open.auto.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

### 


### Between Type.of.residence and other Variables


chi_tab <- table(mdf$Type.of.residence, mdf$Profession)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Type.of.residence, mdf$Presence.of.open.home.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Type.of.residence, mdf$Presence.of.open.auto.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

### 

### Between Profession and other Variables

chi_tab <- table(mdf$Profession, mdf$Presence.of.open.home.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$Profession, mdf$Presence.of.open.auto.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

### 

### Between Loan variables 

chi_tab <- table(mdf$Presence.of.open.auto.loan, mdf$Presence.of.open.home.loan)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

### 

### Experimentation with new Variables

mdf$presence.of.both.loans <- ifelse(mdf$Presence.of.open.auto.loan == 1 & mdf$Presence.of.open.home.loan == 1, 1,0)
mdf$presence.of.any.open.loan <- ifelse(mdf$Presence.of.open.auto.loan == 1 | mdf$Presence.of.open.home.loan == 1, 1,0)

chi_tab <- table(mdf$presence.of.both.loans, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

chi_tab <- table(mdf$presence.of.any.open.loan, mdf$Performance.Tag)
cq <- chisq.test(chi_tab)
cq$statistic
cq$p.value

## Both The new variables seems to show dependency with target variables. Costruct glm to check their importance relative to other variables.

loan_mod <- glm(Performance.Tag ~ Presence.of.open.auto.loan+Presence.of.open.home.loan+presence.of.any.open.loan, data=mdf, family="binomial")
summary(loan_mod)
loan_mod <- glm(Performance.Tag~Presence.of.open.auto.loan+Presence.of.open.home.loan+presence.of.any.open.loan+presence.of.both.loans, data=mdf, family="binomial")
summary(loan_mod)
loan_mod <- glm(Performance.Tag~Presence.of.open.home.loan*Presence.of.open.auto.loan, data=mdf, family="binomial")
summary(loan_mod)

# Above experimentation indicate that adding new variables do not change the significance of the loan variables much
# also there is no multiplicative effect due to the presence of both loans and only home loan seems to impact the decision taken on the approval status.



## ----- Modelling on master data set ------------## 

# the p-value of less than 0.05 which indicates a string correlation.
# Naive Bayes

set.seed(100)

split_ind <- sample.split(master_data$Performance.Tag, SplitRatio=0.7)

nb_train <- master_data[split_ind,]
nb_test <- master_data[!split_ind,]


summary(nb_train$Performance.Tag)
summary(nb_test$Performance.Tag)

# View(nb_train[,-1])

# From e1071
nb_mod1 <- naiveBayes(Performance.Tag ~ ., data=nb_train[,-1], laplace = 1)

nb_mod1

# From naivebayes
nb_mod2 <- naive_bayes(Performance.Tag ~ ., data=nb_train[,-1])

nb_mod1$tables$Age

summary(nb_train$Age)
mean(nb_train$Age)
sd(nb_train$Age)

nb_mod2$tables$Age

summary(nb_mod1)
summary(nb_mod2)

class(nb_mod1)
class(nb_mod2)

summary(nb_mod1$apriori)
summary(nb_mod1$tables)
summary(nb_mod1$levels)
summary(nb_mod1$call)

# Obtain Class Predictions
nb_pred_c1 <- predict(nb_mod1, newdata = nb_test, type="class")
nb_pred_c2 <- predict(nb_mod2, newdata = nb_test, type="class")


# Obtain Class Probabilities
nb_pred_p1 <- predict(nb_mod1, newdata = nb_test, type="raw")
nb_pred_p2 <- predict(nb_mod2, newdata = nb_test, type="prob")
head(nb_pred_p1,10)
head(nb_pred_p2,10)

# Both models from these 2 packages returning more or less similar probabilities.
# lets choose the model from e1071 package.

# Convert Matrix to Data Frame
nb_pred_p_df1 <- as.data.frame(nb_pred_p1)
nb_pred_p_df2 <- as.data.frame(nb_pred_p2)

# View(nb_pred_p)
# View(nb_pred_p_df)

colnames(nb_pred_p_df1) <- c("prob_0", "prob_1")
colnames(nb_pred_p_df2) <- c("prob_0", "prob_1")


nb_test$pred_perf <- nb_pred_p1

table(nb_pred_c1)
table(nb_pred_c2)
table(nb_test$Performance.Tag)

confusionMatrix(nb_pred_c1, nb_test$Performance.Tag)
confusionMatrix(nb_pred_c2, nb_test$Performance.Tag)



## Plot ROC Curve 

ROC1 <- roc(nb_test$Performance.Tag, nb_pred_p_df1$prob_1)
plot(ROC1, col="orange")
auc(ROC1)

ROC2 <- roc(nb_test$Performance.Tag, nb_pred_p_df2$prob_1)
plot(ROC2, col="Red")
auc(ROC2)

## Positive Class: 0
## Accuracy:      83.83 %
## Sensitivity:   86.32 %
## Specificity:   27.47 % (Ability to detect defaulters has been poor)
## AUC:           0.6658

###  End of Naive Bayes Experimentation


## ----- Modelling on demographic subset ------------## 



dim(master_data)

# Create dataframe for demographic data

demo_data <- master_data[,c(1:11,29)]


## Bin the continuous variables into appropriate number of bins based on their range.

max(demo_data$Age) - min(demo_data$Age)
unique(cut(demo_data$Age, breaks = 5))
demo_data$Age_bin <- cut(demo_data$Age, breaks = 5)

max(demo_data$Income) - min(demo_data$Income)
unique(cut(demo_data$Income, breaks = 4))
demo_data$income_bin <- cut(demo_data$Income, breaks = 4)

max(demo_data$No.of.dependents) - min(demo_data$No.of.dependents)
unique(cut(demo_data$No.of.dependents, breaks = 2))
demo_data$dependents_bin <- cut(demo_data$No.of.dependents, breaks = 2)

max(demo_data$No.of.months.in.current.residence) - min(demo_data$No.of.months.in.current.residence)
unique(cut(demo_data$No.of.months.in.current.residence, breaks = 6))
demo_data$cur_res_bin <- cut(demo_data$No.of.months.in.current.residence, breaks = 6)

max(demo_data$No.of.months.in.current.company) - min(demo_data$No.of.months.in.current.company)
unique(cut(demo_data$No.of.months.in.current.company, breaks = 6))
demo_data$cur_emp_bin <- cut(demo_data$No.of.months.in.current.company, breaks = 6)

View(demo_data)


##--------- Modelling for demographic data -------------##

set.seed(10)

demo_split_1 <- sample.split(demo_data$Performance.Tag, SplitRatio=0.7)

demo_train <- demo_data[demo_split_1,]
demo_test <- demo_data[!demo_split_1,]


colnames(demo_train)
dim(demo_train)
dim(demo_test)

str(demo_train)

# From e1071
# With Continuous variables ( based on PDF)
nb_dem_mod1 <- naiveBayes(Performance.Tag ~ Age + Gender + Marital.Status..at.the.time.of.application. + No.of.dependents +
                           Income + Education + Profession + Type.of.residence + No.of.months.in.current.residence +
                           No.of.months.in.current.company, 
                          data=demo_train[,-1], laplace = 1)

# With Binned Variables
nb_dem_mod2 <- naiveBayes(Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                           Education + Profession + Type.of.residence + Age_bin + 
                           income_bin + cur_res_bin + cur_emp_bin + dependents_bin, 
                         data=demo_train[,-1], laplace = 1)


# print the model
nb_dem_mod1
nb_dem_mod2



# Obtain class predictions
nb_pred_d_c1 <- predict(nb_dem_mod1, newdata = demo_test, type="class")
nb_pred_d_c2 <- predict(nb_dem_mod2, newdata = demo_test, type="class")

summary(nb_pred_d_c1)
summary(nb_pred_d_c2)

# Obtain Class Probabilities
nb_pred_d_p1 <- predict(nb_dem_mod1, newdata = demo_test, type="raw")
nb_pred_d_p2 <- predict(nb_dem_mod2, newdata = demo_test, type="raw")


# Convert to dataframe
nb_pred_d_p1_df <- as.data.frame(nb_pred_d_p1)
nb_pred_d_p2_df <- as.data.frame(nb_pred_d_p2)


colnames(nb_pred_d_p1_df) <- c("prob_0", "prob_1")
colnames(nb_pred_d_p2_df) <- c("prob_0", "prob_1")


View(nb_pred_d_p2_df)

# Confusion Matrix 
confusionMatrix(nb_pred_d_c1, nb_test_demo$Performance.Tag)
confusionMatrix(nb_pred_d_c2, nb_test_demo$Performance.Tag)

## Conclusion:
## Demographich data fields are not that useful when it comes to identifying the detaulters.
## As shown above, the specificity of 0 is not acceptable 
## It also possibly conveys that the application decision could have been made primarily made using credit bureau fields.

# Sensitivity : 1.0000          
# Specificity : 0.0000     [Not a single applicant identified as a defaulter]
# Pos Pred Value : 0.9577          
# Neg Pred Value :    NaN


