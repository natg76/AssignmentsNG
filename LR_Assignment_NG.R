# Load essential libraries

library(ggplot2)
library(lubridate)
library(MASS)
library(car)
library(stringr)
library(dplyr)


# load the media company data
carprice <- read.csv("CarPrice_Assignment.csv", header=T)

str(carprice)
View(carprice)


#### STEP 1:  Sanity Checks & Cleaning  ###########

#Check for duplicate values
sum(duplicated(carprice$car_ID))

#Check for NA values across the entire data frame
sum(is.na(carprice))

## No NA values, so no need to check at the field level.

## Visual inspection shown tha doornumber and cylinder numbers are coded as text. 
## Can be converted to number.
## Car name contains Brand. That can be extracted.

summary(carprice$cylindernumber)
levels(carprice$cylindernumber)
carprice$cylindernumber <- case_when(
                            carprice$cylindernumber == "two" ~ 2,
                            carprice$cylindernumber == "three" ~ 3,
                            carprice$cylindernumber == "four" ~ 4,
                            carprice$cylindernumber == "five" ~ 5,
                            carprice$cylindernumber == "six" ~ 6,
                            carprice$cylindernumber == "eight" ~ 8,
                            carprice$cylindernumber == "twelve" ~ 12,
                            TRUE ~ as.numeric(0))

summary(carprice$cylindernumber)
boxplot(carprice$cylindernumber)


carprice$car_brand <- as.factor(str_split(tolower(carprice$CarName), " ", simplify=T)[,1])
summary(carprice$car_brand)

carprice$car_brand <- as.factor(str_replace(carprice$car_brand, "maxda", "mazda"))
carprice$car_brand <- as.factor(str_replace(carprice$car_brand, "toyouta", "toyota"))
carprice$car_brand <- as.factor(str_replace(carprice$car_brand, "vokswagen", "volkswagen"))
carprice$car_brand <- as.factor(str_replace(carprice$car_brand, "vw", "volkswagen"))
carprice$car_brand <- as.factor(str_replace(carprice$car_brand, "porcshce", "porsche"))

summary(carprice$car_brand)



# There are some mis spelled brand names in Car name.
# The same need to be corrected.




# 12 seems to be an outlier. however it is a valid value. So no need to handle it in this case.


## Same treatment for Door Numbers

summary(carprice$doornumber)
levels(carprice$doornumber)

carprice$doornumber <- case_when(
                          carprice$doornumber == "two" ~ 2,
                          carprice$doornumber == "four" ~ 4,
                          TRUE ~ as.numeric(0))

summary(carprice$doornumber)
boxplot(carprice$doornumber)



#### STEP 2:  EDA  ########### 

## 2.1 Univariate Analysis ### 

summary(carprice$symboling)
summary(carprice$CarName)

# Car Name field is not going to be useful. Brand name count be. However for the regression problem, we can exclude it.


summary(carprice$fueltype)
summary(carprice$aspiration)
summary(carprice$doornumber)
summary(carprice$carbody)
summary(carprice$drivewheel)

# carprice$drivewheel <- str_replace(carprice$drivewheel, "4", "f")
# Assumption:  4wd - assumed as Four Wheel Drive / All Wheel Drive vehicle. Do no seems to be a DQ issue in this field

summary(carprice$enginetype)
summary(carprice$fuelsystem)
summary(carprice$enginelocation)
# All cars except 3 have front engine. Not useful for analysis. Can be removed.
carprice$enginelocation <- NULL

## Numeric fields
summary(carprice$wheelbase)
summary(carprice$carlength)
summary(carprice$carwidth)
summary(carprice$carheight)
summary(carprice$curbweight)
summary(carprice$enginesize)
summary(carprice$boreratio)
summary(carprice$stroke)
summary(carprice$compressionratio)
summary(carprice$horsepower)
summary(carprice$peakrpm)
summary(carprice$citympg)
summary(carprice$highwaympg)
summary(carprice$price)


# Plot histograms for Numeric variables to quickly see the distribution

hist(carprice$wheelbase)
hist(carprice$carlength)
hist(carprice$carwidth)
hist(carprice$carheight)
hist(carprice$curbweight)
# All above normally distributed, nothing strange.

hist(carprice$enginesize)
# Engine size skewed more twoards the left

hist(carprice$boreratio)
hist(carprice$stroke)
hist(carprice$compressionratio)
hist(carprice$horsepower)
# Horsepower is also skewed to the left.

hist(carprice$peakrpm)
hist(carprice$citympg)
hist(carprice$highwaympg)
hist(carprice$price)
# Price is also skewed to left. Almost half of the cars priced below 10K.

## 2.2 Bivariate Analysis ### 

# Perform scatter plots for numeric variables to see the relationship

ggplot(data=carprice, aes(x=car_ID , y= price)) + geom_point()  # ID Field & Lots of Noise
ggplot(data=carprice, aes(x=wheelbase, y=price)) + geom_point()
ggplot(data=carprice, aes(x=carlength, y=price)) + geom_point()
ggplot(data=carprice, aes(x=carwidth, y=price)) + geom_point()
ggplot(data=carprice, aes(x=carheight, y=price)) + geom_point()  # Noise and widely spread.
ggplot(data=carprice, aes(x=curbweight , y= price)) + geom_point()
ggplot(data=carprice, aes(x=enginesize , y= price)) + geom_point()
ggplot(data=carprice, aes(x=boreratio , y= price)) + geom_point()  # Little Noisy..
ggplot(data=carprice, aes(x=stroke , y= price)) + geom_point()  ## Noise 
ggplot(data=carprice, aes(x=compressionratio , y= price)) + geom_point()  # Skewed and Noisy.. More like Categorical
ggplot(data=carprice, aes(x=horsepower , y= price)) + geom_point()
ggplot(data=carprice, aes(x=peakrpm , y= price)) + geom_point()  ## Lots of Noise
ggplot(data=carprice, aes(x=citympg , y= price)) + geom_point()  ## Negative Correlation
ggplot(data=carprice, aes(x=highwaympg , y= price)) + geom_point()

# ggplot(data=carprice, aes(x=price , y= price)) + geom_point()


## Data Understadning:
# The above set of plots shown the following.
# 1. Mileage variables (citympg and highwaympg) have shown a negative relation ship with price. Explains that expensive cars are not for mileage.
# 2. City ID Card Name do not make any sense to plot them agains. Can be removed. However keeping it for ID tracking.
# 3. carheight, stroke, compressionratio, peakrpm shown a great deal of noise in the scatter plot, when plotted against price. So they can be removed.
# 4. For the variables to be removed, also found the correlation with price to rule out.

# Above plot shows too much noice. Indicating car height doesnt indicate any relationship with price.
# Explains business understading that there are Tall SUVs and Short sports cars whihc are expensive.
# So Car height can be excluded.


cor(carprice$price, carprice$carheight)
cor(carprice$price, carprice$stroke)
cor(carprice$price, carprice$compressionratio)
cor(carprice$price, carprice$peakrpm)

## All the above 4 variables shown insignifcant correlation. Can be removed from the data.

carprice$carheight <- NULL
carprice$stroke  <- NULL
carprice$compressionratio <- NULL
carprice$peakrpm <- NULL
carprice$car_ID <- NULL
carprice$CarName <- NULL

## Plot the correlation matrix to identify the "Multi-colinearity Suspects"
library(corrplot)

# Filtering the records with numerical value
carprice_num <- Filter(is.numeric, carprice)

# creating correlation matrix
carprice_num <- cor(carprice_num, use="pairwise.complete.obs")

# Ploting correlation matrix
corrplot(carprice_num, method = "circle",order="FPC")

## Correlation Analysis to be added here ##




## Dummy variables for categorical variables.

# D1. Fuel type and Aspirations have only 2 levels. so directly covert to 0 & 1

summary(carprice$fueltype)
str(carprice$fueltype)
levels(carprice$fueltype) <- c(0,1)
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

# D2. Aspirations have only 2 levels. so directly covert to 0 & 1

summary(as.factor(carprice$aspiration))
str(carprice$aspiration)
levels(carprice$aspiration) <- c(0,1)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]
summary(as.factor(carprice$aspiration))


summary(carprice$carbody)
summary(carprice$drivewheel)
summary(carprice$enginetype)
summary(carprice$fuelsystem)


# D3. Now we come across variables having more than 3 levels. 
summary(carprice$carbody)

#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = carprice))

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carprice_1 <- cbind(carprice[,-5], dummy_1[,-1])
View(carprice_1)
dummy_1 <- NULL




# Now we come across variables having more than 3 levels. 
summary(carprice$carbody)

#Converting "furnishingstatus" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = carprice))

#check the dummy_1 data frame.
# View(dummy_1)
# First column from dummy_1 can be removed and 5th column (carbody) to be removed.
# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
carprice_1 <- cbind(carprice[,-5], dummy_1[,-1])
View(carprice_1)
dummy_1 <- NULL


#### STEP 2:  Modelling  ###########

model1


#### STEP 2:  Validation  ###########







