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

# 12 seems to be an outlier. however it is a valid value. So no need to handle it in this case.




## Car Name contains both Brand and Model name. Separating them to 2 differnt fields.

carprice$car_brand <- as.factor(str_split(tolower(carprice$CarName), " ", simplify=T)[,1])
summary(carprice$car_brand)


# There are some mis spelled brand names in Car name.
# The same need to be corrected.


carprice$car_brand <- str_replace(carprice$car_brand, "maxda", "mazda")
carprice$car_brand <- str_replace(carprice$car_brand, "toyouta", "toyota")
carprice$car_brand <- str_replace(carprice$car_brand, "vokswagen", "volkswagen")
carprice$car_brand <- str_replace(carprice$car_brand, "vw", "volkswagen")
carprice$car_brand <- str_replace(carprice$car_brand, "porcshce", "porsche")
carprice$car_brand <-  as.factor(carprice$car_brand)

summary(carprice$car_brand)



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
ggplot(data=carprice, aes(x=curbweight , y= price, col=fueltype)) + geom_point()
ggplot(data=carprice, aes(x=curbweight , y= price, col=aspiration)) + geom_point()
ggplot(data=carprice, aes(x=enginesize , y= price)) + geom_point()
ggplot(data=carprice, aes(x=boreratio , y= price)) + geom_point()  # Little Noisy..
ggplot(data=carprice, aes(x=stroke , y= price)) + geom_point()  ## Noise 
ggplot(data=carprice, aes(x=compressionratio , y= price)) + geom_point()  # Skewed and Noisy.. More like Categorical
ggplot(data=carprice, aes(x=horsepower , y= price)) + geom_point()
ggplot(data=carprice, aes(x=peakrpm , y= price)) + geom_point()  ## Lots of Noise
ggplot(data=carprice, aes(x=citympg , y= price)) + geom_point()  ## Negative Correlation
ggplot(data=carprice, aes(x=highwaympg , y= price)) + geom_point()

# ggplot(data=carprice, aes(x=price , y= price)) + geom_point()


ggplot(data=carprice, aes(x=fueltype , y= price, fill=car_brand)) + geom_boxplot()
ggplot(data=carprice, aes(x=aspiration , y= price)) + geom_boxplot() 
ggplot(data=carprice, aes(x=car_brand , y=price, fill="red")) + geom_boxplot() 

ggplot(data=carprice, aes(x= , y= price)) + geom_point()

## Data Understadning summary:
# The above set of plots shown the following.
# 1. Mileage variables (citympg and highwaympg) have shown a negative relation ship with price. Explains that expensive cars are not for mileage.
# 2. City ID Card Name do not make any sense to plot them agains. Can be removed. However keeping it for ID tracking.
# 3. carheight, stroke, compressionratio, peakrpm shown a great deal of noise in the scatter plot, when plotted against price. So they can be removed.
# 4. For the variables to be removed, also found the correlation with price to rule out.
# 5. Plot for carheigth shows too much noice. Indicating car height doesnt indicate any relationship with price. Explains business understading that there are Tall SUVs and Short sports cars whihc are expensive.So Car height can be excluded.


## Find correlation as a second validation to those variables , which I am planning to remove. 

cor(carprice$price, carprice$carheight)
cor(carprice$price, carprice$stroke)
cor(carprice$price, carprice$compressionratio)
cor(carprice$price, carprice$peakrpm)

## All the above 4 variables shown insignifcant correlation. Can be removed from the data.
## Id and Name also can be removed now

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

# create the correlation matrix
carprice_num <- cor(carprice_num, use="pairwise.complete.obs")

# Ploting correlation matrix
corrplot.mixed(carprice_num)

# whiteblack <- c("white", "black")
# corrplot(carprice_num, order = "hclust", addrect = 2, col = whiteblack, bg = "gold2")

## Correlation Analysis to be added here ##

## citympg & highwaympg shows strong correlation between them. One of them can be used in the model
## Wheelbase, carlength, cardwidth & curbweigth shows strong correlations between them. one of the 4 can be used in the model
## Symboling and doornumber shows negligle strength of relationship with price. Can be removed. We can remove them at a later stage.

carprice$symboling <- NULL
carprice$doornumber <- NULL



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

# D2. Aspirations have only 2 levels. so directly covert to 0 & 1

summary(as.factor(carprice$fuelsystem))
str(carprice$aspiration)
levels(carprice$aspiration) <- c(0,1)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]
summary(as.factor(carprice$aspiration))


# D3 Covert carbody into dummy variables set

summary(carprice$carbody)
summary(carprice$drivewheel)
summary(carprice$enginetype)
summary(carprice$fuelsystem)


# D3. convert Car Body into dummy variable set.
summary(carprice$carbody)
dummy_1 <- data.frame(model.matrix( ~carbody, data = carprice))
View(dummy_1)
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carprice_1 <- cbind(carprice, dummy_1[,-1])
#View(carprice_1)
dummy_1 <- NULL


# D4. convert Drivewheel to dummy variable set
summary(carprice$drivewheel)
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = carprice))
View(dummy_1)
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carprice_1 <- cbind(carprice_1, dummy_1[,-1])
dummy_1 <- NULL


# D5. convert engine_type to dummy variable set
summary(carprice$enginetype)
dummy_1 <- data.frame(model.matrix( ~enginetype, data = carprice))
View(dummy_1)
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carprice_1 <- cbind(carprice_1, dummy_1[,-1])
dummy_1 <- NULL


# D6. convert Drivewheel to dummy variable set
summary(carprice$fuelsystem)
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = carprice))
View(dummy_1)
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carprice_1 <- cbind(carprice_1, dummy_1[,-1])
dummy_1 <- NULL


### Remove the variables that are no longer required (for which dummy variables are created.)

carprice_1$carbody <- NULL
carprice_1$drivewheel <- NULL
carprice_1$enginetype <- NULL
carprice_1$fuelsystem <- NULL

# Car brand do indicate some explanation to the pricing. as premium price is chanrged by certain brands.
# Introducing brand as a variable into the model will be useful when the objective is to predict pricing for the next model from one of the existing brands.
# In this case, this for the chinese automaker who wants to see if the pricing of their vehicle. So it may not be helpful.

brand_med_price <- carprice_1 %>% group_by(car_brand) %>% summarise(median(price))
names(brand_med_price)[2]  <- "brand_value_med"

carprice_1 <- merge(x=carprice_1, y=brand_med_price, by="car_brand", all.x = T)
View(carprice_1)

carprice_1$car_brand <- NULL


#################### Stage 3 ~ Model Building and Evaluation ###########################

# Divide dataset for Traning and Testing 

set.seed(100)

trainindices= sample(1:nrow(carprice_1), 0.7*nrow(carprice_1))

# generate the train data set
train_cp = carprice_1[trainindices,]
View(train_cp)

#Similarly store the rest of the observations into an object "test".
test_cp = carprice_1[-trainindices,]


#### STEP 2:  Modelling  ###########

## Build the first model with all the numeric variables.

carp_model_1 <- lm(price ~ ., data=train_cp)

summary(carp_model_1)
vif(carp_model_1)
alias(carp_model_1)

## Coeffcients are computed as NAs for some variables indicating "perfect multi-collinearity"
## Removing these 3 variables 

carp_model_2 <- lm(price ~	fueltype + 	aspiration + 	wheelbase + 	carlength + 	
                     carwidth + 	curbweight + 	cylindernumber + 	enginesize + 	
                     boreratio + 	horsepower + 	citympg + 	highwaympg + 	
                     carbodyhardtop + 	carbodyhatchback + 	carbodysedan + 	carbodywagon + 	
                     drivewheelfwd + 	drivewheelrwd + 	
                     enginetypedohcv + 	enginetypel + 	enginetypeohc + 	enginetypeohcf + 	enginetypeohcv + 	enginetyperotor + 	
                     fuelsystem2bbl + 	fuelsystemmpfi + 	fuelsystemspdi + 	fuelsystemspfi + 	brand_value_med , data=train_cp)

summary(carp_model_2)
vif(carp_model_2)

## No more aliases issues in the model 2. However a lot of variables not significant and at the same time there is still multi-collinearity issues
## Based on the corrplot plotted above lets remove some of the veraibles.
## Between citympg and highwaympg, we can remove citympg as it shows high vif. 
## Similarly wheelbase, carlength & carwidth, we can remove carlength
## Between cylinder number and engine size, we can remove enginesize

## Iteration 3 

carp_model_3 <- lm(price ~	fueltype + 	aspiration + 	wheelbase + 
                     carwidth + 	curbweight + 	cylindernumber + 	
                     boreratio + 	horsepower + 	highwaympg + 	
                     carbodyhardtop + 	carbodyhatchback + 	carbodysedan + 	carbodywagon + 	
                     drivewheelfwd + 	drivewheelrwd + 	
                     enginetypedohcv + 	enginetypel + 	enginetypeohc + 	enginetypeohcf + 	enginetypeohcv + 	enginetyperotor + 	
                     fuelsystem2bbl + 	fuelsystemmpfi + 	fuelsystemspdi + 	fuelsystemspfi + 	brand_value_med , data=train_cp)

summary(carp_model_3)
vif(carp_model_3)

## Now we can remove the least significant variables with high VIF

# Remove fuelsystem2bbl from this dummy group (higher VIF & not significant)
# Remove enginetypeohc from the engine type dummy group (higher VIF)
# Remove drivewheelrwd from Wheel drive droup (higher VIF)

carp_model_4 <- lm(price ~	fueltype + 	aspiration + 	wheelbase + 
                     carwidth + 	curbweight + 	cylindernumber + 	
                     boreratio + 	horsepower + 	highwaympg + 	
                     carbodyhardtop + 	carbodyhatchback + 	carbodysedan + 	carbodywagon + 	
                     drivewheelfwd + 	 	
                     enginetypedohcv + 	enginetypel  + 	enginetypeohcf + 	enginetypeohcv + 	enginetyperotor + 	
                     fuelsystemspfi + 	fuelsystemmpfi + 	fuelsystemspdi + 	
                     brand_value_med , data=train_cp)

summary(carp_model_4)
vif(carp_model_4)

## Fuel system set of variables do not show any signficance improvement. 
## Remove fuelsystemmpfi, enginetypeohcv


carp_model_5 <- lm(price ~	fueltype + 	aspiration + 	wheelbase + 
                     carwidth + 	curbweight + 	cylindernumber + 	
                     boreratio + 	horsepower + 	highwaympg + 	
                     carbodyhardtop + 	carbodyhatchback + 	carbodysedan + 	carbodywagon + 	
                     drivewheelfwd + 	 	
                     enginetypedohcv + 	enginetypel  + 	enginetypeohcf + enginetyperotor + 	
                     fuelsystemspfi + 	fuelsystemspdi + 	
                     brand_value_med , data=train_cp)

summary(carp_model_5)
vif(carp_model_5)

## Now both fuel system variables can be removed. not significant.
## Engine type rotr can be removed.
## Cylindernumber can be removed.. anyway horsepower takes care of number of cylinders

carp_model_6 <- lm(price ~	fueltype + 	aspiration + 	wheelbase + 
                     carwidth + 	curbweight + 		
                     boreratio + 	horsepower + 	highwaympg + 	
                     carbodyhardtop + 	carbodyhatchback + 	carbodysedan + 	carbodywagon + 	
                     enginetypedohcv + 	enginetypel  + 	enginetypeohcf + 
                     brand_value_med , data=train_cp)

summary(carp_model_6)
vif(carp_model_6)

## Curb weight is a function of size of the car. So car width and wheelbase can be removed as they are not significant.

carp_model_7 <- lm(price ~	fueltype + 	aspiration + 	curbweight + 		
                     boreratio + 	horsepower + 	highwaympg + 	
                     carbodyhardtop + carbodyhatchback + 	carbodysedan + 	carbodywagon + 	
                     enginetypedohcv + enginetypel  + enginetypeohcf , data=train_cp)

summary(carp_model_7)
vif(carp_model_7)


## Remove fueltype, bore ratio..


carp_model_8 <- lm(price ~		aspiration + 	curbweight + 		
                     + 	horsepower + 	highwaympg + 	
                     carbodyhardtop + carbodyhatchback + 	carbodysedan + 	carbodywagon + 	
                     enginetypedohcv + enginetypel  + enginetypeohcf , data=train_cp)

summary(carp_model_8)
vif(carp_model_8)

## Remove engine type variables, except enginetypeohcf

carp_model_9 <- lm(price ~	aspiration + 	curbweight + 	horsepower + 	highwaympg + 	
              carbodyhardtop + carbodyhatchback + 	carbodysedan + 	carbodywagon + 	enginetypeohcf , data=train_cp)

summary(carp_model_9)
vif(carp_model_9)

# We can remove aspiration which is the least significant of all

carp_model_10 <- lm(price ~	curbweight + 	horsepower + 	highwaympg + 	
                     carbodyhardtop + carbodyhatchback + 	carbodysedan + 	
                      carbodywagon + 	enginetypeohcf, data=train_cp)

summary(carp_model_10)
vif(carp_model_10)

## Now we have a model where all the variables are signficant. We can stop with dimensionality reduction here.

# Adding a variable for area (length*width to check if has an impact)

carprice_1$carsize.area <- carprice_1$carlength * carprice_1$carwidth
train_cp$carsize.area <- train_cp$carlength * train_cp$carwidth

carp_model_11 <- lm(price ~	curbweight + 	horsepower + 	highwaympg + 	carsize.area +
                      carbodyhardtop + carbodyhatchback + 	carbodysedan + 	
                      carbodywagon + 	enginetypeohcf, data=train_cp)

summary(carp_model_11)

## It didnt have. So we can stop with model_10 as the final regression model for now




## Redraw Correlation plot with dummy variables

## Calculate VIF

step <- stepAIC(carp_model_1, direction="both")

step

## Iteration 1: It shows that fuelsystem4bbl, fuelsystemidi, fuelsystemmfi have coeff NA




# str_split(str(carprice_1), ":")




#### STEP 2:  Validation  ###########







