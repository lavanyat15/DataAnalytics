# Import function
library(readr)

# Import data
cars <- read_csv("cars.csv")

# View summary and structure of data 
View(cars)
attributes(cars)
summary(cars)
str(cars)
names(cars)

# Renaming columns
names(cars)<-c("CarName","CarSpeed","Distance")
names(cars)

# Check for null values - NA
is.na(cars)

# Using function (Code->Extract functions wraps highlighted code in function)

#Plotting for columns

  #par(mfrow=c(5,1))
  #Scatter Plot shows somewhat linear relation ship between the 2 variables
  plot(cars$`Distance`,cars$`CarSpeed`)
  #abline(lm(cars$CarSpeed~cars$Distance), col="red") # regression line (y~x)
  lines(lowess(cars$Distance,cars$CarSpeed), col="blue") # lowess line (x,y)
  
  
  #Histogram
  hist(cars$CarSpeed) # slight Right skew
  hist(cars$Distance) #Normal
 
  #Normal Quantile Plot- is a way to see if your data is normally distributed.
  qqnorm(cars$`CarSpeed`)
  qqline(cars$`CarSpeed`)
  
  #Normal Quantile plot 
  qqnorm(cars$`Distance`)
  qqline(cars$`Distance`)


#Correlation between car speed and car distance. High correlation of 0.9
cor(cars$CarSpeed, cars$Distance)

#Create train and test sets
#These two lines calculate the sizes of each set but do not create the sets:
# set.seed(1579) This does nothing 
trainSize<-round(nrow(cars)*0.7) 
testSize<-nrow(cars)-trainSize

# Actually CREATE the sets in ransomized order
set.seed(1579)
training_indices<-sample(seq_len(nrow(cars)),size =trainSize)

trainSet<-cars[training_indices,]

testSet<-cars[-training_indices,] 


# Linear regression Model 
# Training Model
cars.regression <- lm(Distance ~ CarSpeed, trainSet)

# Prints summary statistics of model . 
# R2 0.9 , y=c+mx (y=-31.4117+4.8762*x )
summary(cars.regression)

#Prediction 
preds.cars <- predict(cars.regression,testSet)
preds.cars

#Playing around with functions available in regression

# Find what attribues are available in our model , Then using those attributes 
attributes(cars.regression)

coefficients(cars.regression)
fitted.values(cars.regression)

# plotting fitted values against residuals. 
# Seems like its a curve . This may not be a good fit for a linear model
# Residuals are mostly grouped below the regression line which might be bias
# indicate a bad fit despite a high R2. 
plot(fitted.values(cars.regression),residuals(cars.regression)) 
abline(0,0)

distresid <- cars.regression$residuals
distresid
# Residuals mostly on the Line
qqnorm(distresid)
qqline(distresid)

#Output comparison
y<--31.4117+4.8762*4
y

output <- cbind(testSet$CarSpeed,testSet$Distance, preds.cars)
output