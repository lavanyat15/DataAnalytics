# install.packages(readr) Package already installed 
# Error in install.packages : object 'readr' not found
library("readr")

IrisDataset <- read.csv('iris.csv') # Quotes were missing for file import

attributes(IrisDataset)
sapply(IrisDataset, class)
summary(IrisDataset) # data set name misspelled

str(IrisDataset) # data set name misspelled

names(IrisDataset)
is.na(IrisDataset)

#hist(IrisDataset$Species) # Species must be numeric
hist(IrisDataset$Sepal.Length) 
hist(IrisDataset$Sepal.Width) # Normal curve
hist(IrisDataset$Petal.Length)
hist(IrisDataset$Petal.Width)

plot(IrisDataset$Sepal.Length)
     
qqnorm(IrisDataset$Sepal.Length) # Missing field name in data set
qqline(IrisDataset$Sepal.Length)

qqnorm(IrisDataset$Sepal.Width) 
qqline(IrisDataset$Sepal.Width)

# Since the Goal is to predict Petal width from petal length lets focus on the below variables
qqnorm(IrisDataset$Petal.Length) 
qqline(IrisDataset$Petal.Length)

qqnorm(IrisDataset$Petal.Width) 
qqline(IrisDataset$Petal.Width)

# Non linear relationship ?? - inconclusive ? 
plot(IrisDataset$Petal.Length,IrisDataset$Petal.Width)
lines(lowess(IrisDataset$Petal.Length,IrisDataset$Petal.Width), col="blue") # lowess line (x,y)

# Correlation is strong - 96%
cor.test(IrisDataset$Petal.Length,IrisDataset$Petal.Width)   

#IrisDataset$Species<- as.numeric(IrisDataset$Species) # Warning message:NAs introduced by coercion  
     
trainSizeIris <- round(nrow(IrisDataset) * 0.8) # Converted to 0.8 from 0.2
     
testSizeIris <- nrow(IrisDataset) - trainSizeIris
     
trainSizeIris # Misspelled
     
testSizeIris
set.seed(312)    
trainSetIris <- IrisDataset[training_indices, ]
     
testSetIris <- IrisDataset[-training_indices, ]
     
     # set.seed(405) This is not neccessary 
     
    #trainSetIris <- IrisDataset[training_indices, ] # this is not necessary
     
    # testSetIris <- IrisDataset[-training_indices, ]# this is not necessary
     
     # Statement had testset. 
     # y~x variables were switched
     # LinearModel<- lm(trainSet$Petal.Length ~ trainSet$Petal.Width) 
     # The above  stmt gave warning message. Name matching (Model and Prediction)  fixed the warning
     # Warning message:'newdata' had 115 rows but variables found have 35 rows 
     
     LinearModel<- lm(Petal.Width ~ Petal.Length, trainSetIris)
     
     # r2 value seems to be very low (0.15) indicating a very weak
     summary(LinearModel)
     
     prediction<-predict(LinearModel,testSetIris) # Missing , 
     
     prediction
     
     distresid <- LinearModel$residuals
     distresid
    
     # r2 value seems to be very low (0.15) indicating a very weak
     # residual plot seems to have high variance between 0.2 - 0.35 .
     # there is not randomness in teh residual plot which indicates a bad fit.
     plot(fitted.values(LinearModel),residuals(LinearModel)) 
     abline(0,0)
     
     # Residuals are mostly on the line
     qqnorm(distresid)
     qqline(distresid)
     
     output <- cbind(testSetIris$Petal.Length,testSetIris$Petal.Width, prediction)
     output