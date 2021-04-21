# Predict customer Brand preferences using GBM, Random Forest and C5.0

# Model PErformance based on Accuracy score - RF2 and C502 models

#Imports
library(caret)
library(readr)
library(gbm)

#Import dataset
completeResponses <- read_csv("SurveyData/CompleteResponses.csv")

# Find correlation

correlationMatrix <- cor(completeResponses[,1:7])
correlationMatrix
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#### Cor heatmap
library(reshape2)
melted_cormat <- melt(completeResponses)
head(melted_cormat)


####
completeResponses$elevel<-as.factor(completeResponses$elevel)
completeResponses$car <- as.factor(completeResponses$car)
completeResponses$zipcode <- as.factor(completeResponses$zipcode)
completeResponses$brand <- as.factor(completeResponses$brand)

attributes(completeResponses)
str(completeResponses)
summary(completeResponses)

#Count 
table(completeResponses$elevel)
table(completeResponses$brand)
table(completeResponses$car)
table(completeResponses$zipcode)


# Count of missing values in dataset - no missing data
sum(is.na(completeResponses))


# Histogram to check data distribution - Flat dstribution
hist(completeResponses$age)
hist(completeResponses$salary)
hist(completeResponses$credit)

# Split Data
# define an 75%/25% train/test split of the dataset
set.seed(1551)
inTraininggbm <- createDataPartition(completeResponses$salary, p = .75, list = FALSE)
traininggbm <- completeResponses[inTraininggbm,]
testinggbm <- completeResponses[-inTraininggbm,]

######  GBM   ######

#Tuning params
fitcontrolGbm1 <- trainControl(method = "cv", number = 10)

# Fit model
fitGbm1 <- train(brand~., data = traininggbm, method='gbm', trControl=fitcontrolGbm1)

fitGbm1

predsGbm1 <- predict(fitGbm1, testinggbm)
postResample(predsGbm1, testinggbm$brand)
        # Accuracy     Kappa 
        # 0.9239482 0.8400030 

# Confusion Matrix and four fold plot for visual of CM
cfm<-confusionMatrix(predsGbm1, testinggbm$brand)

fourfoldplot(cfm$table)
#### The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage =
###  0.1 and n.minobsinnode = 10.
 
      ####    Reference
      # Prediction    0    1
      # 0  867  110
      # 1   78 1417
      # 

      # Variable Importance
      # gbm variable importance
      # 
      # only 20 most important variables shown (out of 34)
      # 
      # Overall
      # salary   100.00000
      # age       75.01438
      # credit     1.46550
      # zipcode8   0.18019
      # car18      0.14329
      # elevel4    0.14154
      # car15      0.14122
      # car4       0.10456

importance <- varImp(fitGbm1)
# summarize importance
print(importance)
# plot importance
plot(importance)

####### Random Forest  #####

### Automatic
# Tuning parameters
set.seed(1515)
rfControl <- trainControl(method="cv", number=10)

# Fit Model 
set.seed(13)
system.time(fitRF1 <- train(brand~.,data=traininggbm, method='rf', trControl=rfControl))
fitRF1
# Predict
      ####           Reference
      # Prediction    0    1
      # 0  837   98
      # 1  108 1429
 
predictRf1 <- predict(fitRF1, testinggbm)
postResample(predictRf1, testinggbm$brand)
        # Accuracy     Kappa 
        # 0.9166667 0.8231962 
confusionMatrix(predictRf1, testinggbm$brand)


### Manual 
set.seed(13)
system.time(
fitRF2 <- train(brand~.,data=traininggbm, method='rf', trControl=rfControl, tuneLength=5))
fitRF2
###         Reference
      # Prediction    0    1
      # 0  849  101
      # 1   96 1426

#Prediction
predictRf2 <- predict(fitRF2, testinggbm)
postResample(predictRf2, testinggbm$brand)
        # Accuracy     Kappa 
        # 0.9203074 0.8314316 
cmRf2 <- confusionMatrix(predictRf2, testinggbm$brand)
fourfoldplot(cmRf2$table)
importanceRf2 <- varImp(fitRF2)
# summarize importance
print(importanceRf2)
# plot importance
plot(importanceRf2)
        # Overall
        # salary   100.0000
        # age       51.6659
        # credit    13.9493
        # elevel3    0.8993
        # elevel4    0.8240
        # elevel1    0.8213
        # elevel2    0.7658
        # zipcode3   0.4909
        # zipcode4   0.4897
        # zipcode6   0.4876
        # zipcode7   0.4655
        # zipcode2   0.4634
        # zipcode1   0.4451
        # zipcode5   0.4411
        # zipcode8   0.3464
        # car15      0.3000
        # car8       0.2092
        # car12      0.1529
        # car10      0.1418
        # car5       0.1413

### Manual mtry values  
set.seed(13)
system.time(
  fitRF3 <- train(brand~.,data=traininggbm, method='rf', trControl=rfControl, tuneGrid=expand.grid(mtry=c(1,10,18,26,34))))
fitRF3

        # Reference
        # Prediction    0    1
        # 0  841   96
        # 1  104 1431

#Prediction
predictRf3 <- predict(fitRF3, testinggbm)
postResample(predictRf3, testinggbm$brand)
        # Accuracy     Kappa 
        # 0.9190939 0.8284152 
cmRf3 <- confusionMatrix(predictRf3, testinggbm$brand)

importanceRf3 <- varImp(fitRF3)
# summarize importance
print(importanceRf3)
# plot importance
plot(importanceRf3)

        # Overall
        # salary   100.0000
        # age       51.6659
        # credit    13.9493
        # elevel3    0.8993
        # elevel4    0.8240

##### C5.0 Model  #####
library(C50)

set.seed(1516)
fitC501 <- C5.0(formula=brand~., data = traininggbm, trials = 6)
summary(fitC501)

      # Confusion Matrix and Statistics
      # Reference
      # Prediction    0    1
      # 0  809  112
      # 1  136 1415

predictc501 <- predict(fitC501, testinggbm)
postResample(predictc501, testinggbm$brand)
        # Accuracy     Kappa 
        # 0.8996764 0.7865450 
confusionMatrix(predictc501, testinggbm$brand)


# Using tunecontrol and expand grid 
#gridc501 <- expand.grid(.model="tree",.trials=6,.winnow="FALSE")
set.seed(1516)
fitC502 <- train(brand~., data = traininggbm, method='C5.0',trControl=rfControl)#,tuneGrid=expand.grid(.model="tree",.trials=6,.winnow="FALSE") )
summary(fitC502)
        # Confusion Matrix and Statistics
        # Reference
        # Prediction    0    1
        # 0  815   70
        # 1  130 1457

predictc502 <- predict(fitC502, testinggbm)

postResample(predictc502, testinggbm$brand)
      # Accuracy     Kappa 
      # 0.9190939 0.8265939 
#Without tunegrid      # Accuracy     Kappa 
      # 0.9215210 0.8326841 
cmC502 <- confusionMatrix(predictc502, testinggbm$brand)
fourfoldplot(cmC502$table)

print(importanceCf2 <- varImp(fitC502))
        # salary    100.00
        # age        84.12
        # car3        0.00
        # car19       0.00
        # zipcode1    0.00
  # Overall Commented tunegrid 
  # salary    100.00
  # age        85.21
  # car2       16.62
  # zipcode4   14.96
  # elevel1    13.61
  # zipcode2   10.91
  # car20       9.52
  # zipcode8    0.00
  # elevel4     0.00

# Auto tuning C5.0

set.seed(1516)
fitC503 <- train(brand~., data = traininggbm, method='C5.0')
summary(fitC503)
predictc503 <- predict(fitC503, testinggbm)
postResample(predictc503, testinggbm$brand)
      # Accuracy     Kappa 
      # 0.8576052 0.6991047 
confusionMatrix(predictc503, testinggbm$brand)
        # Confusion Matrix and Statistics
        # 
        # Reference
        # Prediction    0    1
        # 0  841   92
        # 1  104 1435

print(importanceCf3 <- varImp(fitC503))

        # Overall
        # salary    100.00
        # age       100.00
        # elevel3    69.97
        # car5       65.38
        # car6       65.30
        # car14      54.47
        # credit     30.27
        # car10      29.71
        # car8       19.58
        # car4       10.22
        # car19       0.00
        # car15       0.00
plot(importanceCf3)

# Auto with C5.0() function

fitC504 <- C5.0(brand~., data=traininggbm)
summary(fitC504)

predc504 <- predict(fitC504, testinggbm)
postResample(predc504, testinggbm$brand)
      #Accuracy     Kappa 
      # 0.8422330 0.6662187 
confusionMatrix(predictc503, testinggbm$brand)

##################################################################
### Predicting for Incomplete dataset using RF2 and fitC502 model
##################################################################

#Import dataset

SurveyIncomplete <- read_csv("SurveyData/SurveyIncomplete.csv")
head(SurveyIncomplete$brand)

#Converted all nominal columns to factor using readr
SurveyIncomplete$elevel<-as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)

head(SurveyIncomplete$brand)

attributes(SurveyIncomplete)
str(SurveyIncomplete)
summary(SurveyIncomplete) 

#Count 
table(SurveyIncomplete$elevel)
table(SurveyIncomplete$brand) 
table(SurveyIncomplete$car)
table(SurveyIncomplete$zipcode)

# Count of missing values in dataset - no missing data
sum(is.na(SurveyIncomplete))


# Predicting using RF2 Model 
predBrand1 <- predict(fitRF2, newdata=SurveyIncomplete)
postResample(predBrand1, SurveyIncomplete$brand)
      #### Accuracy      Kappa 
      #    0.39120000 0.01293983 
cmBrand1 <- confusionMatrix(predBrand1, SurveyIncomplete$brand)
cmBrand1
fourfoldplot(cmBrand1$table)

# Predicting usinf C5.0 - 2
predBrand2 <- predict(fitC502, newdata=SurveyIncomplete)
head(predBrand1, 30)
postResample(predBrand2, SurveyIncomplete$brand)
        # Accuracy      Kappa 
#         0.37760000 0.01151182 
cmBrand2 <- confusionMatrix(predBrand2, SurveyIncomplete$brand)
cmBrand2
fourfoldplot(cmBrand2$table)
summary1<-summary(predBrand1)
summary1
summary2<-summary(predBrand2)
summary2
outputDF <- data.frame(IncompleteSurveryData=SurveyIncomplete$brand, RFPrediction=predBrand1, C5.0Prediction=predBrand2)

#Create CSV for Predicted VS Ground Truth
write.csv(outputDF,file="SurveyData/OutputPreds.csv",row.names = TRUE)

# Plot Brand Preference
par(mfrow=c(4,1))
barplot(table(completeResponses$brand),main="Complete Survey Brand Preference",
        col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),
        xlab="Brand",
        ylab="Count")
legend("topleft", legend = c("Acer","Sony") , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) ) , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE)
barplot(table(SurveyIncomplete$brand),main="InComplete Survey Brand Preference",
        col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),
        xlab="Brand",
        ylab="Count")
legend("topright", legend = c("Acer","Sony") , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) ) , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE)
barplot(table(predBrand1),main=" Random Forest Prediction",
        col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),
        xlab="Brand",
        ylab="Count")
legend("topleft", legend = c("Acer","Sony") , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) ) , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE)
barplot(table(predBrand2),main="C5.0 Prediction", 
        col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),
        xlab="Brand",
        ylab="Count")
legend("topleft", legend = c("Acer","Sony") , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) ) , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE)
dev.off()

#### resamples() to create a summary for all models used.

results <- resamples(list(rf=fitRF2, C5.0=fitC502))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
####