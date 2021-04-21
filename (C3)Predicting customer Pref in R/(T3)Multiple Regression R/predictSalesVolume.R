########################################################
# Multiple Regression for R - Predict volume of sales 
########################################################

#Imports
library(caret)
library(readr)
library(dbplyr)
library(tidyverse)
library(GGally)
library(e1071) #SVM

# Import dataset
productData <- read.csv("productattributes/existingproductattributes2017.csv")

productData$ProductType <- as.factor(productData$ProductType)
str(productData)
hist(productData$Volume)

#############################################
#    Plot to show which product sells more
#############################################

# ProductType ~ Volume sold 
ggplot(productData) +
  geom_boxplot(aes(x = ProductType, y = Volume,
                   fill = ProductType)) +
  labs(x = 'Product Type') +
  ggtitle("Product~Volume sold") +
  
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#############################################
#Dummify data
#############################################

newDataFrame <- dummyVars(" ~ .", data = productData)

readyData <- data.frame(predict(newDataFrame, newdata = productData))

#Rename columns
names(readyData)[1] <- "Product-Accessories"
names(readyData)[2] <- "Product-Display"
names(readyData)[3] <- "Product-ExtendedWarranty"
names(readyData)[4] <- "Product-GameConsole"
names(readyData)[5] <- "Product-Laptop"
names(readyData)[6] <- "Product-Netbook"
names(readyData)[7] <- "Product-PC"
names(readyData)[8] <- "Product-Printer"
names(readyData)[9] <- "Product-PrinterSupplies"
names(readyData)[10] <- "Product-Smartphone"
names(readyData)[11] <- "Product-Software"
names(readyData)[12] <- "Product-Tablet"
colnames(readyData)

str(readyData)
summary(readyData) # - NAs in BestSellersRank

# Drop NAs - BestSellersRank has very low correlation hence dropping column
# readyData <- na.omit(readyData)
readyData$BestSellersRank <- NULL
summary(readyData)
names(readyData)

#############################################
#     Correlation and plotting for EDA   
#############################################

#Correlation
corrData <- cor(readyData[,1:28])
corrData
highlyCorrelated <- findCorrelation(corrData, cutoff=0.5)
# print indexes of highly correlated attributes
highlyCorrelated 

library(corrplot)
corrplot(corrData)

#############################################
# Plot to see correlation between X5 reviews 
# and volume as shown by corrplot
#############################################

# Volume~X5StarReview - Plot Shows Very linear relationship - High correlation
plot(readyData$x5StarReviews,readyData$Volume)
lines(lowess(readyData$x5StarReviews,readyData$Volume), col="blue")

# Volume~X4Star Review - Mostly high correlation with few outliers 
plot(readyData$x4StarReviews,readyData$Volume)
lines(lowess(readyData$x4StarReviews,readyData$Volume), col="blue")

# Volume~NegativeServiceReview 
plot(readyData$NegativeServiceReview,readyData$Volume)
lines(lowess(readyData$NegativeServiceReview,readyData$Volume), col="blue")

#############################################
#     Subsetting data for pair plot 
#############################################

readyData2 <- readyData %>% 
  subset(select = c("Volume", "ProfitMargin", "PositiveServiceReview", 
                    "x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews",
                    "NegativeServiceReview"))
# glimpse(readyData2)
# head(readyData2)
ggpairs(readyData2) + theme_bw()

#############################################
#      Modelling 
#############################################

#Create train and test sets

trainSize<-round(nrow(readyData)*0.8) 
testSize<-nrow(readyData)-trainSize

# Actually CREATE the sets in ransomized order

set.seed(2469)
training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)

trainSet<-readyData[training_indices,]

testSet<-readyData[-training_indices,] 

#############################################
# Linear regression Model 
#############################################
# Warning message:
#   In summary.lm(sales.regression) :
#   essentially perfect fit: summary may be unreliable
#   Linear model has perfect score because X5star reviews has perfect linear relationship with Volume.
#   Model takes just that one variable and makes a perfect prediction hence unreliable
############################################
# Training Model
sales.regression <- lm(Volume~.-ProductNum-ProductWidth-ProductHeight, trainSet)

# Prints summary statistics of model . perfect fit: summary may be unreliable
# Model performs perfectly, which might indicate overfitting or wrong model choice
summary(sales.regression)

# Residual standard error: 7.975e-14 on 22 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 8.818e+32 on 23 and 22 DF,  p-value: < 2.2e-16

set.seed(2469)
predlm1 = round(predict(sales.regression, testSet),0)
predlm1 # Negative values in Predictions - there should not be negative values for Volume
postResample(predlm1, testSet$Volume)

cbind(ProductNum=testSet$ProductNum,Actual=testSet$Volume,PredVolume=predlm1)
#Residual
reslm<-resid(sales.regression)

#produce residual vs. fitted plot
plot(fitted(sales.regression), reslm)

#add a horizontal line at 0 
abline(0,0)
#############################################
## SVM Regression
#############################################

# SVM 1 
# Low R2 but No negative values in Predictions - there should not be negative values for Volume

set.seed(2469)
modelsvm1 = svm(Volume~.-ProductNum-ProductWidth-ProductHeight, trainSet)
summary(modelsvm1)

##### Varimp does not work for SVM ########
#library(rminer)
# varImp(modelsvm1) -- Why cant varImp be used in svm() above
#svm.imp <- Importance(modelsvm1, data=trainSet) 
#print(svm.imp)

########################################################

#Predict
set.seed(2469)
predsvm1 = round(predict(modelsvm1, testSet),0)
predsvm1 # No negative values in Predictions - there should not be negative values for Volume
postResample(predsvm1, testSet$Volume)

data.frame(R2=R2(predsvm1,testSet$Volume),
           RMSE=RMSE(predsvm1,testSet$Volume),
           MAE=MAE(predsvm1,testSet$Volume))
        # RMSE     Rsquared          MAE 
        # 1501.3813492    0.5302915  669.5259542 
        
#######
#SVM 2
#######
  # R2 is 67% for svmLinear but :- Lots of negative values in preds which is incorrect 
  # R2 is 62% for svmRadialSigma but no negative values for predictions

set.seed(2469)
fitControlsvm <- trainControl(method="cv",number = 5, savePred=T)
set.seed(2469)
modelsvm2 <- train(Volume~.-ProductNum-ProductWidth-ProductHeight, method = "svmRadialSigma",trainSet,trControl=fitControlsvm)
summary(modelsvm2)
modelsvm2$bestTune

plot(varImp(modelsvm2))
#Prints predictions for each folds [ set savePred=T in fitControl ]
head(modelsvm2$pred)
set.seed(2469)
predsvm2 <- round(predict(modelsvm2, testSet),0)
predsvm2  
postResample(predsvm2, testSet$Volume)
        # method="svmLinear"
        #  RMSE    Rsquared         MAE 
        # 281.9665822   0.9694257 155.4902963 
        # method="svmRadialSigma
        # RMSE     Rsquared          MAE 
        # 1257.2137407    0.9188803  544.3368760 

#############################################
# SVM 3
# Polynomial gave the best R2 value - 87% - Bad residual plot
# Sigmoid - 74% , Good residual plot *** Good for predictions with newproducts dataset ****
# Radial - 50% , Good residual plot
# linear - 99% - Not good
#############################################

set.seed(2469)
modelsvm3 <- svm(Volume~.-ProductNum-ProductWidth-ProductHeight-ProfitMargin,data=trainSet,type="eps-regression",kernel="radial",cross=10)
summary(modelsvm3)
set.seed(2469)
predsvm3 <- round(predict(modelsvm3, testSet),0)
predsvm3  
postResample(predsvm3, testSet$Volume)
        # RMSE    Rsquared         MAE 
        # 1060.470286    0.873362  612.131347 

cbind(ProductNum=testSet$ProductNum,Actual=testSet$Volume,PredVolume=predsvm3)
#Residual
ressvm3<-resid(modelsvm3)

#produce residual vs. fitted plot
plot(fitted(modelsvm3), ressvm3)

#add a horizontal line at 0 
abline(0,0)
#############################################
# Random Forest 
#############################################
# RF1

fitControlRF1 <- trainControl(method="repeatedcv", number=15, repeats=3,savePred=T)
set.seed(2469)
modelrf1 <- train(Volume~.-ProductNum-ProductWidth-ProductHeight-ProfitMargin,
                  data=trainSet,method='rf',trControl=fitControlRF1)
modelrf1
        # mtry  RMSE      Rsquared   MAE     
        # 2    588.8772  0.8777100  370.4525
        # 12    493.9762  0.9602970  269.5887
        # 23    468.0872  0.9726869  251.6388
# Variable importance 
imprf1 <- varImp(modelrf1)
plot(imprf1)

#Residual
resrf1<-resid(modelrf1)
resrf1

#produce residual vs. fitted plot
plot(fitted(modelrf1), resrf1)

#add a horizontal line at 0 - All points close to or on 0 line. Means good predictions
abline(0,0)

set.seed(2469)
predrf1 <- round(predict(modelrf1, testSet),0)
View(predrf1) # Opens as a new tab
postResample(predrf1, testSet$Volume)
        # RMSE    Rsquared         MAE 
        # 900.1973010   0.9053662 247.2636750 

#View of Actual vs Predicted Values - Predictions close to actual values
cbind(ProductNum=testSet$ProductNum,Actual=testSet$Volume,PredVolume=predrf1)

#############################################
# GBM
#############################################
#GBM 1

library(gbm)
set.seed(2469)
modelgbm1 <- gbm(Volume~.-ProductNum-ProductWidth-ProductHeight-ProfitMargin,data=trainSet,
                 cv.folds=15,shrinkage=0.1, verbose=F, n.trees = 5000)

summary(modelgbm1)

#########
#  There were 23 predictors of which 10 had non-zero influence.
        # var   rel.inf
        # ProductDepth                             ProductDepth 22.349093
        # Price                                           Price 17.138554
        # x5StarReviews                           x5StarReviews 15.298467
        # ShippingWeight                         ShippingWeight 10.569259
        # NegativeServiceReview           NegativeServiceReview  6.946379
        # PositiveServiceReview           PositiveServiceReview  6.183626
        # x2StarReviews                           x2StarReviews  5.259119
        # x3StarReviews                           x3StarReviews  3.931805
        # `Product-Accessories`           `Product-Accessories`  3.570631
        # x4StarReviews                           x4StarReviews  3.033699
        # x1StarReviews                           x1StarReviews  3.002788
        # Recommendproduct                     Recommendproduct  2.716581
        # `Product-Display`                   `Product-Display`  0.000000
        # `Product-ExtendedWarranty` `Product-ExtendedWarranty`  0.000000
        # `Product-GameConsole`           `Product-GameConsole`  0.000000
        # `Product-Laptop`                     `Product-Laptop`  0.000000
        # `Product-Netbook`                   `Product-Netbook`  0.000000
        # `Product-PC`                             `Product-PC`  0.000000
        # `Product-Printer`                   `Product-Printer`  0.000000
        # `Product-PrinterSupplies`   `Product-PrinterSupplies`  0.000000
        # `Product-Smartphone`             `Product-Smartphone`  0.000000
        # `Product-Software`                 `Product-Software`  0.000000
        # `Product-Tablet`                     `Product-Tablet`  0.000000
#######
# Predict 
set.seed(2469)
predgbm1 <- round(predict(modelgbm1, testSet),0)

postResample(predgbm1, testSet$Volume)
        # RMSE     Rsquared          MAE 
        # 1341.7060945    0.3908937  555.9477731 

# GBM 2
set.seed(2469)
modelgbm2 <- train(Volume~.-ProductNum-ProductWidth-ProductHeight-ProfitMargin,
                  data=trainSet,method='gbm',
                  trControl=trainControl(method="repeatedcv",number=15,repeats=2),
                  tuneGrid=expand.grid(shrinkage=0.01, interaction.depth=3, n.trees = 5000,n.minobsinnode=4))
summary(modelgbm2)

#Residual
resgbm2<-resid(modelgbm2)
resgbm2
#produce residual vs. fitted plot
# Points close to zero with some outliers
plot(fitted(modelgbm2), resgbm2)

#add a horizontal line at 0 - All points close to or on 0 line. Means good predictions
abline(0,0)

# Predict 
set.seed(2469)
predgbm2 <- round(predict(modelgbm2, testSet),0)

postResample(predgbm2, testSet$Volume)
        # RMSE     Rsquared          MAE 
        # 1569.5003675    0.4435663  793.0829954 
impgbm <- varImp(modelgbm2)

# XGBoost 
library(xgboost)

# With default hyperparams
set.seed(2469)
grid_default <- expand.grid(
                nrounds = 100,
                max_depth = 6,
                eta = 0.3,
                gamma = 0,
                colsample_bytree = 1,
                min_child_weight = 1,
                subsample = 1
              )
#view(grid_default)
set.seed(2469)
train_control <- trainControl(
                  method="repeatedcv", 
                  number=10, 
                  repeats=3,
                  verboseIter = FALSE, # no training log
                  #allowParallel = TRUE # FALSE for reproducible results 
                )
set.seed(2469)
modelxgb1 <- train(Volume~.-ProductNum-ProductWidth-ProductHeight-ProfitMargin,
                    data=trainSet,
                    trControl = train_control,
                    tuneGrid = grid_default,
                    method = "xgbTree",
                    verbose = TRUE
                  )
summary(modelxgb1)
#Find best tuned tree
modelxgb1$bestTune

varImp(modelxgb1)

# Predict 
set.seed(2469)
predxgb1 <- round(predict(modelxgb1, testSet),0)
view(postResample(predxgb1, testSet$Volume)) # Shows a table of values
        # RMSE    Rsquared         MAE 
        # 1042.331036    0.987071  277.411362
view(predxgb1)

# Residual
resxgb1<-resid(modelxgb1)
resxgb1

#produce residual vs. fitted plot
plot(fitted(modelxgb1), resxgb1)

#add a horizontal line at 0 
abline(0,0)

#View of Actual vs Predicted Values - Predictions close to actual values
cbind(ProductNum=testSet$ProductNum,Actual=testSet$Volume,PredVolume=predxgb1)


#XGB 2
set.seed(2469)
tune_grid <- expand.grid(
                  nrounds = seq(from = 200, to = nrounds, by = 50),
                  eta = c(0.025, 0.05, 0.1, 0.3),
                  max_depth = c(2, 3, 4, 5, 6),
                  gamma = 0,
                  colsample_bytree = 1,
                  min_child_weight = 1,
                  subsample = 1
                )
set.seed(2469)
train_control <- trainControl(
                  method="repeatedcv", 
                  number=10, 
                  repeats=3,
                  verboseIter = FALSE, # no training log
                  #allowParallel = TRUE # FALSE for reproducible results 
                )
set.seed(2469)
modelxgb2 <- train(Volume~.-ProductNum-ProductWidth-ProductHeight-ProfitMargin,
                   data=trainSet,
                   trControl = train_control,
                   tuneGrid = grid_default,
                   method = "xgbTree",
                   verbose = TRUE
                )
summary(modelxgb2)

#Find best tuned tree
modelxgb2$bestTune

# Predict 
set.seed(2469)
predxgb2 <- round(predict(modelxgb2, testSet),0)
postResample(predxgb2, testSet$Volume)
        # RMSE    Rsquared         MAE 
        # 1042.331036    0.987071  277.411362 

# Residual
resxgb2<-resid(modelxgb2)
resxgb2

#produce residual vs. fitted plot
plot(fitted(modelxgb2), resxgb2)

#add a horizontal line at 0 
abline(0,0)

#View of Actual vs Predicted Values - Predictions close to actual values
cbind(ProductNum=testSet$ProductNum,Actual=testSet$Volume,PredVolume=predxgb2)

################################################
# Predict for New Products attribute dataset
# Using SVM3, RF1, GBM2, XGB1
################################################

# Import newproducts dataset
newProductData <- read_csv("productattributes/newproductattributes2017.csv")
view(newProductData)

summary(newProductData)
sum(is.na(newProductData))

newProductData$BestSellersRank <- NULL
str(newProductData)
newProductData$ProductType <- as.factor(newProductData$ProductType)
str(newProductData)

#############################################
# Dummify data
#############################################

newDataFrame2 <- dummyVars(" ~ .", data = newProductData)

readyData2 <- data.frame(predict(newDataFrame2, newdata = newProductData))

#Rename columns
names(readyData2)[1] <- "Product-Accessories"
names(readyData2)[2] <- "Product-Display"
names(readyData2)[3] <- "Product-ExtendedWarranty"
names(readyData2)[4] <- "Product-GameConsole"
names(readyData2)[5] <- "Product-Laptop"
names(readyData2)[6] <- "Product-Netbook"
names(readyData2)[7] <- "Product-PC"
names(readyData2)[8] <- "Product-Printer"
names(readyData2)[9] <- "Product-PrinterSupplies"
names(readyData2)[10] <- "Product-Smartphone"
names(readyData2)[11] <- "Product-Software"
names(readyData2)[12] <- "Product-Tablet"
colnames(readyData2)

str(readyData2)
summary(readyData2)
names(readyData2)

#############################################
# Make Volume predictions
#############################################
#lm - Can this be reliable ???
set.seed(2469)
predprodvollm <- round(predict(sales.regression, newdata=readyData2),0)
predprodvollm
cbind(ProductNum=readyData2$ProductNum,PredictedVolume=predprodvollm)

# SVM3
######################################################
# Negative values for - Not reliable predictions
####################################################
set.seed(2469)
predprodvolsvm <- round(predict(modelsvm3, newdata=readyData2),0)
predprodvolsvm
cbind(ProductNum=readyData2$ProductNum,PredictedVolume=predprodvolsvm)

# RF1 
######################################################
# From predictions and residual plot for actual model 
#This seems to be reliable in  predictions.
####################################################
set.seed(2469)
predprodvolrf <- round(predict(modelrf1, newdata=readyData2),0)
predprodvolrf
cbind(ProductNum=readyData2$ProductNum,PredictedVolume=predprodvolrf)

# Writing predictions to an external file
output <- readyData2
names(output)
# Drop columns that are not PC, Laptop,Notbook smartphone
output$`Product-Accessories`<- NULL
output$`Product-Display`<- NULL
output$`Product-ExtendedWarranty`<- NULL
output$`Product-GameConsole`<- NULL
output$`Product-Printer`<- NULL
output$`Product-PrinterSupplies`<- NULL
output$`Product-Software`<- NULL
output$`Product-Tablet`<- NULL

output$predictions <- predprodvolrf
write.csv(output, file="C3T3output.csv", row.names = TRUE)

# GBM2 
# Residual Vs Fitted Plot has points close to zero in the model above (BUT) 
# Predictions have negative Negative values so predictions are unreliable for this model
##########
set.seed(2469)
predprodvolgbm <- round(predict(modelgbm2, newdata=readyData2),0)
predprodvolgbm
cbind(ProductNum=readyData2$ProductNum,PredictedVolume=predprodvolgbm)

# XGB1

# Predictions have negative Negative values so predictions are unreliable for this model
##################
set.seed(2469)
predprodvolxgb1 <- round(predict(modelxgb1, newdata=readyData2),0)
predprodvolxgb1
cbind(ProductNum=readyData2$ProductNum,PredictedVolume=predprodvolxgb1)



