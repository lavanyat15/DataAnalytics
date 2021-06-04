######################################################################################
## Topics covered in this code : 

## Visualization for Submeters by month and weekly.
## Subset 2 ways to form TS component 
## Method 1 - Subset for Month of January (2007,2008,2009) with frequency =52
## Method 2 - Subset 1st week in January 2008 , frequency = 48 (every half hour cycle)

## Build and forecast using TSLM model with subset data 
## Build and forecast HoltWinters model

#####################################################################################

# Imports 
library(RMySQL)       # RMySQL has data for this task
library(dplyr)        # Data munging
library(lubridate)    # Date/Time data wrangling package 

library(ggplot2)      # Plotting
library(plotly)
library("gridExtra")  # For grid.arrange()

library(forecast)   # To forecast model

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

# Only select tables with complete year information
yr2007 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2007")
yr2008 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2008")
yr2009 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2009")

####################################################################################
# Converted Date field using as.Date() from chr datatype
####################################################################################

yr2007$Date <- as.Date(yr2007$Date) 
yr2008$Date <- as.Date(yr2008$Date) 
yr2009$Date <- as.Date(yr2009$Date) 

####################################################################################
# Combining year 2007, 2008, 2009 to make one DF. 
# Combine Date and Time column
# Re-order Columns
# Convert DateTime from character to POSIXct 
# Add timezone to DateTime variable
####################################################################################

timeDf <- bind_rows(yr2007,yr2008,yr2009)
timeDf <-cbind(timeDf,DateTime=paste(timeDf$Date,timeDf$Time),stringsAsFactors=FALSE) 
timeDf <- timeDf[,c(6,1,2,3,4,5)]
timeDf$DateTime <- as.POSIXct(timeDf$DateTime,"%Y/%m/%d %H:%M:%S")
attr(timeDf$DateTime, "tzone") <- "Europe/London"

str(timeDf)
summary(timeDf)
#################################################################################
## Create year, quarter, month, week, weekday, day, hour and minute w/ Lubridate
#################################################################################

timeDf$year <- year(timeDf$DateTime)
timeDf$quarter <- quarter(timeDf$DateTime)
timeDf$month <- month(timeDf$DateTime)
timeDf$week <- week(timeDf$DateTime)
timeDf$weekday <- weekdays(timeDf$DateTime)
timeDf$day <- mday(timeDf$DateTime)
timeDf$hour <- hour(timeDf$DateTime)
timeDf$minute <- minute(timeDf$DateTime)

#################################################################################
## Line plot for energy usage separated by submeters - Too DENSE
## Adding granularity - Subsetting data every 45 mins
## Comparing  week 1 and week 2 in January 2008 for usage pattern

#################################################################################
house45 <- timeDf[seq(1,nrow(timeDf),45),]
housedatawk108 <- filter(house45, year == 2008 & month == 1 & week == 1)

Line_sm1 <- ggplot(housedatawk108) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm2 <- ggplot(housedatawk108) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm3 <- ggplot(housedatawk108) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

#plot_Line  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3)

#house45_2 <- timeDf[seq(1,nrow(timeDf),45),]
housedatawk208 <- filter(house45, year == 2008 & month == 1 & week ==2)

Line_sm1_wk2 <- ggplot(housedatawk208) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm2_wk2 <- ggplot(housedatawk208) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm3_wk2 <- ggplot(housedatawk208) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

#plot_Line  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3,Line_sm1_wk1,Line_sm2_wk2,Line_sm3_wk3)

housedatawk308 <- filter(house45, year == 2008 & month == 1 & week ==3)

Line_sm1_wk3 <- ggplot(housedatawk308) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm2_wk3 <- ggplot(housedatawk308) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm3_wk3 <- ggplot(housedatawk308) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

plot_Linek4508  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3,Line_sm1_wk2,Line_sm2_wk2,Line_sm3_wk2,
                                Line_sm1_wk3,Line_sm2_wk3,Line_sm3_wk3)

###################################################################################
## ## Comparing  week 1 ,week 2 and week 3 in January 2009 for usage pattern
###################################################################################

house4509 <- timeDf[seq(1,nrow(timeDf),45),]
housedatawk109 <- filter(house4509, year == 2009 & month == 1 & week == 1)

Line_sm109 <- ggplot(housedatawk109) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm209 <- ggplot(housedatawk109) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm309 <- ggplot(housedatawk109) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

#plot_Line  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3)

housedatawk209 <- filter(house4509, year == 2009 & month == 1 & week ==2)

Line_sm1_wk209 <- ggplot(housedatawk209) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm2_wk209 <- ggplot(housedatawk209) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm3_wk209 <- ggplot(housedatawk209) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

#plot_Line  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3,Line_sm1_wk1,Line_sm2_wk2,Line_sm3_wk3)

housedatawk309 <- filter(house4509, year == 2009 & month == 1 & week ==3)

Line_sm1_wk309 <- ggplot(housedatawk309) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm2_wk309 <- ggplot(housedatawk309) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm3_wk309 <- ggplot(housedatawk309) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

plot_Line4509  <- grid.arrange(Line_sm109,Line_sm209,Line_sm309,Line_sm1_wk209,Line_sm2_wk209,
                               Line_sm3_wk209,Line_sm1_wk309,Line_sm2_wk309,Line_sm3_wk309)

#########################################################################################################
# Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009 then plot usage
########################################################################################################
#house070809weekly <- filter(timeDf, weekday == 'Monday' & hour == 20 & minute == 1)
Line_sm1_070809weekly <- ggplot(house070809weekly) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm2_070809weekly <- ggplot(house070809weekly) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm3_070809weekly <- ggplot(house070809weekly) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

plot_Line070809weekly  <- grid.arrange(Line_sm1_070809weekly,Line_sm2_070809weekly,Line_sm3_070809weekly)

##################################################################################
#
## Time series Forecating stats HERE ! 
#
###################################################################
## TSLM Model 
###################################################################

## SET 1 ##

# Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009-SM3
house070809weekly <- filter(timeDf, weekday == 'Monday' & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
library(ggfortify)
autoplot(tsSM3_070809weekly)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'orange', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly , col=4)

#Decompose to plot its components
comp_SM3_070809weekly <- decompose(tsSM3_070809weekly)
summary(comp_SM3_070809weekly)
comp_SM3_070809weekly
plot(comp_SM3_070809weekly, col=2)

# Fit TSLM model for timeseries
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

# Residual standard error: 7.648 on 104 degrees of freedom
# Multiple R-squared:  0.3243

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3 <- forecast(fitSM3, h=20, level=c(80,90))
par
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time",main = "Forecast Submeter 3")

## Submeter 1 ###
## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, start=c(2007,1))

autoplot(tsSM1_070809weekly)
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM1_070809weekly , col=4)

#Decompose to plot its components
comp_SM1_070809weekly <- decompose(tsSM1_070809weekly)
summary(comp_SM1_070809weekly)
comp_SM1_070809weekly$trend
plot(comp_SM1_070809weekly, col=2)

# Fit TSLM model for timeseries
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)
# Residual standard error: 7.054 on 104 degrees of freedom
# Multiple R-squared:  0.422,	Adjusted R-squared:  0.1329 

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM1 <- forecast(fitSM1, h=40)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM1,ylab= "Watt-Hours", xlab="Time",main = "Forecast Submeter 1")

## Submeter 2
## Create TS object with SubMeter1
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, start=c(2007,1))

autoplot(tsSM2_070809weekly)
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM2_070809weekly , col=4)

#Decompose to plot its components
comp_SM2_070809weekly <- decompose(tsSM2_070809weekly)
summary(comp_SM2_070809weekly)
comp_SM2_070809weekly$trend
plot(comp_SM2_070809weekly, col=2)

# Fit TSLM model for timeseries
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)
# Residual standard error: 5.911 on 104 degrees of freedom
# Multiple R-squared:  0.3245,	Adjusted R-squared:  -0.01325 

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM2 <- forecast(fitSM2, h=50, level=c(80,90))
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM2,ylab= "Watt-Hours", xlab="Time",main = "Forecast Submeter 2")

##################################
## SET 2 subset for forecast
##  Subset Week 1 in January 2008
## Frequency every 30 mins
##################################

###############
# Submeter 1
###############

## Create TS object with SubMeter1
tsJanweek1_08_SM1 <- ts(Janweek1_08$Sub_metering_1, frequency=48)

## Plot sub-meter 3 with plot.ts
plot.ts(tsJanweek1_08_SM1 , col=4)

#Decompose to plot its components
comp_Janweek1_08_SM1 <- decompose(tsJanweek1_08_SM1)
summary(comp_Janweek1_08_SM1)
plot(comp_Janweek1_08_SM1, col=2)

# Fit TSLM model for timeseries
fitJanweek1_08_SM1 <- tslm(tsJanweek1_08_SM1 ~ trend + season) 
summary(fitJanweek1_08_SM1)

## Create the forecast for sub-meter 1. Forecast ahead 
forecastJanweek1_08_SM1 <- forecast(fitJanweek1_08_SM1, h=100)
## Plot the forecast for sub-meter 1. 
plot(forecastJanweek1_08_SM1)

###############
# Submeter 2
###############

## Create TS object with SubMeter2
tsJanweek1_08_SM2 <- ts(Janweek1_08$Sub_metering_2, frequency=48)

## Plot Timeseries with plot.ts
plot.ts(tsJanweek1_08_SM2 , col=4)

#Decompose to plot its components
comp_Janweek1_08_SM2 <- decompose(tsJanweek1_08_SM2)
summary(comp_Janweek1_08_SM2)
plot(comp_Janweek1_08_SM2, col=2)

# Fit TSLM model for timeseries
fitJanweek1_08_SM2 <- tslm(tsJanweek1_08_SM2 ~ trend + season) 
summary(fitJanweek1_08_SM2)

## Create the forecast for sub-meter 1. Forecast ahead 
forecastJanweek1_08_SM2 <- forecast(fitJanweek1_08_SM2, h=100, level=c(80,90))
## Plot the forecast 
plot(forecastJanweek1_08_SM2)


##############
## Submeter 3
##############
house30_08 <- timeDf[seq(1,nrow(timeDf),30),]
Janweek1_08 <- filter(house30_08, year == 2008,month == 1, week == 1) # Trying for every minute to see if plot is better
# Subsetting to select only submeter columns
Janweek1_08 <- subset (Janweek1_08, select=c(Sub_metering_1, Sub_metering_2, Sub_metering_3)) 

## Create TS object with SubMeter3
tsJanweek1_08_SM3 <- ts(Janweek1_08$Sub_metering_3, frequency=48)

## Plot sub-meter 3 with plot.ts
plot.ts(tsJanweek1_08_SM3 , col=4)

#Decompose to plot its components
comp_Janweek1_08_SM3 <- decompose(tsJanweek1_08_SM3)
summary(comp_Janweek1_08_SM3)
plot(comp_Janweek1_08_SM3, col=2)

# Fit TSLM model for timeseries
fitJanweek1_08_SM3 <- tslm(tsJanweek1_08_SM3 ~ trend + season) 
summary(fitJanweek1_08_SM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastJanweek1_08_SM3 <- forecast(fitJanweek1_08_SM3,level=c(80,90), h=100)

## Plot the forecast for sub-meter 3. 
plot(forecastJanweek1_08_SM3, ylim = c(0,20),ylab= "Watt-Hours", xlab="Time")

###########################
## Holt Winters Modeling
###########################
## Submeter 3

## Decompose to plot its components
comp_SM3_070809weekly <- decompose(tsSM3_070809weekly)
comp_SM3_070809weekly$seasonal
summary(comp_SM3_070809weekly)
plot(comp_SM3_070809weekly, col=2)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - comp_SM3_070809weekly$seasonal

## Plotting to see how the submeter readings behave with seasonally adjusted Vs Not adjusted data
plot(tsSM3_070809Adjusted,col='red') 
lines(tsSM3_070809weekly, col='blue')

plot(decompose(tsSM3_070809Adjusted),col=2)# Very small seasonal component present close to '0'

## Holt Winters Exponential Smoothing & Plot, Alpha=0.12, SSE = 8900
tsSM3_070809HW <- HoltWinters(tsSM3_070809Adjusted,beta=FALSE, gamma=FALSE)
tsSM3_070809HW$SSE
plot(tsSM3_070809HW, ylim = c(0, 25))

plot.ts(tsSM3_070809weekly, ylab='Time')
lines(tsSM3_070809HW$fitted[,1], lty=2, col="blue")

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_070809HW, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## HW predict function and plot
tsSM3_HW070809pred <- predict(tsSM3_070809HW, n.ahead=20, prediction.interval = T,level = 0.95)

plot(tsSM3_070809HW,tsSM3_HW070809pred)


## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_070809HW, h=20, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

##############
## Submeter 2
##############
#Decompose to plot its components
comp_SM2_070809weekly <- decompose(tsSM2_070809weekly)
summary(comp_SM2_070809weekly)
plot(comp_SM2_070809weekly, col=2)

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly - comp_SM2_070809weekly$seasonal
## Plotting to see how the submeter readings behave with seasonally adjusted Vs Not adjusted data
plot(tsSM2_070809Adjusted,col='red') 
lines(tsSM2_070809weekly, col='blue')

plot(decompose(tsSM2_070809Adjusted),col=2) # Very small seasonal component present close to '0'

## Holt Winters Exponential Smoothing & Plot, Alpha=0.02, SSE = 4892
# Low alpha, means more weight added to past onservations , hence smooth curve
tsSM2_070809HW <- HoltWinters(tsSM2_070809Adjusted,beta=FALSE, gamma=FALSE)
tsSM2_070809HW$SSE

plot(tsSM2_070809HW)

## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_070809HW, h=25)
plot(tsSM2_HW070809for, ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
# Predict using Predict() function
tsSM2_HW070809pred <- predict(tsSM2_070809HW, n.ahead=25, level = 0.95)
plot(tsSM2_070809HW,tsSM2_HW070809pred)

## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_070809HW, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

#############
## Submeter 1
##############
#Decompose to plot its components
comp_SM1_070809weekly <- decompose(tsSM1_070809weekly)
summary(comp_SM1_070809weekly)
plot(comp_SM1_070809weekly, col=2)

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly - comp_SM1_070809weekly$seasonal
## Plotting to see how the submeter readings behave with seasonally adjusted Vs Not adjusted data
plot(tsSM1_070809Adjusted,col='red') 
lines(tsSM1_070809weekly, col='blue')

plot(decompose(tsSM1_070809Adjusted),col=2) # Very small seasonal component present close to '0'


## Holt Winters Exponential Smoothing & Plot, Alpha= 0.006, SSE= 6737
tsSM1_070809HW <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
tsSM1_070809HW$SSE

plot(tsSM1_070809HW, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_070809HW, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
# Predict using Predict() function
tsSM1_HW070809pred <- predict(tsSM1_070809HW, n.ahead=25, level = 0.95)
plot(tsSM1_070809HW,tsSM1_HW070809pred)

## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_070809HW, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

decomp_complete <- cbind(Submeter1 = summary(comp_SM1_070809weekly),Submeter2 = summary(comp_SM2_070809weekly),Submeter3 = summary(comp_SM3_070809weekly))
View(decomp_complete)

##################   Weekly Time frame prediction / Forecast #################

####################################################################  
# Submeter 3 
### SSE for submeter 3 is higher than time frame used for previous model
### Alpha value is close to 1, placing more weightage on recent observations
### meaning model adjusts to future values better. 

################################################################### 

comp_SM3_Janweek1_08 <- decompose(tsJanweek1_08_SM3)
summary(comp_SM3_Janweek1_08)
plot(comp_SM3_Janweek1_08, col=2)

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM3_Janweek1_08Adjusted <- tsJanweek1_08_SM3 - comp_SM3_Janweek1_08$seasonal
plot(tsSM3_Janweek1_08Adjusted,col='red') 
lines(tsJanweek1_08_SM3, col='blue')

plot(decompose(tsSM3_Janweek1_08Adjusted),col=2) # Very small seasonal component present close to '0'

# Alpha = 0.6 ( Takes more recent data for predictions) , SSE = 13609
tsSM3_Janweek1_08HW <- HoltWinters(tsSM3_Janweek1_08Adjusted,beta=FALSE, gamma=FALSE)
tsSM3_Janweek1_08HW$SSE

plot(tsSM3_Janweek1_08HW, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_Janweek1_08HWFor <- forecast(tsSM3_Janweek1_08HW, h=25)
plot(tsSM3_Janweek1_08HWFor, ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
# Predict using Predict() function
tsSM3_Janweek1_08HWpred <- predict(tsSM3_Janweek1_08HW, n.ahead=100, level = 0.95)
plot(tsSM3_Janweek1_08HW,tsSM3_Janweek1_08HWpred)


###########
# Submeter 2
###########
comp_SM2_Janweek1_08 <- decompose(tsJanweek1_08_SM2)
summary(comp_SM2_Janweek1_08)
plot(comp_SM2_Janweek1_08, col=2)

## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_Janweek1_08Adjusted <- tsJanweek1_08_SM2 - comp_SM2_Janweek1_08$seasonal
plot(tsSM2_Janweek1_08Adjusted,col='red') 
lines(tsJanweek1_08_SM2, col='blue')

plot(decompose(tsSM2_Janweek1_08Adjusted),col=2) # Very small seasonal component present close to '0'

# Alpha = 0.088 ( Takes past data for predictions) , SSE = 8068, level = -0.5
tsSM2_Janweek1_08HW <- HoltWinters(tsSM2_Janweek1_08Adjusted,beta=FALSE, gamma=FALSE)
tsSM2_Janweek1_08HW$SSE

plot(tsSM2_Janweek1_08HW, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM2_Janweek1_08HWFor <- forecast(tsSM2_Janweek1_08HW, h=25)
plot(tsSM2_Janweek1_08HWFor, ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
# Predict using Predict() function
tsSM2_Janweek1_08HWpred <- predict(tsSM2_Janweek1_08HW, n.ahead=25, level = 0.95)
plot(tsSM2_Janweek1_08HW,tsSM2_Janweek1_08HWpred)




