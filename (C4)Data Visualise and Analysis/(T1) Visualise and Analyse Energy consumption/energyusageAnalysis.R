######################################################################################
## Topics covered in this code : 

## Reading data in from mysql DB, cleaning data, filtering data to find patterns in energy usage
## Performed EDA to understand usage during the month in question - If client was home or not 
## Visualization for Submeters by month and weekly.
## Granularity added to line plots to understand energy usage

#####################################################################################

# Imports 
library(RMySQL)       # RMySQL has data for this task
library(dplyr)        # Data munging
library(lubridate)    # Date/Time data wrangling package 

library(ggplot2)      # Plotting
library(plotly)
library("gridExtra")  # For grid.arrange()


## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#Using the dbListFields function learn the attributes associated with the yr_2006 table.
dbListFields(con,'yr_2006')

# Select Date, Time and submeter columns from all tables year 2006 - year 2010
yr2006 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2006")
yr2007 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2007")
yr2008 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2008")
yr2009 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2009")
yr2010 <- dbGetQuery(con,"SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2010")

####################################################################################
# Observations for 2006 DF 
# 1. Converted Date field using as.Date() since it was chr datatype
# 2. Date Column has observations for the month of "December 16 - 31" only
####################################################################################

str(yr2006)
yr2006$Date <- as.Date(yr2006$Date)
summary(yr2006)

head(yr2006)
tail(yr2006,20)

####################################################################################
# Observations for 2007 DF 
# 1. Converted Date field using as.Date() since it was chr datatype
# 2. Date Column has observations for the month of "January 1 - December 31"
####################################################################################

str(yr2007)
yr2007$Date <- as.Date(yr2007$Date)
summary(yr2007)

head(yr2007)
tail(yr2007)

####################################################################################
# Observations for 2008 DF 
# 1. Converted Date field using as.Date() since it was chr datatype
# 2. Date Column has observations for the month of "January 1 - December 31"
####################################################################################

str(yr2008)
yr2008$Date <- as.Date(yr2008$Date)
summary(yr2008)

head(yr2008)
tail(yr2008)

####################################################################################
# Observations for 2009 DF 
# 1. Converted Date field using as.Date() since it was chr datatype
# 2. Date Column has observations for the month of "January 1 - December 31"
####################################################################################

str(yr2009)
yr2009$Date <- as.Date(yr2009$Date)
summary(yr2009)

head(yr2009)
tail(yr2009)

####################################################################################
# Observations for 2010 DF 
# 1. Converted Date field using as.Date() since it was chr datatype
# 2. Date Column has observations for the month of "January 1 - November 26" only
####################################################################################

str(yr2010)
yr2010$Date <- as.Date(yr2010$Date)
summary(yr2010)

head(yr2010)
tail(yr2010)

####################################################################################
# Combining year 2007, 2008, 2009 to make one DF. 
# 2006 and 2010 have been ommited due to lack of data for entire year. 
####################################################################################

energyInfoDF <- bind_rows(yr2007,yr2008,yr2009)
str(energyInfoDF)
summary(energyInfoDF)
head(energyInfoDF)
tail(energyInfoDF)

## Combine Date and Time attribute values in a new attribute column
####################################################################################
##  Alternate way to change column name in R -> give "name" before paste()
## energyInfoDF <-cbind(energyInfoDF,DateTime=paste(energyInfoDF$Date,energyInfoDF$Time) 
####################################################################################

energyInfoDF <-cbind(energyInfoDF,paste(energyInfoDF$Date,energyInfoDF$Time), 
                      stringsAsFactors=FALSE)
# energyInfoDF$DateTime <- NULL

# Give the new attribute in the 6th column a header name 
colnames(energyInfoDF)[6] <-"DateTime"

####################################################################################
## REORDER COLUMNS
## energyInfoDF <- energyInfoDF[,c(ncol(energyInfoDF), 1:(ncol(energyInfoDF)-1))]
##                               c(6                 , 1:maxnumberofcolumns-1)
##                    [OR]
##                               c(6,1,2,3,4,5)
####################################################################################
# Move the DateTime attribute within the dataset
energyInfoDF <- energyInfoDF[,c(ncol(energyInfoDF), 1:(ncol(energyInfoDF)-1))]
head(energyInfoDF)

## Convert DateTime from character to POSIXct 
energyInfoDF$DateTime <- as.POSIXct(energyInfoDF$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone :  attr() -> will get or set specific attributes of an object

grep("London", OlsonNames(), value=TRUE) # To find true value of tz for London due to Warning
attr(energyInfoDF$DateTime, "tzone") <- "Europe/London"

## Inspect the data types
str(energyInfoDF)

#################################################################################
## Create year, quarter, month, week, weekday, day, hour and minute w/ Lubridate
#################################################################################

energyInfoDF$year <- year(energyInfoDF$DateTime)
energyInfoDF$quarter <- quarter(energyInfoDF$DateTime)
energyInfoDF$month <- month(energyInfoDF$DateTime)
energyInfoDF$week <- week(energyInfoDF$DateTime)
energyInfoDF$weekday <- weekdays(energyInfoDF$DateTime)
energyInfoDF$day <- mday(energyInfoDF$DateTime)
energyInfoDF$hour <- hour(energyInfoDF$DateTime)
energyInfoDF$minute <- minute(energyInfoDF$DateTime)

table(energyInfoDF$quarter)
table(energyInfoDF$month)
table(energyInfoDF$week)
table(energyInfoDF$weekday)
table(energyInfoDF$hour)
table(energyInfoDF$minute)

##########################################
## year 2010 added maybe due to adding timezone in the step above 
## Changed tz to London to fix 2010 issue 

# 2007   2008   2009      2010 
# 521609 526905 521320     60 
##########################################
table(energyInfoDF$year)

#################################################################################
### Which submeter uses more power 
# sm1     sm2     sm3
# 1819989 2108410 9758843
##  Submeter 3 Uses more power, Submeter 1 uses least
#################################################################################
summary(energyInfoDF$Sub_metering_1)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 0.000   0.000   0.000   1.159   0.000  82.000 
sd(energyInfoDF$Sub_metering_1) #  6.288272
summary(energyInfoDF$Sub_metering_2)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 0.000   0.000   0.000   1.343   1.000  78.000 
sd(energyInfoDF$Sub_metering_2) # 5.972199
summary(energyInfoDF$Sub_metering_3)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 0.000   0.000   1.000   6.216  17.000  31.000 
sd(energyInfoDF$Sub_metering_3) # 8.341281

sm1<-sum(energyInfoDF$Sub_metering_1)
sm2<-sum(energyInfoDF$Sub_metering_2)
sm3<-sum(energyInfoDF$Sub_metering_3)
cbind(sm1,sm2,sm3)

#################################################################################
## Plotting to compare Submeter power usage
#################################################################################

x <- energyInfoDF[,c(4,5,6)] # Subsetting only the columns we need (All submeter readings)
library(reshape2)
data<- melt(x)
# Density Plot
dp<-ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)
dp

#Histogram
ggplot(energyInfoDF, aes(x=Sub_metering_1)) + 
  geom_histogram(binwidth=5,fill="#69b3a2", color="#e9ecef", alpha=0.9)

ggplot(energyInfoDF, aes(x=Sub_metering_2)) + 
  geom_histogram(binwidth=5,fill="#69b3a2", color="#e9ecef", alpha=0.9)

ggplot(energyInfoDF, aes(x=Sub_metering_3)) + 
  geom_histogram(binwidth=5,fill="#69b3a2", color="#e9ecef", alpha=0.9)

# ggplot(data=energyInfoDF, aes(x=Sub_metering_1)) +geom_density(adjust=1.5)
##########################################################
## Usage Grouped by month and year
## Usage seems more on submeter #3
## Same as inferred from density and Box plot above
##########################################################
Yr_usage_sub <-  energyInfoDF%>% 
  group_by(year) %>% 
  summarise(sub1_avg=mean(Sub_metering_1), sub2_avg= mean(Sub_metering_2),sub3_avg= mean(Sub_metering_3)) %>% 
  ungroup() %>% 
  arrange(year)
head(Yr_usage_sub)


# PieChart to plot usage averaged each year on each submeter

pct <- round(Yr_usage_sub$sub1_avg/sum(Yr_usage_sub$sub1_avg)*100)
lbls <- Yr_usage_sub$year
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add '%' to labels

par(mfrow = c(1,3)) # Display all piecharts in 1 row 3 columns

pie(Yr_usage_sub$sub1_avg, labels=lbls, main="Pie Chart of Submeter 1")
pie(Yr_usage_sub$sub2_avg, labels=lbls, main="Pie Chart of Submeter 2")
pie(Yr_usage_sub$sub3_avg, labels=lbls, main="Pie Chart of Submeter 3")

dev.off() # reset par

Mon_usage_sub <-  energyInfoDF%>% 
  group_by(month,year) %>% 
  summarise(sub1_avg=mean(Sub_metering_1), sub2_avg= mean(Sub_metering_2),sub3_avg= mean(Sub_metering_3)) %>% 
  ungroup() %>% 
  arrange(year)
View(Mon_usage_sub)

#################################################################################
# Plotting average usage for submeters
# There is a dip in power usage in all Submeters for month 8 in year 2008
#################################################################################
Plot_mon_sm1 <-  ggplot(Mon_usage_sub,
                        aes(x=factor(month),y=sub1_avg,group=factor(year),color=factor(year))) +
  geom_line(size=1) +geom_point() + xlab("Months") + ylab("Montly Usage by Submeter 1") 
Plot_mon_sm2 <-  ggplot(Mon_usage_sub,
                        aes(x=factor(month),y=sub2_avg,group=factor(year),color=factor(year))) +
  geom_line(size=1) +geom_point() + xlab("Months") + ylab("Montly Usage by Submeter 2") 
Plot_mon_sm3 <-  ggplot(Mon_usage_sub,
                        aes(x=factor(month),y=sub3_avg,group=factor(year),color=factor(year))) +
  geom_line(size=1) +geom_point() + xlab("Months") + ylab("Montly Usage by Submeter 3") 

## Multiple Plots in One Plot with grid.arrange()
plot_mon  <- grid.arrange(Plot_mon_sm1,Plot_mon_sm2,Plot_mon_sm3)

#################################################################################
## Line plot for energy usage in all submeters - Too DENSE
#################################################################################

Plot_sm1 <- ggplot(energyInfoDF) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter 1 Vs DateTime")
Plot_sm2 <- ggplot(energyInfoDF) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter 2 Vs DateTime")
Plot_sm3 <- ggplot(energyInfoDF) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter 3 Vs DateTime")

plot_G  <- grid.arrange(Plot_sm1,Plot_sm2,Plot_sm3)

#################################################################
## Subsetting data and adding granularity from here on 
## Using Plotly 
#################################################################

## Plot all of sub-meter 1 
## Way too crowded
plot(energyInfoDF$Sub_metering_1)

## Subset the second week of 2008 - All Observations
## 
houseWeek2 <- filter(energyInfoDF, year == 2008 & week == 2)
View(houseWeek2)
## Plot subset houseWeek
plot(houseWeek2$Sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay9 <- filter(houseWeek2, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay9, x = ~houseDay9$DateTime, y = ~houseDay9$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay9, x = ~houseDay9$DateTime, y = ~houseDay9$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay9$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay9$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 15 Minute frequency
houseDay15 <- filter(houseDay9, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay15, x = ~houseDay15$DateTime, y = ~houseDay15$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~houseDay15$Sub_metering_2, name = 'Laundry Room', mode = 'lines+markers') %>%
  add_trace(y = ~houseDay15$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines+markers') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

###################################################
# Plotting for August 2008 by each week 
###################################################
houseweek31 <- filter(energyInfoDF, year == 2008 & month == 8 & week == 31 & (minute == 0 | minute == 15 | minute == 30 | minute == 45) )

Ph1<- plot_ly(houseweek31, x = ~houseweek31$DateTime, y= ~houseweek31$Sub_metering_1, 
        name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek31$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek31$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption Summer, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


houseweek32 <- filter(energyInfoDF, year == 2008 & month == 8 & week == 32 & (minute == 0 | minute == 15 | minute == 30 | minute == 45) )

Ph2 <- plot_ly(houseweek32, x = ~houseweek32$DateTime, y= ~houseweek32$Sub_metering_1, 
       name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek32$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek32$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption Summer, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseweek33 <- filter(energyInfoDF, year == 2008 & month == 8 & week == 33 & (minute == 0 | minute == 15 | minute == 30 | minute == 45) )

Ph3 <- plot_ly(houseweek33, x = ~houseweek33$DateTime, y= ~houseweek33$Sub_metering_1, 
        name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek33$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek33$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption Summer, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseweek34 <- filter(energyInfoDF, year == 2008 & month == 8 & week == 34 & (minute == 0 | minute == 15 | minute == 30 | minute == 45) )

Ph4 <- plot_ly(houseweek34, x = ~houseweek34$DateTime, y= ~houseweek34$Sub_metering_1, 
        name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek34$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek34$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption Summer, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# add multiple plots with plotly
plot_2008  <- subplot(Ph1, Ph2, Ph3, Ph4, nrows = 4, margin = 0.05)

################################################################################################

###################################################
# Plotting for August 2008 by each Day , week 1 
###################################################

houseday1 <- filter(energyInfoDF, year == 2008 & month == 8 & day == 1 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

pl1<- plot_ly(houseday1, x = ~houseday1$DateTime, y= ~houseday1$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday1$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday1$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption August 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseday4 <- filter(energyInfoDF, year == 2008 & month == 8 & day == 4 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

pl4<- plot_ly(houseday4, x = ~houseday4$DateTime, y= ~houseday4$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday4$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday4$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption August 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseday5 <- filter(energyInfoDF, year == 2008 & month == 8 & day == 5 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

pl5<- plot_ly(houseday5, x = ~houseday5$DateTime, y= ~houseday5$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday5$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday5$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption August 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# add multiple plots with plotly
plot_Aug  <- subplot(pl1, pl4, pl5, nrows = 3, margin = 0.05)

#Percentage of total power use over a day by each sub-meter.
#Percentage of total power use over an entire year by each sub-meter.
yrSub <- table(Yr_usage_sub)
pie(Yr_usage_sub, labels, main = "City pie chart", col = rainbow(length(x)))

#################################################################################
## Line plot for energy usage in all submeters - Too DENSE
#################################################################################
housedata2008 <- filter(energyInfoDF, year == 2008 & month == 8)

Line_sm1 <- ggplot(housedata2008) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm2 <- ggplot(housedata2008) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm3 <- ggplot(housedata2008) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

plot_Line  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3)