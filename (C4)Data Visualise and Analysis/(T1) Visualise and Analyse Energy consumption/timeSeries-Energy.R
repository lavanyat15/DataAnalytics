######################################################################################
## Topics covered in this code : 

## Reading data in from mysql DB, cleaning data, filtering data to find patterns in energy usage
## Visualization for Submeters by month and weekly.
## Granularity added to line plots to understand energy usage
## Filtered for Month of January (2007,2008,2009) with frequency 52
## created TS object to forecast on TSLM model

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

summary(timeDf$Sub_metering_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   1.159   0.000  82.000 
sd(timeDf$Sub_metering_1) #  6.288272
summary(timeDf$Sub_metering_2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   1.343   1.000  78.000 
sd(timeDf$Sub_metering_2) # 5.972199
summary(timeDf$Sub_metering_3)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   6.216  17.000  31.000 
sd(timeDf$Sub_metering_3) # 8.341281


#################################################################################
### Which submeter uses more power 
# sm1     sm2     sm3
# 1819989 2108410 9758843
##  Submeter 3 Uses more power, Submeter 1 uses least
#################################################################################

sm1<-sum(timeDf$Sub_metering_1)
sm2<-sum(timeDf$Sub_metering_2)
sm3<-sum(timeDf$Sub_metering_3)

#################################################################################
## Plotting to compare Submeter power usage
#################################################################################

x <- timeDf[,c(4,5,6)] # Subsetting only the columns we need (All submeter readings)
library(reshape2)
data<- melt(x)
# Density Plot
dp<-ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)
dp
##########################################################
## Usage Grouped by month and year
## Usage seems more on submeter #3
## Same as inferred from density and Box plot above
##########################################################
Yr_usage_sub <-  timeDf%>% 
  group_by(year) %>% 
  summarise(sub1_avg=mean(Sub_metering_1), sub2_avg= mean(Sub_metering_2),sub3_avg= mean(Sub_metering_3)) %>% 
  ungroup() %>% 
  arrange(year)
head(Yr_usage_sub)


#######################################################################
# OPTIONAL work in course POA 1
# Percentage of total power use by each sub-meterin 2007, 2008
# PieChart 
#######################################################################
# Converting to long format for pie chart
library(tidyr)
sub_usage <- Yr_usage_sub%>%
  tidyr::pivot_longer(
  cols = starts_with("sub"), 
  names_to = "submeter", 
  values_to = "result", 
  names_prefix = "sub_")
View(sub_usage)

slice1 <- filter(sub_usage, year==2007)
pctsub1 <- round(slice1$result/sum(slice1$result)*100) 
lblssub1 <- slice1$submeter
lblssub1 <- paste(lblssub1, pctsub1) # add percents to labels
lblssub1 <- paste(lblssub1,"%",sep="") # add '%' to labels

slice2 <- filter(sub_usage, year==2008)
pctsub2 <- round(slice2$result/sum(slice2$result)*100) 
lblssub2 <- slice2$submeter
lblssub2 <- paste(lblssub2, pctsub2) # add percents to labels
lblssub2 <- paste(lblssub2,"%",sep="") # add '%' to labels

slice3 <- filter(sub_usage, year==2009)
pctsub3 <- round(slice3$result/sum(slice3$result)*100) 
lblssub3 <- slice3$submeter
lblssub3 <- paste(lblssub3, pctsub3) # add percents to labels
lblssub3 <- paste(lblssub3,"%",sep="") # add '%' to labels

par(mfrow = c(1,3)) # Display all piecharts in 1 row 3 columns
pie(slice1$result, labels=lblssub1, main="Pie Chart for energy usage in 2007")
pie(slice2$result, labels=lblssub2, main="Pie Chart for energy usage in 2008")
pie(slice3$result, labels=lblssub3, main="Pie Chart for energy usage in 2009")
dev.off() # reset par


#######################################################################
# OPTIONAL work in course POA 1
# Percentage of total power usage by each submeter grouped by year
# PieChart 
#######################################################################

pct1 <- round(Yr_usage_sub$sub1_avg/sum(Yr_usage_sub$sub1_avg)*100)
pct2 <- round(Yr_usage_sub$sub2_avg/sum(Yr_usage_sub$sub2_avg)*100)
pct3 <- round(Yr_usage_sub$sub3_avg/sum(Yr_usage_sub$sub3_avg)*100)
lbls <- Yr_usage_sub$year


lbls1 <- paste(lbls, pct1) # add percents to labels
lbls1 <- paste(lbls1,"%",sep="") # add '%' to labels
lbls2 <- paste(lbls, pct2) 
lbls2 <- paste(lbls2,"%",sep="") 
lbls3 <- paste(lbls, pct3)
lbls3 <- paste(lbls3,"%",sep="")

par(mfrow = c(1,3)) # Display all piecharts in 1 row 3 columns

pie(Yr_usage_sub$sub1_avg, labels=lbls1, main="Pie Chart of Submeter 1")
pie(Yr_usage_sub$sub2_avg, labels=lbls2, main="Pie Chart of Submeter 2")
pie(Yr_usage_sub$sub3_avg, labels=lbls3, main="Pie Chart of Submeter 3")

dev.off() # reset par

Mon_usage_sub <-  timeDf%>% 
  group_by(month,year) %>% 
  summarise(sub1_avg=mean(Sub_metering_1), sub2_avg= mean(Sub_metering_2),sub3_avg= mean(Sub_metering_3)) %>% 
  ungroup() %>% 
  arrange(year)
View(Mon_usage_sub)

#################################################################
## Subsetting data and adding granularity from here on 
## Using Plotly 
#################################################################

###################################################
# Plotting for August 2008 by each week 
###################################################
houseweek31 <- filter(timeDf, year == 2008 & month == 8 & week == 31 & (minute == 0 | minute == 30) )

Ph1<- plot_ly(houseweek31, x = ~houseweek31$DateTime, y= ~houseweek31$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek31$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek31$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption Summer, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


houseweek32 <- filter(timeDf, year == 2008 & month == 8 & week == 32 & (minute == 0 | minute == 30) )

Ph2 <- plot_ly(houseweek32, x = ~houseweek32$DateTime, y= ~houseweek32$Sub_metering_1, 
               name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek32$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek32$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption Summer, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseweek33 <- filter(timeDf, year == 2008 & month == 8 & week == 33 & (minute == 0 | minute == 30) )

Ph3 <- plot_ly(houseweek33, x = ~houseweek33$DateTime, y= ~houseweek33$Sub_metering_1, 
               name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek33$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek33$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption Summer, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseweek34 <- filter(timeDf, year == 2008 & month == 8 & week == 34 & (minute == 0 | minute == 30) )

Ph4 <- plot_ly(houseweek34, x = ~houseweek34$DateTime, y= ~houseweek34$Sub_metering_1, 
               name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek34$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek34$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption August, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# add multiple plots with plotly
plot_2008  <- subplot(Ph1, Ph2, Ph3, Ph4, nrows = 4, margin = 0.05)

###################################################
# Plotting for January 2008 by each week 
###################################################
houseweek1 <- filter(timeDf, year == 2008 & month == 1 & week == 1 & (minute == 0 | minute == 30) )

Pjan1<- plot_ly(houseweek1, x = ~houseweek1$DateTime, y= ~houseweek1$Sub_metering_1, 
              name = 'Kitchen - SM 1',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek1$Sub_metering_2, name = 'Laundry Room - SM 2',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek1$Sub_metering_3, name = 'Water Heater & AC - SM 3',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008 - Week 1 (Every 30 minute)",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


houseweek2 <- filter(timeDf, year == 2008 & month == 1 & week == 2 & (minute == 0 | minute == 30) )

Pjan2 <- plot_ly(houseweek2, x = ~houseweek2$DateTime, y= ~houseweek2$Sub_metering_1, 
               name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek2$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek2$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseweek3 <- filter(timeDf, year == 2008 & month == 1 & week == 3 & (minute == 0 | minute == 30) )

Pjan3 <- plot_ly(houseweek3, x = ~houseweek3$DateTime, y= ~houseweek3$Sub_metering_1, 
               name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek3$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek3$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseweek4 <- filter(timeDf, year == 2008 & month == 1 & week == 4 & (minute == 0 | minute == 30) )

Pjan4 <- plot_ly(houseweek4, x = ~houseweek4$DateTime, y= ~houseweek4$Sub_metering_1, 
               name = 'Kitchen',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek4$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek4$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# add multiple plots with plotly
plot_jan2008  <- subplot(Pjan1, Pjan2, Pjan3, Pjan4, nrows = 4, margin = 0.05)

#######################################################
# Plotting for January 2008 by  Day 1,2,3, 4, 5 in week 1 
########################################################

janday1 <- filter(timeDf, year == 2008 & month == 1 & day == 1 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

pljan1<- plot_ly(janday1, x = ~janday1$DateTime, y= ~janday1$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines',color="blue") %>%
  add_trace(y = ~janday1$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines',color="red") %>%
  add_trace(y = ~janday1$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines',color="green") %>%
  layout(title = "Power Consumption per day January 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

janday2 <- filter(timeDf, year == 2008 & month == 1 & day == 2 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

pljan2<- plot_ly(janday2, x = ~janday2$DateTime, y= ~janday2$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines',color="blue") %>%
  add_trace(y = ~janday2$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines',color="red") %>%
  add_trace(y = ~janday2$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines',color="green") %>%
  layout(title = "Power Consumption per day January 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

janday3 <- filter(timeDf, year == 2008 & month == 1 & day == 3 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))
pljan3<- plot_ly(janday3, x = ~janday3$DateTime, y= ~janday3$Sub_metering_1, 
                 name = 'Kitchen',type = 'scatter', mode = 'lines',color="blue") %>%
  add_trace(y = ~janday3$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines',color="red") %>%
  add_trace(y = ~janday3$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines',color="green") %>%
  layout(title = "Power Consumption per day January 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

janday4 <- filter(timeDf, year == 2008 & month == 1 & day == 4 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

pljan4<- plot_ly(janday4, x = ~janday4$DateTime, y= ~janday4$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines',color="blue") %>%
  add_trace(y = ~janday4$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines',color="red") %>%
  add_trace(y = ~janday4$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines',color="green") %>%
  layout(title = "Power Consumption per day January 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

janday5 <- filter(timeDf, year == 2008 & month == 1 & day == 5 & (minute == 0 | minute == 15 | minute == 30 | minute == 45))

pljan5<- plot_ly(janday5, x = ~janday5$DateTime, y= ~janday5$Sub_metering_1, 
              name = 'Kitchen',type = 'scatter', mode = 'lines',color="blue") %>%
  add_trace(y = ~janday5$Sub_metering_2, name = 'Laundry Room',type = 'scatter', mode = 'lines',color="red") %>%
  add_trace(y = ~janday5$Sub_metering_3, name = 'Water Heater & AC',type = 'scatter', mode = 'lines',color="green") %>%
  layout(title = "Power Consumption  per day January 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# add multiple plots with plotly
plot_Janday  <- subplot(pljan1, pljan2, pljan3, pljan4, pljan5, nrows = 5, margin = 0.05)


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

plot_Line  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3)

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
## ## Comparing  week 1 and week 2 in January 2009 for usage pattern
###################################################################################

house4509 <- timeDf[seq(1,nrow(timeDf),45),]
housedatawk109 <- filter(house4509, year == 2009 & month == 1 & week == 1)

Line_sm109 <- ggplot(housedatawk109) + geom_line(mapping = aes(DateTime,Sub_metering_1),colour="Blue") + ggtitle("Submeter-1 (Kitchen) Vs DateTime")
Line_sm209 <- ggplot(housedatawk109) + geom_line(mapping = aes(DateTime,Sub_metering_2),colour="Green") + ggtitle("Submeter-2(Laundry Room) Vs DateTime")
Line_sm309 <- ggplot(housedatawk109) + geom_line(mapping = aes(DateTime,Sub_metering_3),colour="Orange") + ggtitle("Submeter-3(Water Heater & AC) Vs DateTime")

plot_Line  <- grid.arrange(Line_sm1,Line_sm2,Line_sm3)

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
##################################################################################
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009-SM3
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

########################################################
## Subset to one observation 
# per week on Mondays at 
# 9:00pm for 2007, 2008 and 2009 - SM1 & SM2
#########################################################
house070809weeklyH21 <- filter(timeDf, weekday == 'Monday' & hour == 21 & minute == 1)

## Create TS object with SubMeter1
tsSM1_070809weeklyH21 <- ts(house070809weeklyH21$Sub_metering_1, frequency=52, start=c(2007,1))

autoplot(tsSM1_070809weeklyH21, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM1_070809weeklyH21 , col = 2)

## Create TS object with SubMeter2
tsSM2_070809weeklyH21 <- ts(house070809weeklyH21$Sub_metering_2, frequency=52, start=c(2007,1))

autoplot(tsSM2_070809weeklyH21, ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM2_070809weeklyH21, col=3)

##Split time series to its components and plot them
mysdSM1 <- decompose(tsSM1_070809weeklyH21)
summary(mysdSM1)
mysdSM2 <- decompose(tsSM2_070809weeklyH21)
mysdSM3 <- decompose(tsSM3_070809weekly)
plot(mysdSM1, col=2)
plot(mysdSM2, col=3)
plot(mysdSM3, col=4)


############################################################################
# Multiple ts plots in 1 Plot - Self learning purpose
############################################################################
plot.ts(tsSM1_070809weeklyH21, ylab="Energy usage in KWH", col=2)
lines(tsSM2_070809weeklyH21,               # Draw second time series
      type = "l",
      col = 3)
lines(tsSM3_070809weekly,                 # Draw third time series
      type = "l",
      col = 4)
legend("topright",                        # Add legend to plot
       c("Kitchen", "Laundry", "Water Heater / AC"),
       lty = 2,
       col = 2:4)
############################################################################
## Apply time series linear regression to the sub-meter 3 ts object 
## and use summary to obtain R2 and RMSE from the model you built
############################################################################
library(forecast)   # To forecast model
attributes(tsSM3_070809weekly)

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)
