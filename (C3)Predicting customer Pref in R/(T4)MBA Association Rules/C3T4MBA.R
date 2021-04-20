########################################
# Imports
########################################
library(arules)
library(arulesViz)
library(tcltk)
library(plotly)

# Read transaction ( Do not use read.csv for transaction data)

tr <- read.transactions("ElectronidexTransactions2017/ElectronidexTransactions2017.csv",
                        format = "basket", sep = ",", skip = 0)

#######################################################
# Analyse Data 
# Methods that can be used to analyse transaction data
#######################################################
# See 1-10 transactions
inspect(tr[1:50]) 
summary(tr)

dim(tr) # get dimension of transaction object

# To view data in readable format
as(tr[1:50], "data.frame") # List
DATAFRAME(tr[1:50]) # Dataframe

length(tr) # Number of transactions.

size(tr) # Number of items per transaction

LIST(tr) # Lists the transactions by conversion

itemLabels(tr) # To see the item labels

#######################################################
# Visualise Dataset
#######################################################
itemFrequencyPlot(tr,topN=50,type="absolute") # Plot top n items

image(sample(tr,150))

#######################################################
# Apriori Alogirthm
#######################################################
prodRules<- apriori(tr, parameter = list(supp=0.003, conf=0.7))

# Inspect Rules
inspect(prodRules[1:10])

#######################################################
##  Find Redundent rules
#######################################################

prodRules       # has 19 rules

redrules <- is.redundant(prodRules)

summary(redrules)      # No Redundent rules

# To remove redundent rules if any 
prodRules <- prodRules[!redrules]

## non-redundant rules
inspect(prodRules[!is.redundant(prodRules)])

#Sorting by a lift
prodRules_bylift <- sort(prodRules, by="lift", decreasing=TRUE)

## A faster/less memory consuming way to get the top n rules sort by parameter 
inspect(head(prodRules, n=23, by = "lift"))

#######################################################
##  Filtering rules by specific product 
#######################################################
## Type 1
rulesinimac <- subset(prodRules_bylift, items %in% "iMac")
as(rulesinimac, "data.frame") # Instead of inspect () due to readable format

## Type 2
## Check what customers buy when they bought HP laptop
rules_hplaptop <- apriori(tr, parameter = list(supp=0.00001, conf=0.2),
                  appearance = list(default="rhs", lhs="HP Laptop"))

as(rules_hplaptop, "data.frame") # Instead of inspect () due to readable format
plot(rules_hplaptop, method="paracoord")

rules_imac <- apriori(tr, parameter = list(supp=0.00001, conf=0.2),
                          appearance = list(default="rhs", lhs="iMac"))
as(rules_imac, "data.frame") # Instead of inspect () due to readable format
plot(rules_imac, method="graph")

#######################################################
##  Visualise Rules
#######################################################
plot(prodRules_bylift[1:7], method="paracoord")
plot(prodRules_bylift[8:23], method="paracoord")

plot(prodRules_bylift[1:7], method = "grouped")
plot(prodRules_bylift[8:23], method = "grouped")

# Filter rules with confidence greater than 0.4 or 40%
subRules1<-prodRules_bylift[quality(prodRules_bylift)$confidence>0.4]

#Plot SubRules - scatter plot with supp and conf on x, y and Lift as color
plot(subRules1)

# Two-key Plot
# The two-key plot uses support and confidence on x and y-axis respectively. 
# It uses order for coloring. The order is the number of items in the rule.
plot(subRules1,method="two-key plot")

# Graph for top 10 rules
top10subRules <- head(subRules1, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

#Visualise as Data Table
inspectDT(prodRules_bylift)

summary(prodRules)


#######################################################
## Increase Confidence and decrease support to  
## increase rule count to make relevant results 
## (make  intuitive sense)
#######################################################


#############################
# Apriori Alogirthm
#############################

prodRules2<- apriori(tr, parameter = list(supp=0.001, conf=0.9))
summary(prodRules2)
# Inspect Rules
inspect(prodRules2[1:10])

###############################
##  Find Redundent rules
###############################

prodRules2       # has 197 rules

redrules2 <- is.redundant(prodRules2)
  
summary(redrules2) # Has 17 redundent rules

# To remove redundent rules if any 
prodRules2 <- prodRules2[!redrules2] # Has 180 relevant rules

## non-redundant rules
inspect(prodRules2[!is.redundant(prodRules2)])

#Sorting by a lift
prodRules_bylift2 <- sort(prodRules2, by="lift", decreasing=TRUE)

## A faster/less memory consuming way to get the top n rules sort by parameter 
as(head(prodRules2, n=35, by = "lift"), "data.frame")

#######################################################
##  Visualise Rules
#######################################################

# Filter rules with confidence greater than 0.5 or 50% - all rules were above 90% from summary
subRules2<-prodRules_bylift2[quality(prodRules_bylift2)$confidence>0.5]

#Plot SubRules - scatter plot with supp and conf on x, y and Lift as color
plot(subRules2)

# Two-key Plot
# The two-key plot uses support and confidence on x and y-axis respectively. 
# It uses order for coloring. The order is the number of items in the rule.
plot(subRules2,method="two-key plot")

# Graph for top 10 rules
top10subRules <- head(subRules1, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Parallel coordinates plot
plot(prodRules_bylift2, engine="interactive")
plot(prodRules_bylift2[1:15], method="paracoord")
plot(prodRules_bylift2[16:30], method="paracoord")

#Group plot
plot(prodRules_bylift2[1:50], method = "grouped")
plot(prodRules_bylift2[16:30], method = "grouped")

head(quality(prodRules_bylift2))

#Visualise as Data Table
inspectDT(prodRules_bylift2)

summary(prodRules_bylift2)

# Check what items are bought with ViewSonic Monitor since it had the most lift
rulesinVSmonitor <- subset(prodRules_bylift2, items %in% "ViewSonic Monitor")
as(rulesinVSmonitor, "data.frame")
plot(rulesinVSmonitor, method = "grouped")

# Check what items are bought with Lenovo Desktop Computer since it had the most lift
rulesinlenovodesktop <- subset(prodRules_bylift2, items %in% "Lenovo Desktop Computer")
as(rulesinlenovodesktop, "data.frame")

# Check what items are bought with CYBERPOWER Gamer Desktop 
rulesincybergamerdesktop <- subset(prodRules_bylift2, items %in% "CYBERPOWER Gamer Desktop")
as(rulesincybergamerdesktop, "data.frame")

# Check what items are bought with Apple Earpods since it was one of top frequently bought items
rulesinappleearpod <- subset(prodRules_bylift2, items %in% "Apple Earpods")
as(rulesinappleearpod, "data.frame")


############################################################################################################
## From the above subset the rhs is iMac or HP Laptop.
## Which is the most frequently bought item
## Summary(tr) conveys the same
#     most frequent items:
#       iMac                HP Laptop CYBERPOWER Gamer Desktop            Apple Earpods        Apple MacBook Air 
#       2519                     1909                     1809                     1715                     1530 
#       (Other) 
#       33622 

############################################################################################################



