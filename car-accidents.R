rm(list=ls())
library(caTools) 
library(ggplot2)
library(dplyr)
library(corrplot)
library(cluster) 
library(gurobi)

#setwd("/Users/Ahmad Dakhqan/Desktop/MSCI 433/Project/car-accidents/")
setwd("/Users/Celeste/Documents/GitHub/car-accidents/")
set.seed(3859)

#################################################### DATA PREPARATION ###################################################

# Due to memory issues, only use the last 1 million lines of the data (most recent)
# data = read.csv("NCDB_1999_to_2014.csv")
# n = nrow(data)
# m = n - 1000000 + 1
# data = data[m:n,]
# write.csv(data, "data.csv")

data=read.csv("data.csv")

# data cleaning: P_ISEV
data = data[data$P_ISEV != "N",]
data = data[data$P_ISEV != "U",]
data = data[data$P_ISEV != "X",]
data$P_ISEV = as.numeric(as.character(data$P_ISEV))
# Since what we want to classify is injured vs not, use 0s for non-injured and 1s for injured and fatalities
data$P_ISEV[data$P_ISEV == 1 ] = 0
data$P_ISEV[data$P_ISEV == 2 | data$P_ISEV == 3] = 1

# data cleaning: P_SEX
data = data[data$P_SEX != "N",]
data = data[data$P_SEX != "U",] 

# data cleaning: C_MNTH
data = data[data$C_MNTH != "UU",]
data = data[data$C_MNTH != "XX",]

# data cleaning: C_WDAY
data = data[data$C_WDAY != "U",] 
data = data[data$C_WDAY != "X",] 

# data cleaning: C_HOUR
data = data[data$C_HOUR != "UU",]
data = data[data$C_HOUR != "XX",]
# Make colliosn hour group categories
data$C_HOUR = as.numeric(as.character(data$C_HOUR))
data$C_HOUR[data$C_HOUR >= 0 & data$C_HOUR <= 2] = 0
data$C_HOUR[data$C_HOUR >= 3 & data$C_HOUR <= 5] = 3
data$C_HOUR[data$C_HOUR >= 6 & data$C_HOUR <= 8] = 6
data$C_HOUR[data$C_HOUR >= 9 & data$C_HOUR <= 11] = 9
data$C_HOUR[data$C_HOUR >= 12 & data$C_HOUR <= 14] = 12
data$C_HOUR[data$C_HOUR >= 15 & data$C_HOUR <= 17] = 15
data$C_HOUR[data$C_HOUR >= 18 & data$C_HOUR <= 20] = 18
data$C_HOUR[data$C_HOUR >= 21 & data$C_HOUR <= 23] = 21

# data cleaning: C_VEHS
data = data[data$C_VEHS != "UU",]
data = data[data$C_VEHS != "XX",]
# Make vehicle collided group categories rather than usuing indidvidual number of cars in collision
data$C_VEHS = as.numeric(as.character(data$C_VEHS))
data$C_VEHS[data$C_VEHS >= 1 & data$C_VEHS <= 5] = 1
data$C_VEHS[data$C_VEHS >= 6 & data$C_VEHS <= 25] = 6
data$C_VEHS[data$C_VEHS >= 26] = 26

# data cleaning: C_CONF
data = data[data$C_CONF != "QQ",]
data = data[data$C_CONF != "UU",]
data = data[data$C_CONF != "XX",]
data$C_CONF = as.numeric(as.character(data$C_CONF))
# Combine all categories for single vehicle in motion
data$C_CONF[data$C_CONF >= 1 & data$C_CONF <= 6] = 1
# Combine all categories for two vehicles in motion (same direction of travel)
data$C_CONF[data$C_CONF >= 21 & data$C_CONF <= 25] = 21
# Combine all categories for two vehicles in motion (different direction of travel)
data$C_CONF[data$C_CONF >= 31 & data$C_CONF <= 36] = 31

# data cleaning: C_RCFG
data = data[data$C_RCFG !=  "QQ",]
data = data[data$C_RCFG !=  "UU",] 
data = data[data$C_RCFG !=  "XX",]

# data cleaning: C_WTHR
data = data[data$C_WTHR !=  "Q",]
data = data[data$C_WTHR !=  "U",]
data = data[data$C_WTHR !=  "X",]

# data cleaning: C_RSUR
data = data[data$C_RSUR !=  "Q",]
data = data[data$C_RSUR !=  "U",]
data = data[data$C_RSUR !=  "X",]

# data cleaning: C_RALN 
data = data[data$C_RALN != "Q",] 
data = data[data$C_RALN != "U",] 
data = data[data$C_RALN != "X",] 

# data cleaning: C_TRAF 
data = data[data$C_TRAF != "QQ",]
data = data[data$C_TRAF != "UU",]
data = data[data$C_TRAF != "XX",]
data$C_TRAF = as.numeric(as.character(data$C_TRAF))
# Group traffic signals together
data$C_TRAF[data$C_TRAF >= 1 & data$C_TRAF <= 2] = 1
# Group signs together
data$C_TRAF[data$C_TRAF >= 3 & data$C_TRAF <= 5] = 3
# Group schools together
data$C_TRAF[data$C_TRAF >= 8 & data$C_TRAF <= 9] = 8
# Group no passing and low speed zones
data$C_TRAF[data$C_TRAF >= 10 & data$C_TRAF <= 12] = 10
# Group school bus together
data$C_TRAF[data$C_TRAF >= 13 & data$C_TRAF <= 14] = 13
# Group railway crossings together
data$C_TRAF[data$C_TRAF >= 15 & data$C_TRAF <= 16] = 15

# data cleaning: V_ID 
data = data[data$V_ID != "UU",]

# data cleaning: V_TYPE
data = data[data$V_TYPE != "QQ",] 
data = data[data$V_TYPE != "UU",] 
data = data[data$V_TYPE != "NN",] 
data$V_TYPE = as.numeric(as.character(data$V_TYPE))
# Group trucks together
data$V_TYPE[data$V_TYPE >= 5 & data$V_TYPE <= 7] = 5
# Group busses together
data$V_TYPE[data$V_TYPE >= 9 & data$V_TYPE <= 11] = 9

# data cleaning: V_YEAR
data = data[data$V_YEAR != "NNNN",] 
data = data[data$V_YEAR != "UUUU",] 
data = data[data$V_YEAR != "QQQQ",]
# Make vehicle year group categories rather than using indidvidual years
data$V_YEAR = as.numeric(as.character(data$V_YEAR))
data$V_YEAR[data$V_YEAR > 1900 & data$V_YEAR <= 1950] = 1901
data$V_YEAR[data$V_YEAR > 1950 & data$V_YEAR <= 1980] = 1951
data$V_YEAR[data$V_YEAR > 1980 & data$V_YEAR <= 1990] = 1981
data$V_YEAR[data$V_YEAR > 1990 & data$V_YEAR <= 2000] = 1991
data$V_YEAR[data$V_YEAR > 2000 & data$V_YEAR <= 2010] = 2001
data$V_YEAR[data$V_YEAR > 2010] = 2011

# data cleaning: P_ID
data = data[data$P_ID != "NN",]
data = data[data$P_ID != "UU",]

# data cleaning: P_PSN
data = data[data$P_PSN != "NN",]
data = data[data$P_PSN != "QQ",]
data = data[data$P_PSN != "UU",]
data = data[data$P_PSN != "XX",]
# Make person position categories
data$P_PSN = as.numeric(as.character(data$P_PSN))
# 96 is position unknown so get rid of rows with this
data = data[data$P_PSN != 96,]
# First row of vehicle
data$P_PSN[data$P_PSN >= 11 & data$P_PSN <= 13] = 11
# Second row of vehicle
data$P_PSN[data$P_PSN >= 21 & data$P_PSN <= 23] = 21
# Third row of vehicle
data$P_PSN[data$P_PSN >= 31 & data$P_PSN <= 33] = 31

# data cleaning: P_SAFE
data = data[data$P_SAFE != "NN",]
data = data[data$P_SAFE != "QQ",]
data = data[data$P_SAFE != "UU",]
data = data[data$P_SAFE != "XX",]

# data cleaning: P_USER
data = data[data$P_USER != "U",] 

# data cleaning: P_AGE
data = data[data$P_AGE != "NN",]
data = data[data$P_AGE != "UU",]
data = data[data$P_AGE != "XX",]
# Store curernt dataset for use below
dataAgeNotGrouped = data
# Make age-group categories rather than usuing indidvidual ages
data$P_AGE = as.numeric(as.character(data$P_AGE))
data$P_AGE[data$P_AGE > 0 & data$P_AGE <= 10] = 1
data$P_AGE[data$P_AGE > 10 & data$P_AGE <= 20] = 11
data$P_AGE[data$P_AGE > 20 & data$P_AGE <= 30] = 21
data$P_AGE[data$P_AGE > 30 & data$P_AGE <= 40] = 31
data$P_AGE[data$P_AGE > 40 & data$P_AGE <= 50] = 41
data$P_AGE[data$P_AGE > 50 & data$P_AGE <= 60] = 51
data$P_AGE[data$P_AGE > 60 & data$P_AGE <= 70] = 61
data$P_AGE[data$P_AGE > 70 & data$P_AGE <= 80] = 71
data$P_AGE[data$P_AGE > 80 & data$P_AGE <= 90] = 81
data$P_AGE[data$P_AGE > 90] = 91

# Write cleaned data to CSV
write.csv(data, "cleanedData.csv")


# Also want a dataset with just drivers
# Filter rows to get a dataset of only drivers
# P_USER = 1 corresponds to a motor vehicle driver and P_PSN = 11 corresponds to sitting in the driver seat 
driverData = dataAgeNotGrouped[dataAgeNotGrouped$P_USER == 1 & dataAgeNotGrouped$P_PSN == 11,]
# Get rid of illegal drivers (under 16) since they cannot be provided insurance
driverData$P_AGE = as.numeric(as.character(driverData$P_AGE))
driverData = driverData[driverData$P_AGE >= 16,]
# Make age-group categories rather than usuing indidvidual ages
driverData$P_AGE[driverData$P_AGE > 15 & driverData$P_AGE <= 20] = 16
driverData$P_AGE[driverData$P_AGE > 20 & driverData$P_AGE <= 30] = 21
driverData$P_AGE[driverData$P_AGE > 30 & driverData$P_AGE <= 40] = 31
driverData$P_AGE[driverData$P_AGE > 40 & driverData$P_AGE <= 50] = 41
driverData$P_AGE[driverData$P_AGE > 50 & driverData$P_AGE <= 60] = 51
driverData$P_AGE[driverData$P_AGE > 60 & driverData$P_AGE <= 70] = 61
driverData$P_AGE[driverData$P_AGE > 70 & driverData$P_AGE <= 80] = 71
driverData$P_AGE[driverData$P_AGE > 80 & driverData$P_AGE <= 90] = 81
driverData$P_AGE[driverData$P_AGE > 90] = 91
write.csv(driverData, "cleanedDriverData.csv")


#################################################### DATA VISUALIZATION ##################################################

# ------------------------------------------------------ ALL DATA ------------------------------------------------------

# P_SEX VISUALIZATION 

p_sexCategory = data.frame(
  category = c("Male","Female"),
  value = c(nrow(subset(data, P_SEX == "M")),nrow(subset(data, P_SEX == "F")))
)
# Compute percentages
p_sexCategory$fraction <- p_sexCategory$value / sum(p_sexCategory$value)

# Compute the cumulative percentages (top of each rectangle)
p_sexCategory$ymax <- cumsum(p_sexCategory$fraction)

# Compute the bottom of each rectangle
p_sexCategory$ymin <- c(0, head(p_sexCategory$ymax, n=-1))

# Compute label position
p_sexCategory$labelPosition <- (p_sexCategory$ymax + p_sexCategory$ymin) / 2

# Compute a good label
p_sexCategory$label <- paste0(p_sexCategory$category, "\n value: ", p_sexCategory$value)

# Make the plot
ggplot(p_sexCategory, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  ggtitle("All Data: Gender Distribution") +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


# P_AGE VISUALIZATION 

p_ageCategory = data.frame(
  category=c("<= 10",">10 & <=20",">20 & <=30",">30 & <=40",">40 & <=50",">50 & <=60",">60 & <=70",">70 & <=80",">80 & <=90",">90"),  
  value=c(nrow(subset(data, P_AGE == 1)),nrow(subset(data, P_AGE == 11)),nrow(subset(data, P_AGE == 21)),nrow(subset(data, P_AGE == 31)),nrow(subset(data, P_AGE == 41)),nrow(subset(data, P_AGE == 51)),nrow(subset(data, P_AGE == 61)),nrow(subset(data, P_AGE == 71)),nrow(subset(data, P_AGE == 81)),nrow(subset(data, P_AGE == 91)))
)
par(mar=c(6,6,4,2)+0.1,mgp=c(5,1,0))
barplot(height=p_ageCategory$value, names=p_ageCategory$category, xlab = "Age Range(s)",ylab = "Count", col=rgb(0.8,0,0,0.6), las=2, main = "All Data: Age Ranges",cex.names=0.6)


# V_TYPE VISUALIZATION 

v_typeCategory = data.frame(
  category = c("Light-duty","Truck","Tractor","Bus","Motorcycle","Off-Road","Bicycle","Motorhome","Farm Equip.","Construction","Fire Engine","Snowmobile","Street Car"),
  value=c(nrow(subset(data, V_TYPE == 1)),nrow(subset(data, V_TYPE == 5)),nrow(subset(data, V_TYPE == 8)),nrow(subset(data, V_TYPE == 9)),nrow(subset(data, V_TYPE == 14)),nrow(subset(data, V_TYPE == 16)),nrow(subset(data, V_TYPE == 17)),nrow(subset(data, V_TYPE == 18)),nrow(subset(data, V_TYPE == 19)),nrow(subset(data, V_TYPE == 20)),nrow(subset(data, V_TYPE == 21)),nrow(subset(data, V_TYPE ==22)),nrow(subset(data, V_TYPE == 23)))
)
par(mar=c(6,6,4,2)+0.1,mgp=c(5,1,0))
barplot(height=v_typeCategory$value, names=v_typeCategory$category, xlab = "Count",ylab = "Vehicle Type(s)", col=rgb(0.4,0,0,0.2), las=2, main = "All Data: Vehicle Types",cex.names=0.5,horiz=T)


# V_YEAR VISUALIZATION 

v_yearCategory = data.frame(
  category = c("<= 1950",">1950 & <=1980",">1980 & <=1990",">1990 & <=2000",">2000 & <=2010",">2010"),
  value = c(nrow(subset(data, V_YEAR == 1901)),nrow(subset(data, V_YEAR == 1951)),nrow(subset(data, V_YEAR == 1981)),nrow(subset(data, V_YEAR == 1991)),nrow(subset(data, V_YEAR == 2001)),nrow(subset(data, V_YEAR == 2011)))
)
par(mar=c(6,6,4,2)+0.1,mgp=c(5,1,0))
barplot(height=v_yearCategory$value, names=v_yearCategory$category, xlab = "Count",ylab = "Vehicle Model Year(s) Range", col=rgb(0.2,0.8,0.6,0.6), las=2, main = "All Data: Vehicle Model Years",cex.names=0.5, horiz=T)


# ----------------------------------------------------- DRIVER DATA -----------------------------------------------------

# P_SEX VISUALIZATION

p_sex2_Category = data.frame(
  category = c("Male","Female"),
  value = c(nrow(subset(driverData, P_SEX == "M")),nrow(subset(driverData, P_SEX == "F")))
)
# Compute percentages
p_sex2_Category$fraction <- p_sex2_Category$value / sum(p_sex2_Category$value)

# Compute the cumulative percentages (top of each rectangle)
p_sex2_Category$ymax <- cumsum(p_sex2_Category$fraction)

# Compute the bottom of each rectangle
p_sex2_Category$ymin <- c(0, head(p_sex2_Category$ymax, n=-1))

# Compute label position
p_sex2_Category$labelPosition <- (p_sex2_Category$ymax + p_sex2_Category$ymin) / 2

# Compute a good label
p_sex2_Category$label <- paste0(p_sex2_Category$category, "\n value: ", p_sex2_Category$value)

# Make the plot
ggplot(p_sex2_Category, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  ggtitle("Driver Data: Gender Distribution") +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


# P_AGE VISUALIZATION

p_age2_Category = data.frame(
  category=c("<= 10",">10 & <=20",">20 & <=30",">30 & <=40",">40 & <=50",">50 & <=60",">60 & <=70",">70 & <=80",">80 & <=90",">90"),  
  value=c(nrow(subset(driverData, P_AGE == 1)),nrow(subset(driverData, P_AGE == 11)),nrow(subset(driverData, P_AGE == 21)),nrow(subset(driverData, P_AGE == 31)),nrow(subset(driverData, P_AGE == 41)),nrow(subset(driverData, P_AGE == 51)),nrow(subset(driverData, P_AGE == 61)),nrow(subset(driverData, P_AGE == 71)),nrow(subset(driverData, P_AGE == 81)),nrow(subset(driverData, P_AGE == 91)))
)
par(mar=c(6,6,4,2)+0.1,mgp=c(5,1,0))
barplot(height=p_age2_Category$value, names=p_age2_Category$category,  xlab = "Age Range(s)",ylab = "Count",col=rgb(0.8,0,0,0.6), las=2, main = "Driver Data: Age Ranges",cex.names=0.6)


# V_TYPE VISUALIZATION

v_type2_Category = data.frame(
  category = c("Light-duty","Truck","Tractor","Bus","Motorcycle","Off-Road","Bicycle","Motorhome","Farm Equip.","Construction","Fire Engine","Snowmobile","Street Car"),
  value=c(nrow(subset(driverData, V_TYPE == 1)),nrow(subset(driverData, V_TYPE == 5)),nrow(subset(driverData, V_TYPE == 8)),nrow(subset(driverData, V_TYPE == 9)),nrow(subset(driverData, V_TYPE == 14)),nrow(subset(driverData, V_TYPE == 16)),nrow(subset(driverData, V_TYPE == 17)),nrow(subset(driverData, V_TYPE == 18)),nrow(subset(driverData, V_TYPE == 19)),nrow(subset(driverData, V_TYPE == 20)),nrow(subset(driverData, V_TYPE == 21)),nrow(subset(driverData, V_TYPE ==22)),nrow(subset(driverData, V_TYPE == 23)))
)
par(mar=c(6,6,4,2)+0.1,mgp=c(5,1,0))
barplot(height=v_type2_Category$value, names=v_type2_Category$category,xlab = "Count",ylab = "Vehicle Type(s)", col=rgb(0.4,0,0,0.2), las=2, main = "Driver Data: Vehicle Types",cex.names=0.5,horiz=T)


# V_YEAR VISUALIZATION

v_year2_Category = data.frame(
  category = c("<= 1950",">1950 & <=1980",">1980 & <=1990",">1990 & <=2000",">2000 & <=2010",">2010"),
  value = c(nrow(subset(driverData, V_YEAR == 1901)),nrow(subset(driverData, V_YEAR == 1951)),nrow(subset(driverData, V_YEAR == 1981)),nrow(subset(driverData, V_YEAR == 1991)),nrow(subset(driverData, V_YEAR == 2001)),nrow(subset(driverData, V_YEAR == 2011)))
)
par(mar=c(6,6,4,2)+0.1,mgp=c(5,1,0))
barplot(height=v_year2_Category$value, names=v_year2_Category$category,xlab = "Count",ylab = "Vehicle Model Year(s) Range", col=rgb(0.2,0.8,0.6,0.6), las=2, main = "Driver Data: Vehicle Model Years",cex.names=0.5, horiz=T) 


################################################### LOGISTIC REGRESSION ##################################################

# Read in CSV
data = read.csv("cleanedData.csv")
# Treat all data as categorical, not numerical
data$C_YEAR = as.factor(data$C_YEAR)
data$C_MNTH = as.factor(data$C_MNTH)
data$C_WDAY = as.factor(data$C_WDAY)
data$C_HOUR = as.factor(data$C_HOUR)
data$C_VEHS = as.factor(data$C_VEHS)
data$C_CONF = as.factor(data$C_CONF)
data$C_RCFG = as.factor(data$C_RCFG)
data$C_WTHR = as.factor(data$C_WTHR)
data$C_RSUR = as.factor(data$C_RSUR)
data$C_RALN = as.factor(data$C_RALN)
data$C_TRAF = as.factor(data$C_TRAF)
data$V_ID = as.factor(data$V_ID)
data$V_TYPE = as.factor(data$V_TYPE)
data$V_YEAR = as.factor(data$V_YEAR)
data$P_ID = as.factor(data$P_ID)
data$P_SEX = as.factor(data$P_SEX)
data$P_AGE = as.factor(data$P_AGE)
data$P_PSN = as.factor(data$P_PSN)
data$P_SAFE = as.factor(data$P_SAFE)
data$P_USER = as.factor(data$P_USER)
data$P_ISEV = as.factor(data$P_ISEV)

# For model with all data
split = sample.split(data$P_ISEV, SplitRatio = 0.7)
dataTrain = subset(data, split == TRUE)   # Observations to be put in the training set           
dataTest = subset(data, split == FALSE)  # Observations to be put in the testing set 
nrow(dataTrain)
nrow(dataTest)


# Read in driver data CSV
driverData = read.csv("cleanedDriverData.csv")
# Treat all data as categorical, not numerical
driverData$V_TYPE = as.factor(driverData$V_TYPE)
driverData$V_YEAR = as.factor(driverData$V_YEAR)
driverData$P_SEX = as.factor(driverData$P_SEX)
driverData$P_AGE = as.factor(driverData$P_AGE)
driverData$P_ISEV = as.factor(driverData$P_ISEV)

# For model with only driver data
driverSplit = sample.split(driverData$P_ISEV, SplitRatio = 0.7)
driverDataTrain = subset(driverData, driverSplit == TRUE)   # Observations to be put in the training set           
driverDataTest = subset(driverData, driverSplit == FALSE)  # Observations to be put in the testing set 
nrow(driverDataTrain)
nrow(driverDataTest)

# ----------------------------------------------- BUILDING MODELS --------------------------------------------------

# MODEL 1: All variables
# C_SEV is not included because the severity of the crash should not be used to predict the severity of a person in the crash
# V_ID is a sequence number, so it is not included 
# P_ID is a sequence number, so it is not included
# Model 1.1
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                  C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# Try taking out C_WDAY since some of its categories are not statistically significant
# Model 1.2
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add C_WDAY back
# Try taking out C_RCFG since some of its categories are not statistically significant
# Model 1.3
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add C_RCFG back
# Try taking out C_RSUR since some of its categories are not statistically significant
# Model 1.4
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                    C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add C_RSUR back
# Try taking out C_TRAF since some of its categories are not statistically significant
# Model 1.5
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add C_TRAF back
# Try taking out V_YEAR since some of its categories are not statistically significant
# Model 1.6
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF + V_TYPE + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add V_YEAR back
# Try taking out P_PSN since some of its categories are not statistically significant
# Model 1.7
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add P_PSN back
# Try taking out P_SAFE since some of its categories are not statistically significant
# Model 1.8
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add P_SAFE back
# Try taking out P_USER since some of its categories are not statistically significant (two categories are NA)
# Model 1.9
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)
# AIC went up so add P_USER back. Best fit model is Model 1.1
# Final model
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)



# MODEL 2: Collison Info
# C_SEV is not included because the severity of the crash should not be used to predict the severity of a person in the crash
# Model 2.1
severeLog2 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                  C_RSUR + C_RALN + C_TRAF, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog2)
# Try taking out C_YEAR since some of its categories are not statistically significant
# Model 2.2
severeLog2 = glm(P_ISEV ~ C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog2)
# Model 2.2 AIC did not decrease so add C_YEAR back in
# Try taking out C_WDAY since some of its categories are not statistically significant
# Model 2.3
severeLog2 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog2)
# Model 2.3 AIC did not decrease so add C_WDAY back in
# Try taking out C_RCFG since some of its categories are not statistically significant
# Model 2.4
severeLog2 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog2)
# Model 2.4 AIC did not decrease so add C_RCFG back in
# Try taking out C_TRAF since some of its categories are not statistically significant
# Model 2.5
severeLog2 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN , 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog2)
# Model 2.5 AIC did not decrease so add C_TRAF back in. Best fit model is Model 2.1
# Final model
severeLog2 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                   C_RSUR + C_RALN + C_TRAF, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog2)



# MODEL 3: Vehicle Info
# V_ID is a sequence number, so it is not included 
# Model 3.1
severeLog3 = glm(P_ISEV ~  V_TYPE + V_YEAR, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog3)
# There are only two predictor variables so we can easily try removing both 
# Try removing V_YEAR
# Model 3.2
severeLog3 = glm(P_ISEV ~  V_TYPE , 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog3)
# AIC went up so add V_YEAR back in
# Try removing V_TYPE
severeLog3 = glm(P_ISEV ~  V_YEAR , 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog3)
# AIC went up so add V_TYPE back in
# Best model is Model 3.1
# Final model
severeLog3 = glm(P_ISEV ~  V_TYPE + V_YEAR, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog3)



# MODEL 4: Person Info
# P_ID is a sequence number, so it is not included
# Model 4.1
severeLog4 = glm(P_ISEV ~ P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog4)
# Try taking out P_PSN since some of its categories are not statistically significant
# Model 4.2
severeLog4 = glm(P_ISEV ~ P_SEX + P_AGE + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog4)
# Model 4.2 AIC went up so add P_PSN back in
# Try taking out P_SAFE since some of its categories are not statistically significant
# Model 4.3
severeLog4 = glm(P_ISEV ~ P_SEX + P_AGE + P_PSN + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog4)
# Model 4.3 AIC went up so add P_SAFE back in. Best fit model is Model 4.1
# Final model
severeLog4 = glm(P_ISEV ~ P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                 data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog4)



# MODEL 5: Person Info + Vehicle info
# This model will be used to derive parameters/constraints for our optimization problem
# Only use variables that an insurance company would have access to when deciding how much to charge someone per year
# Use the dataset with drivers only since we are interested in making insurance policies for drivers (e.g. not passengers)
# Model 5.1
severeLog5 = glm(P_ISEV ~ V_TYPE + V_YEAR + P_SEX + P_AGE, 
                 data = driverDataTrain, family = binomial(link = "logit")) 
summary(severeLog5)
# Try taking out V_YEAR since some of its categories are not statistically significant
# Model 5.2
severeLog5 = glm(P_ISEV ~ V_TYPE + P_SEX + P_AGE, 
                 data = driverDataTrain, family = binomial(link = "logit")) 
summary(severeLog5)
# Model 5.2 AIC went up so add V_YEAR back
# Try taking out P_AGE since some of its categories are not statistically significant
# Model 5.3
severeLog5 = glm(P_ISEV ~ V_TYPE + V_YEAR + P_SEX, 
                 data = driverDataTrain, family = binomial(link = "logit")) 
summary(severeLog5)
# Model 5.3 AIC went up so add P_AGE back. Best fit model is Model 5.1
# Final model 
severeLog5 = glm(P_ISEV ~ V_TYPE + V_YEAR + P_SEX + P_AGE, 
                 data = driverDataTrain, family = binomial(link = "logit")) 
summary(severeLog5)


# -------------------------------------------- TESTING THE MODELS --------------------------------------------------

# Get the number of injured vs not
injuredFreq = as.data.frame(table(data$P_ISEV))
injuredFreq

# Get baseline accuracy
if(injuredFreq$Freq[2] > injuredFreq$Freq[1]) {
  injuredFreq$Freq[2]/(injuredFreq$Freq[1] + injuredFreq$Freq[2])
} else {
  injuredFreq$Freq[1]/(injuredFreq$Freq[1] + injuredFreq$Freq[2])
}


# MODEL 1

predictTrain = predict(severeLog1, type = "response") 
trainConfMatrix = table(dataTrain$P_ISEV, predictTrain>0.5)
trainConfMatrix
trainConfMatrix = as.data.frame(trainConfMatrix)

predictTest = predict(severeLog1, type = "response", newdata = dataTest)
testConfMatrix = table(dataTest$P_ISEV, predictTest>0.5) 
testConfMatrix
testConfMatrix = as.data.frame(testConfMatrix)

# Plot ROC for testing set
ROCRpred = prediction(predictTest, dataTest$P_ISEV)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
png("ROC1.png", width=1200, height=1200)
par(mar=c(3,3,3,3))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,2.0)) 
title("Model 1 - Receiver Operator Characteristic Curve", cex.main=3)
dev.off()
# By examining the curve, we can see that 0.5 and 0.6 are reasonable thresholds

# Calculate AUC for testing set
auc_ROCR = performance(ROCRpred, measure = "auc")
auc_ROCR = auc_ROCR@y.values[[1]]
auc_ROCR


# MODEL 2

predictTrain = predict(severeLog2, type = "response") 
trainConfMatrix = table(dataTrain$P_ISEV, predictTrain>0.5)
trainConfMatrix
trainConfMatrix = as.data.frame(trainConfMatrix)

predictTest = predict(severeLog2, type = "response", newdata = dataTest)
testConfMatrix = table(dataTest$P_ISEV, predictTest>0.5) 
testConfMatrix
testConfMatrix = as.data.frame(testConfMatrix)

# Plot ROC for testing set
ROCRpred = prediction(predictTest, dataTest$P_ISEV)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
png("ROC2.png", width=1200, height=1200)
par(mar=c(3,3,3,3))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,2.0)) 
title("Model 2 - Receiver Operator Characteristic Curve", cex.main=3)
dev.off()
# By examining the curve, we can see that 0.5 and 0.6 are reasonable thresholds

# Calculate AUC for testing set
auc_ROCR = performance(ROCRpred, measure = "auc")
auc_ROCR = auc_ROCR@y.values[[1]]
auc_ROCR


# MODEL 3

predictTrain = predict(severeLog3, type = "response") 
trainConfMatrix = table(dataTrain$P_ISEV, predictTrain>0.5)
trainConfMatrix
trainConfMatrix = as.data.frame(trainConfMatrix)

predictTest = predict(severeLog3, type = "response", newdata = dataTest)
testConfMatrix = table(dataTest$P_ISEV, predictTest>0.5) 
testConfMatrix
testConfMatrix = as.data.frame(testConfMatrix)

# Plot ROC for testing set
ROCRpred = prediction(predictTest, dataTest$P_ISEV)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
png("ROC3.png", width=1200, height=1200)
par(mar=c(3,3,3,3))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,2.0)) 
title("Model 3 - Receiver Operator Characteristic Curve", cex.main=3)
dev.off()
# By examining the curve, we can see that 0.5 and 0.6 are reasonable thresholds

# Calculate AUC for testing set
auc_ROCR = performance(ROCRpred, measure = "auc")
auc_ROCR = auc_ROCR@y.values[[1]]
auc_ROCR


# MODEL 4

predictTrain = predict(severeLog4, type = "response") 
trainConfMatrix = table(dataTrain$P_ISEV, predictTrain>0.5)
trainConfMatrix
trainConfMatrix = as.data.frame(trainConfMatrix)

predictTest = predict(severeLog4, type = "response", newdata = dataTest)
testConfMatrix = table(dataTest$P_ISEV, predictTest>0.5) 
testConfMatrix
testConfMatrix = as.data.frame(testConfMatrix)

# Plot ROC for testing set
ROCRpred = prediction(predictTest, dataTest$P_ISEV)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
png("ROC4.png", width=1200, height=1200)
par(mar=c(3,3,3,3))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,2.0)) 
title("Model 4 - Receiver Operator Characteristic Curve", cex.main=3)
dev.off()
# By examining the curve, we can see that 0.5 and 0.6 are reasonable thresholds

# Calculate AUC for testing set
auc_ROCR = performance(ROCRpred, measure = "auc")
auc_ROCR = auc_ROCR@y.values[[1]]
auc_ROCR


# MODEL 5

# Need to calculate baseline accuracy for our driver dataset
# Get the number of injured vs not
driverInjuredFreq = as.data.frame(table(driverData$P_ISEV))
driverInjuredFreq

# Get baseline accuracy
if(driverInjuredFreq$Freq[2] > driverInjuredFreq$Freq[1]) {
  driverInjuredFreq$Freq[2]/(driverInjuredFreq$Freq[1] + driverInjuredFreq$Freq[2])
} else {
  driverInjuredFreq$Freq[1]/(driverInjuredFreq$Freq[1] + driverInjuredFreq$Freq[2])
}

driverPredictTrain = predict(severeLog5, type = "response") 
driverTrainConfMatrix = table(driverDataTrain$P_ISEV, driverPredictTrain>0.5)
driverTrainConfMatrix
driverTrainConfMatrix = as.data.frame(driverTrainConfMatrix)

driverPredictTest = predict(severeLog5, type = "response", newdata = driverDataTest)
driverTestConfMatrix = table(driverDataTest$P_ISEV, driverPredictTest>0.5) 
driverTestConfMatrix
driverTestConfMatrix = as.data.frame(driverTestConfMatrix)

# Plot ROC for testing set
ROCRpred = prediction(driverPredictTest, driverDataTest$P_ISEV)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
png("ROC5.png", width=1200, height=1200)
par(mar=c(3,3,3,3))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,2.0)) 
title("Model 5 - Receiver Operator Characteristic Curve", cex.main=3)
dev.off()
# By examining the curve, we can see that 0.5 and 0.6 are reasonable thresholds

# Calculate AUC for testing set
auc_ROCR = performance(ROCRpred, measure = "auc")
auc_ROCR = auc_ROCR@y.values[[1]]
auc_ROCR





# Calculate accuracy for different thresholds

# Calculate specificity for different thresholds

# Calculate sensitivity for different thresholds





################################################## OPTIMIZATION #####################################################

# The idea is to make an insurance policy that maximizes revenue
# Every driver will pay a base cost of $1000 and up to an additional $1000 based on categories they fit into
# Each category value will have an associated factor. For example, males might have a factor of 0.1 and females could 
# have a factor of 0.2. Then males pay 0.1*1000 extra and females pay 0.2*1000 extra. The factors for ALL category values
# should sum to 1 so that the maximum possible value charge is $3000

# ASSUMPTIONS: We have filtered our dataset for specifically drivers (not passengers, pedestrians, etc.)
# but these drivers all have been in car accidents. Hence, we are making insurance policies for people who have been 
# in accidents. In these models, the number of times someone has been in a car accident is irrelevant (our data does
# not indicate repeat offenders)

# Create our dataset for the optimization problem
# Since we use outuput from the logistic regression in some of our optimization models, we will exclude the training data 
# from our dataset (i.e. we will use the testing set from the logistic regression in the optimization models)
insuranceData = subset(driverDataTest, select = c(P_SEX, P_AGE, V_TYPE, V_YEAR))

fixedCost = 2000
maxVariableCost = 1000

sexTable = as.data.frame(table(insuranceData$P_SEX))
ageTable = as.data.frame(table(insuranceData$P_AGE))
vehicleTypeTable = as.data.frame(table(insuranceData$V_TYPE))
vehicleYearTable = as.data.frame(table(insuranceData$V_YEAR))

# P_SEX: female, male
# P_AGE: 16-20, 21-30, 31-40, 41-50, 51-60, 61-70, 71-80, 81-90, 91+
# V_TYPE: Light duty vehicle (1), trucks/vans (5), road tractor (8), school busses (9), motorhomes (18), firetruck (21), streetcar(23)
# V_YEAR: 1901-1950, 1951-1980, 1981-1990, 1991-2000, 2001-2010, 2011+
# Total variables: 24

sexCategories = as.vector(sexTable$Var1)
ageCategories = as.vector(ageTable$Var1)
vehicleTypeCategories = as.vector(vehicleTypeTable$Var1)
vehicleYearCategories = as.vector(vehicleYearTable$Var1)

sexStartIndex = 1
ageStartIndex = sexStartIndex + NROW(sexCategories)
vehicleTypeStartIndex = ageStartIndex + NROW(ageCategories)
vehicleYearStartIndex = vehicleTypeStartIndex + NROW(vehicleTypeCategories)
endIndex = vehicleYearStartIndex + NROW(vehicleYearCategories) - 1

numVars = nrow(sexTable) + nrow(ageTable) + nrow(vehicleTypeTable) + nrow(vehicleYearTable) 

# Want our categories matrix in 1s and 0s (one-hot encoding)
# When a person matches a category value, use a 1, else use 0
sexMatrix = matrix(0, nrow = NROW(insuranceData), ncol = NROW(sexCategories))
ageMatrix = matrix(0, nrow = NROW(insuranceData), ncol = NROW(ageCategories))
vehicleTypeMatrix = matrix(0, nrow = NROW(insuranceData), ncol = NROW(vehicleTypeCategories))
vehicleYearMatrix = matrix(0, nrow = NROW(insuranceData), ncol = NROW(vehicleYearCategories))

for(i in 1:NROW(insuranceData)) {
  sex = insuranceData$P_SEX[i]
  age = insuranceData$P_AGE[i]
  vehicleType = insuranceData$V_TYPE[i]
  vehicleYear = insuranceData$V_YEAR[i]
  sexMatrix[i, which(sexCategories == sex)] = 1
  ageMatrix[i, which(ageCategories == age)] = 1
  vehicleTypeMatrix[i, which(vehicleTypeCategories == vehicleType)] = 1
  vehicleYearMatrix[i, which(vehicleYearCategories == vehicleYear)] = 1
}
categoriesMatrix = cbind(sexMatrix, ageMatrix, vehicleTypeMatrix, vehicleYearMatrix)
categoriesMatrix

# ----------------------------------------------- MODEL 1: BASE MODEL ------------------------------------------------

# In this model (base case), our only constraint is that the factors from all categories must sum to 1. 
# This model can be thought of as a demand-based model. Factors will be determined based on the frequency of 
# categorical values. 

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
# All the factors should add to at most 1
A = matrix(1, nrow = 1, ncol = numVars)

# Set the B vector
# All the factors should add to at most 1
b = matrix(1, nrow = 1, ncol = 1)

# Set the operators vector
operators = matrix('<=', nrow = 1, ncol = 1)

# Set the objective function vector
# The insurance cost for a person is $2000 + (sum of all applicable factors)*1000
# Since every person is charged $2000 regardless, it does not need to be added to our objective function
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A
model$obj = obj
model$modelsense = "max"
model$rhs = b
model$sense = operators
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variable and fixed costs
NROW(insuranceData)*fixedCost + result$objval

# ------------------------------------------- MODEL 2: FACTOR CONSTRAINTS ---------------------------------------------

# In this model, we set upper bounds for factors based on category. All factors for sex must sum to at most 0.25,
# All factors for for age must sum to at most 0.25, etc. This maintains an upper bound of 1 for the sum of all factors.
# The goal here is to not put too much emphasis on one variable. Instead, we want a more even dispersion.

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
# All the factors should add to at most 1
A2 = matrix(0, nrow = 0, ncol = numVars)

sexRow = vector("numeric", numVars)
sexRow[sexStartIndex:(ageStartIndex-1)] = 1
ageRow = vector("numeric", numVars)
ageRow[ageStartIndex:(vehicleTypeStartIndex - 1)] = 1
vehicleTypeRow = vector("numeric", numVars)
vehicleTypeRow[vehicleTypeStartIndex:(vehicleYearStartIndex - 1)] = 1
vehicleYearRow = vector("numeric", numVars)
vehicleYearRow[vehicleYearStartIndex:endIndex] = 1
A2 = rbind(sexRow, ageRow, vehicleTypeRow, vehicleYearRow)

# Set the B vector
# All the factors for a given category should add to at most 0.25 (highest possible sum of all factors is 1)
b2 = matrix(0.25, nrow = 4, ncol = 1)

# Set the operators vector
operators2 = matrix('<=', nrow = 4, ncol = 1)

# Set the objective function vector
# The insurance cost for a person is $2000 + (sum of all applicable factors)*1000
# Since every person is charged $2000 regardless, it does not need to be added to our objective function
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A2
model$obj = obj
model$modelsense = "max"
model$rhs = b2
model$sense = operators2
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variable and fixed costs
NROW(insuranceData)*fixedCost + result$objval

# ------------------------------------------ MODEL 3A: OPTIMIZING REVENUE WEIGHTED BY RISK -------------------------------------------

# In this model, we use the actual prediction values from the logistic regression. Rather than using the 
# logistic regression to derive risk rankings and constraints, we use the predictions in the objective function 
# as weights. Hence, we are trying to maximize the total sum of weighted revenues of all drivers.

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
# All the factors should add to at most 1
A3a = matrix(1, nrow = 1, ncol = numVars)

# Set the B vector
# All the factors should add to at most 1
b3a = matrix(1, nrow = 1, ncol = 1)

# Set the operators vector
operators3a = matrix('<=', nrow = 1, ncol = 1)

# Set the objective function vector
# The insurance cost for a person is $2000 + (sum of all applicable factors)*1000
# Since every person is charged $2000 regardless, it does not need to be added to our objective function
# In this model, we are trying to maximize the weighted revenue (weights are the logistic regression probabilities)
# Get predictions for drivers
driverPredictions = predict(severeLog5, type = "response", newdata = driverDataTest)
coeffs = categoriesMatrix*maxVariableCost*driverPredictions
obj = colSums(coeffs)

# Solve
model = list()
model$A = A3a
model$obj = obj
model$modelsense = "max"
model$rhs = b3a
model$sense = operators3a
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors (cannot use objective function value since it uses weighted revenue)
sum(colSums(categoriesMatrix*maxVariableCost)*(result$x))

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variables and fixed costs
NROW(insuranceData)*fixedCost + sum(colSums(categoriesMatrix*maxVariableCost)*(result$x))


# ----------------------------- MODEL 3B: OPTIMIZING REVENUE WEIGHTED BY RISK WITH FACTOR CONSTRAINTS---------------------------

# This model combines model 2 and model 3A (uses constraints from model 2 and the objective function from model 3A)

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
A3b = A2

# Set the B vector
b3b = b2

# Set the operators vector
operators3b = operators2

# Set the objective function vector
# The insurance cost for a person is $2000 + (sum of all applicable factors)*1000
# Since every person is charged $2000 regardless, it does not need to be added to our objective function
# In this model, we are trying to maximize the weighted revenue (weights are the logistic regression probabilities)
# Get predictions for drivers
driverPredictions = predict(severeLog5, type = "response", newdata = driverDataTest)
coeffs = categoriesMatrix*maxVariableCost*driverPredictions
obj = colSums(coeffs)

# Solve
model = list()
model$A = A3b
model$obj = obj
model$modelsense = "max"
model$rhs = b3b
model$sense = operators3b
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors (cannot use objective function value since it uses weighted revenue)
sum(colSums(categoriesMatrix*maxVariableCost)*(result$x))

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variables and fixed costs
NROW(insuranceData)*fixedCost + sum(colSums(categoriesMatrix*maxVariableCost)*(result$x))


# ------------------------------------------ MODEL 4A: RISK CONSTRAINTS ---------------------------------------------

# In this model we add constraints based on our logistic regression. The constraints are used to restrict our factors 
# based on ranked risks of categorical factors. If males present a higher risk of injury, we should charge them more than 
# females. The model will tell us by how much. The prior models end up optimizing in part based on frequency of categorical
# values (e.g. if there are more male customers than females, charge them more). Rather than using models that optimize 
# solely based on demand, we should also consider risk. If males present a higher risk of severe car crashes, they should not
# be charged less than females.

# Ordered from highest risk to lowest risk (numbers correspond to index within category vectors)
# These ranks are created by ranking the logistic regression coefficients of a variable's categories from highest to lowest
# Example coefficients ranked: 0.03, 0, -0.05, -0.12. Positive coefficients mean the category makes it more likely the accident 
# is severe in comparison to the reference category (which has a coefficient of 0). Negative coefficients mean the category makes 
# it less liley the accident is severe.
sexRanks = c(1,2)
ageRanks = c(9,1,5,2,3,8,4,6,7)
vehicleTypeRanks = c(1,3,2,5,6,4,7)
vehicleYearRanks = c(1,2,3,4,5,6)


# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
# All the factors should add to at most 1
A4a = matrix(1, nrow = 1, ncol = numVars)

# Set the B vector
# All the factors should add to at most 1
b4a = matrix(1, nrow = 1, ncol = 1)

# Set the operators vector
operators4a = matrix('<=', nrow = 1, ncol = 1)

# Add risk constraints based on ranks
# High risk should pay equal to or greater than low risk
for(i in 1:(NROW(sexRanks)-1)) {
  rowVector = vector("numeric", numVars)
  rowVector[sexRanks[i]] = 1
  rowVector[sexRanks[i+1]] = -1
  A4a = rbind(A4a, rowVector)
  operators4a = rbind(operators4a, ">=")
  b4a = rbind(b4a, 0)
}
for(i in 1:(NROW(ageRanks)-1)) {
  rowVector = vector("numeric", numVars)
  rowVector[ageStartIndex -1 + ageRanks[i]] = 1
  rowVector[ageStartIndex -1 + ageRanks[i+1]] = -1
  A4a = rbind(A4a, rowVector)
  operators4a = rbind(operators4a, ">=")
  b4a = rbind(b4a, 0)
}
for(i in 1:(NROW(vehicleTypeRanks)-1)) {
  rowVector = vector("numeric", numVars)
  rowVector[vehicleTypeStartIndex -1 + vehicleTypeRanks[i]] = 1
  rowVector[vehicleTypeStartIndex -1 + vehicleTypeRanks[i+1]] = -1
  A4a = rbind(A4a, rowVector)
  operators4a = rbind(operators4a, ">=")
  b4a = rbind(b4a, 0)
}
for(i in 1:(NROW(vehicleYearRanks)-1)) {
  rowVector = vector("numeric", numVars)
  rowVector[vehicleYearStartIndex -1 + vehicleYearRanks[i]] = 1
  rowVector[vehicleYearStartIndex -1 + vehicleYearRanks[i+1]] = -1
  A4a = rbind(A4a, rowVector)
  operators4a = rbind(operators4a, ">=")
  b4a = rbind(b4a, 0)
}

# Set the objective function vector
# The insurance cost for a person is $2000 + (sum of all applicable factors)*1000
# Since every person is charged $2000 regardless, it does not need to be added to our objective function
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A4a
model$obj = obj
model$modelsense = "max"
model$rhs = b4a
model$sense = operators4a
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variable and fixed costs
NROW(insuranceData)*fixedCost + result$objval


# --------------------------------------- MODEL 4B: RISK AND FACTOR CONSTRAINTS -------------------------------------------

# This model combines model 2 and model 4A (uses constraints from both)

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
A4b = rbind(A2,A4a)

# Set the B vector
b4b = rbind(b2, b4a)

# Set the operators vector
operators4b = rbind(operators2, operators4a)

# Set the objective function vector
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A4b
model$obj = obj
model$modelsense = "max"
model$rhs = b4b
model$sense = operators4b
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variables and fixed costs
NROW(insuranceData)*fixedCost + result$objval


# ----------------------------------------- MODEL 5: FAIRNESS WITHIN VARIABLES  ---------------------------------------

# In this model we add constraints for the fairness of the policy. For example, we can charge males more than females,
# but not by too much or this may be perceived as unfair by the public. We will add constraints to introduce fairness 
# within categories. No two category values should be more than 0.1 different than each other. Note that the overall 
# difference in what people pay can still be greater than 0.1*1000 because their total cost is based on multiple variables.

differenceThreshold = 0.1
  
# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
# All the factors should add to at most 1
A5 = matrix(1, nrow = 1, ncol = numVars)

# Set the B vector
# All the factors should add to at most 1
b5 = matrix(1, nrow = 1, ncol = 1)

# Set the operators vector
operators5 = matrix('<=', nrow = 1, ncol = 1)

# Add fairness constraints for each variable
# Need a constraint for each pair of category values in a variable
# Since we are dealing with abolsolute values, we need two constraints for each pair of values
for(i in 1:(NROW(sexCategories)-1)) {
  for(j in (i+1):(NROW(sexCategories))) {
    rowVector = vector("numeric", numVars)
    rowVector[i] = 1
    rowVector[j] = -1
    A5 = rbind(A5, rowVector, rowVector)
    operators5 = rbind(operators5, "<=", ">=")
    b5 = rbind(b5, differenceThreshold, (-1)*differenceThreshold)
  }
}
for(i in 1:(NROW(ageCategories)-1)) {
  for(j in (i+1):(NROW(ageCategories))) {
    rowVector = vector("numeric", numVars)
    rowVector[ageStartIndex -1 + i] = 1
    rowVector[ageStartIndex -1 + j] = -1
    A5 = rbind(A5, rowVector, rowVector)
    operators5 = rbind(operators5, "<=", ">=")
    b5 = rbind(b5, differenceThreshold, (-1)*differenceThreshold)
  }
}
for(i in 1:(NROW(vehicleTypeCategories)-1)) {
  for(j in (i+1):(NROW(vehicleTypeCategories))) {
    rowVector = vector("numeric", numVars)
    rowVector[vehicleTypeStartIndex -1 + i] = 1
    rowVector[vehicleTypeStartIndex -1 + j] = -1
    A5 = rbind(A5, rowVector, rowVector)
    operators5 = rbind(operators5, "<=", ">=")
    b5 = rbind(b5, differenceThreshold, (-1)*differenceThreshold)
  }
}
for(i in 1:(NROW(vehicleYearCategories)-1)) {
  for(j in (i+1):(NROW(vehicleYearCategories))) {
    rowVector = vector("numeric", numVars)
    rowVector[vehicleYearStartIndex -1 + i] = 1
    rowVector[vehicleYearStartIndex -1 + j] = -1
    A5 = rbind(A5, rowVector, rowVector)
    operators5 = rbind(operators5, "<=", ">=")
    b5 = rbind(b5, differenceThreshold, (-1)*differenceThreshold)
  }
}


# Set the objective function vector
# The insurance cost for a person is $2000 + (sum of all applicable factors)*1000
# Since every person is charged $2000 regardless, it does not need to be added to our objective function
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A5
model$obj = obj
model$modelsense = "max"
model$rhs = b5
model$sense = operators5
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variables and fixed costs
NROW(insuranceData)*fixedCost + result$objval


# --------------------------------------- MODEL 6: FAIRNESS BETWEEN ALL CATEGORIES  -------------------------------------

# This model is similar to model 5, but in this model, we consider fairness between all categories, rather than just 
# within variables. In model 5, we only disallowed differences greater than 0.1 within the variable. In this model,
# we will disallow differences greater than 0.1 within the entire problem (i.e. no two categories may have a difference
# in factors that is greater than 0.1 irresepctive of which variables the categories are associated with)

differenceThreshold = 0.1

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
# All the factors should add to at most 1
A6 = matrix(1, nrow = 1, ncol = numVars)

# Set the B vector
# All the factors should add to at most 1
b6 = matrix(1, nrow = 1, ncol = 1)

# Set the operators vector
operators6 = matrix('<=', nrow = 1, ncol = 1)

# Add fairness constraints for all categories
# Need a constraint for each pair of category values
# Since we are dealing with abolsolute values, we need two constraints for each pair
for(i in 1:(numVars-1)) {
  for(j in (i+1):numVars) {
    rowVector = vector("numeric", numVars)
    rowVector[i] = 1
    rowVector[j] = -1
    A6 = rbind(A6, rowVector, rowVector)
    operators6 = rbind(operators6, "<=", ">=")
    b6 = rbind(b6, differenceThreshold, (-1)*differenceThreshold)
  }
}

# Set the objective function vector
# The insurance cost for a person is $2000 + (sum of all applicable factors)*1000
# Since every person is charged $2000 regardless, it does not need to be added to our objective function
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A6
model$obj = obj
model$modelsense = "max"
model$rhs = b6
model$sense = operators6
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variables and fixed costs
NROW(insuranceData)*fixedCost + result$objval


# ----------------------------- MODEL 5B: FAIRNESS WITHIN VARIABLES AND FACTOR CONSTRAINTS  ------------------------------


# ---------------------------- MODEL 6B: FAIRNESS BETWEEN ALL CATEGORIES AND FACTOR CONSTRAINTS  ------------------------------


# ---------------------------------------------- MODEL 7: COMBINATION -------------------------------------------------


# ---------------------------------- MODEL 7: FAIRNESS, RISK, AND FACTOR CONSTRAINTS -------------------------------------------------


# --------------------------- MODEL 7A: FAIRNESS BETWEEN ALL CATEGORIES WITH RISK CONSTRAINTS --------------------------------------------

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
A7a = rbind(A6, A4a)

# Set the B vector
b7a = rbind(b6, b4a)

# Set the operators vector
operators7a = rbind(operators6, operators4a)

# Set the objective function vector
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A7a
model$obj = obj
model$modelsense = "max"
model$rhs = b7a
model$sense = operators7a
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variables and fixed costs
NROW(insuranceData)*fixedCost + result$objval

# --------------------------- MODEL 7B: FAIRNESS BETWEEN ALL CATEGORIES WITH RISK AND FACTOR CONSTRAINTS --------------------------------------------

# Set the variable types
vtype = matrix('C', nrow = 1, ncol = numVars)

# Set the A matrix 
A7b = rbind(A6, A4a, A2)

# Set the B vector
b7b = rbind(b6, b4a, b2)

# Set the operators vector
operators7b = rbind(operators6, operators4a, operators2)

# Set the objective function vector
coeffs = categoriesMatrix*maxVariableCost
obj = colSums(coeffs)

# Solve
model = list()
model$A = A7b
model$obj = obj
model$modelsense = "max"
model$rhs = b7b
model$sense = operators7b
model$vtype = vtype
result = gurobi(model)

# Resulting factors
result$x

# Total variable costs based on decided factors
result$objval

# Total fixed costs (each person pays $2000)
NROW(insuranceData)*fixedCost

# Get total revenue including both the variables and fixed costs
NROW(insuranceData)*fixedCost + result$objval
