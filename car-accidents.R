rm(list=ls())
library(caTools) 
library(gurobi)

#setwd("/Users/Ahmad Dakhqan/Desktop/MSCI 433/Project/car-accidents/")
setwd("/Users/Celeste/Documents/GitHub/car-accidents/")

############################################ DATA PREPARATION ############################################

# data = read.csv("NCDB_1999_to_2014.csv")
# n = nrow(data)
# m = n - 1000000 + 1
# data = data[m:n,]
# write.csv("data.csv")

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

# data cleaning: C_VEHS
data = data[data$C_VEHS != "UU",]
data = data[data$C_VEHS != "XX",]
# Make vehicle collided-group categories rather than usually indidvidual number of cars in collision
data$C_VEHS = as.numeric(data$C_VEHS)
data$C_VEHS[data$C_VEHS > 1 & data$C_VEHS <= 5] = 2
data$C_VEHS[data$C_VEHS > 5 & data$C_VEHS <= 25] = 3
data$C_VEHS[data$C_VEHS > 25 & data$C_VEHS <= 98] = 4
data$C_VEHS[data$C_VEHS > 99] = 5

# data cleaning: C_CONF
data = data[data$C_CONF != "QQ",]
data = data[data$C_CONF != "UU",]
data = data[data$C_CONF != "XX",]
data$C_CONF = as.numeric(as.character(data$C_CONF))
data$C_CONF[data$C_CONF >= 1 & data$C_CONF <= 6] = 1
data$C_CONF[data$C_CONF >= 21 & data$C_CONF <= 25] = 21
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

# data cleaning: V_ID 
data = data[data$V_ID != "UU",]

# data cleaning: V_TYPE
data = data[data$V_TYPE != "QQ",] 
data = data[data$V_TYPE != "UU",] 
data = data[data$V_TYPE != "NN",] 

# data cleaning: V_YEAR
data = data[data$V_YEAR != "NNNN",] 
data = data[data$V_YEAR != "UUUU",] 
data = data[data$V_YEAR != "QQQQ",]
# Make vehicle year-group categories rather than usually indidvidual years
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

# data cleaning: P_AGE
data = data[data$P_AGE != "NN",]
data = data[data$P_AGE != "UU",]
data = data[data$P_AGE != "XX",]
# Make age-group categories rather than usually indidvidual ages
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

# data cleaning: P_PSN
data = data[data$P_PSN != "NN",]
data = data[data$P_PSN != "QQ",]
data = data[data$P_PSN != "UU",]
data = data[data$P_PSN != "XX",]

# data cleaning: P_SAFE
data = data[data$P_SAFE != "NN",]
data = data[data$P_SAFE != "QQ",]
data = data[data$P_SAFE != "UU",]
data = data[data$P_SAFE != "XX",]

# data cleaning: P_USER
data = data[data$P_USER != "U",] 

# Write cleaned data to CSV
write.csv(data, "cleanedData.csv")

########################################## LOGISTIC REGRESSION ############################################

# Read in CSV
data = read.csv("cleanedData.csv")
# Treat all data as categorical, not numerical
data$P_ISEV = as.factor(data$P_ISEV)
data$P_SEX = as.factor(data$P_SEX)
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
data$P_AGE = as.factor(data$P_AGE)
data$P_PSN = as.factor(data$P_PSN)
data$P_SAFE = as.factor(data$P_SAFE)
data$P_USER = as.factor(data$P_USER)


split = sample.split(data$P_ISEV, SplitRatio = 0.7)
dataTrain = subset(data, split == TRUE)   # Observations to be put in the training set           
dataTest = subset(data, split == FALSE)  # Observations to be put in the testing set 
nrow(dataTrain)
nrow(dataTest)

# --------------------------------------------- BUILDING MODELS --------------------------------------------------

# Model 1: All variables
# C_SEV is not included because the severity of the crash should not be used to predict the severity of a person in the crash
# V_ID is a sequence number, so it is not included 
# P_ID is a sequence number, so it is not included
severeLog1 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                  C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog1)

# Model 2: Collison Info
# C_SEV is not included because the severity of the crash should not be used to predict the severity of a person in the crash
severeLog2 = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                  C_RSUR + C_RALN + C_TRAF, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog2)


# Model 3: Vehicle Info
# V_ID is a sequence number, so it is not included 
severeLog3 = glm(P_ISEV ~  V_TYPE + V_YEAR, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog3)


# Model 4: Person Info
# P_ID is a sequence number, so it is not included
severeLog4 = glm(P_ISEV ~ P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog4)


# --------------------------------------------- FINAL MODELS --------------------------------------------------


# Get the number of injured vs not
injuredFreq = as.data.frame(table(data$P_ISEV))
injuredFreq

# Get baseline accuracy
if(injuredFreq$Freq[2] > injuredFreq$Freq[1]) {
  injuredFreq$Freq[2]/(injuredFreq$Freq[1] + injuredFreq$Freq[2])
} else {
  injuredFreq$Freq[1]/(injuredFreq$Freq[1] + injuredFreq$Freq[2])
}

predictTrain = predict(severeLog1, type = "response") 
trainConfMatrix = table(dataTrain$P_ISEV, predictTrain>0.5)
trainConfMatrix
trainConfMatrix = as.data.frame(trainConfMatrix)

predictTest = predict(severeLog1, type = "response", newdata = dataTest)
testConfMatrix = table(dataTest$P_ISEV, predictTest>0.5) 
testConfMatrix
testConfMatrix = as.data.frame(testConfMatrix)
