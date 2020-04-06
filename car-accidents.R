rm(list=ls())
library(caTools) 
library(gurobi)

############################################ DATA PREPARATION ############################################

#setwd("/Users/Ahmad Dakhqan/Desktop/MSCI 433/Project/car-accidents/")
setwd("/Users/Celeste/Documents/GitHub/car-accidents/")
data=read.csv("data.csv")

# data cleaning: P_ISEV
data = data[data$P_ISEV != "N",]
data = data[data$P_ISEV != "U",]
data = data[data$P_ISEV != "X",]
data$P_ISEV = as.numeric(data$P_ISEV)
# Since what we want to classify is fatal vs non-fatal, use 0s for non-injured and injured. Use 1s for fatalities
data$P_ISEV[data$P_ISEV == 1 | data$P_ISEV == 2] = 0
data$P_ISEV[data$P_ISEV == 3] = 1
data$P_ISEV = as.factor(data$P_ISEV)

# data cleaning: P_SEX
data = data[data$P_SEX != "N",]
data = data[data$P_SEX != "U",] 
data$P_SEX = as.factor(data$P_SEX)

# data cleaning: C_MNTH
data = data[data$C_MNTH != "UU",]
data = data[data$C_MNTH != "XX",]
data$C_MNTH = as.factor(data$C_MNTH)

# data cleaning: C_WDAY
data = data[data$C_WDAY != "U",] 
data = data[data$C_WDAY != "X",] 
data$C_WDAY = as.factor(data$C_WDAY)

# data cleaning: C_HOUR
data = data[data$C_HOUR != "UU",]
data = data[data$C_HOUR != "XX",]
data$C_HOUR = as.factor(data$C_HOUR)

# data cleaning: C_VEHS
data = data[data$C_VEHS != "UU",]
data = data[data$C_VEHS != "XX",]
data$C_VEHS = as.factor(data$C_VEHS)

# data cleaning: C_CONF
data = data[data$C_CONF != "QQ",]
data = data[data$C_CONF !="UU",]
data = data[data$C_CONF != "XX",]
data$C_CONF = as.factor(data$C_CONF)

# data cleaning: C_RCFG
data = data[data$C_RCFG !=  "QQ",]
data = data[data$C_RCFG !=  "UU",] 
data = data[data$C_RCFG !=  "XX",]
data$C_RCFG = as.factor(data$C_RCFG)

# data cleaning: C_WTHR
data = data[data$C_WTHR !=  "Q",]
data = data[data$C_WTHR !=  "U",]
data = data[data$C_WTHR !=  "X",]
data$C_WTHR = as.factor(data$C_WTHR)

# data cleaning: C_RSUR
data = data[data$C_RSUR !=  "Q",]
data = data[data$C_RSUR !=  "U",]
data = data[data$C_RSUR !=  "X",]
data$C_RSUR = as.factor(data$C_RSUR)

# data cleaning: C_RALN 
data = data[data$C_RALN != "Q",] 
data = data[data$C_RALN != "U",] 
data = data[data$C_RALN != "X",] 
data$C_RALN = as.factor(data$C_RALN)

# data cleaning: C_TRAF 
data = data[data$C_TRAF != "QQ",]
data = data[data$C_TRAF != "UU",]
data = data[data$C_TRAF != "XX",]
data$C_TRAF = as.factor(data$C_TRAF)

# data cleaning: V_ID 
data = data[data$V_ID != "UU",]
data$V_ID = as.factor(data$V_ID)

# data cleaning: V_TYPE
data = data[data$V_TYPE != "QQ",] 
data = data[data$V_TYPE != "UU",] 
data = data[data$V_TYPE != "NN",] 
nrow(data)
data$V_TYPE = as.factor(data$V_TYPE)

# data cleaning: V_YEAR
data = data[data$V_YEAR != "NNNN",] 
data = data[data$V_YEAR != "UUUU",] 
data = data[data$V_YEAR != "QQQQ",] 
data$V_YEAR = as.factor(data$V_YEAR)

# data cleaning: P_ID
data = data[data$P_ID != "NN",]
data = data[data$P_ID != "UU",]
data$P_ID = as.factor(data$P_ID)

# data cleaning: P_AGE
data = data[data$P_AGE != "NN",]
data = data[data$P_AGE != "UU",]
data = data[data$P_AGE != "XX",]
data$P_AGE = as.factor(data$P_AGE)

# data cleaning: P_PSN
data = data[data$P_PSN != "NN",]
data = data[data$P_PSN != "QQ",]
data = data[data$P_PSN != "UU",]
data = data[data$P_PSN != "XX",]
data$P_PSN = as.factor(data$P_PSN)

# data cleaning: P_SAFE
data = data[data$P_SAFE != "NN",]
data = data[data$P_SAFE != "QQ",]
data = data[data$P_SAFE != "UU",]
data = data[data$P_SAFE != "XX",]
data$P_SAFE = as.factor(data$P_SAFE)

# data cleaning: P_USER
data = data[data$P_USER != "U",] 
data$P_USER = as.factor(data$P_USER)

# Write cleaned data to CSV
write.csv(data, "cleanedData.csv")

########################################## LOGISTIC REGRESSION ############################################

# Read in CSV
data = read.csv("cleanedData.csv")

split = sample.split(data$P_ISEV, SplitRatio = 0.7)
dataTrain = subset(data, split == TRUE)   # Observations to be put in the training set           
dataTest = subset(data, split == FALSE)  # Observations to be put in the testing set 
nrow(dataTrain)
nrow(dataTest)

# C_SEV is not included because the severity of the crash should not be used to predict the severity of a person in the crash
# V_ID is a sequence number, so it is not included 
# P_ID is a sequence number, so it is not included
severeLog = glm(P_ISEV ~  C_YEAR + C_MNTH + C_WDAY + C_HOUR + C_VEHS + C_CONF + C_RCFG + C_WTHR + 
                  C_RSUR + C_RALN + C_TRAF + V_TYPE + V_YEAR + P_SEX + P_AGE + P_PSN + P_SAFE + P_USER, 
                data = dataTrain, family = binomial(link = "logit")) 
summary(severeLog)
