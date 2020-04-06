#rm(list=ls())
#install.packages("caTools")
library(caTools) 
library(gurobi)

############################################ DATA PREPARATION ############################################

#setwd("/Users/Ahmad Dakhqan/Desktop/MSCI 433/Project/car-accidents/")
setwd("/Users/Celeste/Documents/GitHub/car-accidents/")
data=read.csv("data.csv")

# Replace 2s with 0s. End up with 1s as fatalities and 0s as non-fatal
data$C_SEV[data$C_SEV == 2] = 0

#data cleaning: C_MNTH
data<-data[!(data$C_MNTH=="UU"),]
data<-data[!(data$C_MNTH=="XX"),]
nrow(data)
#data cleaning: C_WDAY
data<-data[!(data$C_WDAY =="U"),] 
data<-data[!(data$C_WDAY =="X"),] 
nrow(data)
#data cleaning: C_HOUR
data<-data[!(data$C_HOUR=="UU"),]
data<-data[!(data$C_HOUR=="XX"),]
nrow(data)
#data cleaning: C_VEHS
data<-data[!(data$C_VEHS =="UU"),]
data<-data[!(data$C_VEHS =="XX"),]
nrow(data)
#data cleaning: C_CONF
data<-data[!(data$C_CONF =="QQ"),]
data<-data[!(data$C_CONF =="UU"),]
data<-data[!(data$C_CONF =="XX"),]
nrow(data)
#data cleaning: C_RCFG
data<-data[!(data$C_RCFG =="QQ"),]
data<-data[!(data$C_RCFG =="UU"),] 
data<-data[!(data$C_RCFG =="XX"),]
nrow(data)
#data cleaning: C_WTHR
data<-data[!(data$C_WTHR =="Q"),]
data<-data[!(data$C_WTHR =="U"),]
data<-data[!(data$C_WTHR =="X"),]
nrow(data)
#data cleaning: C_RSUR
data<-data[!(data$C_RSUR =="Q"),]
data<-data[!(data$C_RSUR =="U"),]
data<-data[!(data$C_RSUR =="X"),]
nrow(data)
#data cleaning: C_RALN 
data<-data[!(data$C_RALN =="Q"),] 
data<-data[!(data$C_RALN =="U"),] 
data<-data[!(data$C_RALN =="X"),] 
nrow(data)
#data cleaning: C_TRAF 
data<-data[!(data$C_TRAF =="QQ"),]
data<-data[!(data$C_TRAF =="UU"),]
data<-data[!(data$C_TRAF =="XX"),]
nrow(data)
#data cleaning: V_ID 
data<-data[!(data$V_ID =="UU"),]
nrow(data)
#data cleaning: V_TYPE
data<-data[!(data$V_TYPE =="QQ"),] 
data<-data[!(data$V_TYPE =="UU"),] 
data<-data[!(data$V_TYPE =="NN"),] 
nrow(data)
#data cleaning: V_YEAR
data<-data[!(data$V_YEAR =="NNNN"),] 
data<-data[!(data$V_YEAR =="UUUU"),] 
data<-data[!(data$V_YEAR =="QQQQ"),] 
nrow(data)
#data cleaning: P_ID
data<-data[!(data$P_ID =="NN"),]
data<-data[!(data$P_ID =="UU"),]
nrow(data)
#data cleaning: P_AGE
data<-data[!(data$P_AGE =="NN"),]
data<-data[!(data$P_AGE =="UU"),]
data<-data[!(data$P_AGE =="XX"),]
nrow(data)
#data cleaning: P_PSN
data<-data[!(data$P_PSN =="NN"),]
data<-data[!(data$P_PSN =="QQ"),]
data<-data[!(data$P_PSN =="UU"),]
data<-data[!(data$P_PSN =="XX"),]
nrow(data)
#data cleaning: P_ISEV
data<-data[!(data$P_ISEV =="N"),]
data<-data[!(data$P_ISEV =="U"),]
data<-data[!(data$P_ISEV =="X"),]
nrow(data)
#data cleaning: P_SAFE
data<-data[!(data$P_SAFE =="NN"),]
data<-data[!(data$P_SAFE =="QQ"),]
data<-data[!(data$P_SAFE =="UU"),]
data<-data[!(data$P_SAFE =="XX"),]
nrow(data)
#data cleaning: P_USER
data<-data[!(data$P_USER =="U"),] #data cleaning
nrow(data)


split = sample.split(data$C_SEV, SplitRatio = 0.7)
dataTrain = subset(data, split == TRUE)   # Observations to be put in the training set           
dataTest = subset(data, split == FALSE)  # Observations to be put in the testing set 
nrow(dataTrain)
nrow(dataTest)

severeLog = glm(C_SEV ~ P_AGE + P_SEX, data = dataTrain, family = binomial) 
summary(severeLog)