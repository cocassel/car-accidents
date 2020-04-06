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

nrow(data)
data<-data[!(data$C_MNTH=="UU"),] #data cleaning
nrow(data)
data<-data[!(data$C_WDAY =="U"),] #data cleaning
nrow(data)
data<-data[!(data$C_HOUR=="UU"),] #data cleaning
nrow(data)
data<-data[!(data$C_VEHS =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$C_CONF =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$C_CONF =="QQ"),] #data cleaning
nrow(data)
data<-data[!(data$C_RCFG =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$C_RCFG =="QQ"),] #data cleaning
nrow(data)
data<-data[!(data$C_WTHR =="U"),] #data cleaning
nrow(data)
data<-data[!(data$C_WTHR =="Q"),] #data cleaning
nrow(data)
data<-data[!(data$C_RSUR =="U"),] #data cleaning
nrow(data)
data<-data[!(data$C_RSUR =="Q"),] #data cleaning
nrow(data)
data<-data[!(data$C_RALN =="U"),] #data cleaning
nrow(data)
data<-data[!(data$C_RALN =="Q"),] #data cleaning
nrow(data)
data<-data[!(data$C_TRAF =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$C_TRAF =="QQ"),] #data cleaning
nrow(data)
data<-data[!(data$V_ID =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$V_TYPE =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$V_TYPE =="QQ"),] #data cleaning
nrow(data)
data<-data[!(data$V_TYPE =="NN"),] #data cleaning
nrow(data)
data<-data[!(data$V_YEAR =="UUUU"),] #data cleaning
nrow(data)
data<-data[!(data$V_YEAR =="NNNN"),] #data cleaning
nrow(data)
data<-data[!(data$P_ID =="NN"),] #data cleaning
nrow(data)
data<-data[!(data$P_AGE =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$P_AGE =="NN"),] #data cleaning
nrow(data)
data<-data[!(data$P_PSN =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$P_PSN =="QQ"),] #data cleaning
nrow(data)
data<-data[!(data$P_PSN =="NN"),] #data cleaning
nrow(data)
data<-data[!(data$P_ISEV =="U"),] #data cleaning
nrow(data)
data<-data[!(data$P_ISEV =="N"),] #data cleaning
nrow(data)
data<-data[!(data$P_SAFE =="UU"),] #data cleaning
nrow(data)
data<-data[!(data$P_SAFE =="QQ"),] #data cleaning
nrow(data)
data<-data[!(data$P_SAFE =="NN"),] #data cleaning
nrow(data)
data<-data[!(data$P_USER =="U"),] #data cleaning
nrow(data)


split = sample.split(data$C_SEV, SplitRatio = 0.7)
dataTrain = subset(data, split == TRUE)   # Observations to be put in the training set           
dataTest = subset(data, split == FALSE)  # Observations to be put in the testing set 
nrow(dataTrain)
nrow(dataTest)

severeLog = glm(C_SEV ~ P_AGE + P_SEX, data = dataTrain, family = binomial) 
summary(severeLog)