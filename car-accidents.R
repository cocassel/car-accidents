#install.packages("caTools")
library(caTools) 
library(gurobi)

############################################ DATA PREPARATION ############################################

#setwd("/Users/Ahmad Dakhqan/Desktop/MSCI 433/Project/")
setwd("/Users/Celeste/Documents/GitHub/car-accidents/")
data=read.csv("data.csv")

# Replace 2s with 0s. End up with 1s as fatalities and 0s as non-fatal
data$C_SEV[data$C_SEV == 2] = 0

split = sample.split(data$C_SEV, SplitRatio = 0.7)
dataTrain = subset(data, split == TRUE)   # Observations to be put in the training set           
dataTest = subset(data, split == FALSE)  # Observations to be put in the testing set 
nrow(dataTrain)
nrow(dataTest)

qualityLog = glm(C_SEV ~ P_AGE + P_SEX, data = dataTrain, family = binomial) 
