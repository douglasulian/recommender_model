options(warn = -1)
#### Install Packages ####
list.of.packages = c('tidyverse','dplyr','compiler','profvis','SPOT')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

#### Libraries ####
# Loads tidyverse tools
library(tidyverse)
library(dplyr)

# Loads compiler to user JIT
library(compiler)

library(profvis)

library(reshape2)

#### Import Functions ####
debugSource('scripts/evaluation.R')
debugSource('scripts/dataBase.R')
debugSource('input.R')
#### Main ####
enableJIT(0)

clearConnections()

#trainningConnection = getDataBaseConnection(schema = 'trainning2000' ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
trainningConnection = getDataBaseConnection(schema = schema,
                                            dbUser = dbUser,
                                            dbHost = dbHost,
                                            dbName = dbName,
                                            dbPass = dbPass)
# number of clusters must be less then or equal to the number of users available for clustering.
# 2000 = 1021
# 200 = 104
# results = list()
# 

trainningData = getTrainningData(trainningConnection)

result = executeModel(trainningData    = trainningData,
               clusterMethod           = clusterMethod,
               noClustersK             = noClustersK,
               usersTimeDiffAlphaIndex = usersTimeDiffAlphaIndex,
               mixedDistanceBetaIndex  = mixedDistanceBetaIndex,
               forgCurveLambdaIndex    = forgCurveLambdaIndex,
               tagsCutGamaIndex        = tagsCutGamaIndex)

writeClustersToDB(result$clusters, trainningConnection)

writeClustersProfilesToDB(result$clustersProfiles, trainningConnection)

clearConnections()
