options(warn = -1)
#### Install Packages ####
list.of.packages = c('tidyverse','dplyr','compiler','profvis','SPOT')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

#### Libraries ####
# Loads tidyverse tools
library(tidyverse)
library(dplyr)
library(compiler)
library(profvis)
library(SPOT)

#### Import Functions ####
# debugSource('scripts/stemmer.R')
debugSource('scripts/util.R')
debugSource('scripts/dataBase.R')
debugSource('scripts/distances.R')
debugSource('scripts/cluster.R')
debugSource('scripts/plots.R')
debugSource('scripts/evaluation.R')

#### Main ####
enableJIT(3)

# number of clusters must be less then or equal to the number of users available for clustering.
#            6000  2000   200
# articles   6906  8140  2272
# tags      16550 10849  4588
# users            1021   104

bounds = tribble(
  ~clusterMethod , ~bound , ~tagsMethod, ~articlesMethod, ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex
  ,'kmedoid'     , 'lower', 'topn'     , 'topn'         ,  1.0             ,  1                   ,  0.00                ,  0.00                  , 0.0                    
  ,'kmedoid'     , 'upper', 'topn'     , 'topn'         ,  10849           ,  8140                ,  1.00                ,  1.00                  , 1.0                    
  ,'kmedoid'     , 'lower', 'index'    , 'topn'         ,  0.0             ,  1                   ,  0.00                ,  0.00                  , 0.0                    
  ,'kmedoid'     , 'upper', 'index'    , 'topn'         ,  1.0             ,  8140                ,  1.00                ,  1.00                  , 1.0                    
  ,'kmedoid'     , 'lower', 'index'    , 'index'        ,  0.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    
  ,'kmedoid'     , 'upper', 'index'    , 'index'        ,  1.0             ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    
  ,'kmedoid'     , 'lower', 'topn'     , 'index'        ,  1.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    
  ,'kmedoid'     , 'upper', 'topn'     , 'index'        ,  10849           ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    
  ,'ward.D'      , 'lower', 'topn'     , 'topn'         ,  1.0             ,  1                   ,  0.00                ,  0.00                  , 0.0                    
  ,'ward.D'      , 'upper', 'topn'     , 'topn'         ,  10849           ,  8140                ,  1.00                ,  1.00                  , 1.0                    
  ,'ward.D'      , 'lower', 'index'    , 'topn'         ,  0.0             ,  1                   ,  0.00                ,  0.00                  , 0.0                    
  ,'ward.D'      , 'upper', 'index'    , 'topn'         ,  1.0             ,  8140                ,  1.00                ,  1.00                  , 1.0                    
  ,'ward.D'      , 'lower', 'index'    , 'index'        ,  0.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    
  ,'ward.D'      , 'upper', 'index'    , 'index'        ,  1.0             ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    
  ,'ward.D'      , 'lower', 'topn'     , 'index'        ,  1.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    
  ,'ward.D'      , 'upper', 'topn'     , 'index'        ,  10849           ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    
)

cacheFile      = 'cache.RData'
cacheDir       = 'cache//2000_silhouette'

clearConnections()

trainningConnection = getDataBaseConnection(schema = 'trainning2000', dbUser = "cl-us-gzh", dbHost = "10.238.4.109", dbName = "cl-us-gzh", dbPass = "cl-us-gzh")
testingConnection   = getDataBaseConnection(schema = 'testing2000'  , dbUser = "cl-us-gzh", dbHost = "10.238.4.109", dbName = "cl-us-gzh", dbPass = "cl-us-gzh")
spotResults         = list()
experimentTimes     = list()
printHeader2()
for (i in 5:8) {
  experimentTime = system.time({
    spotResult = spot(fun                 = spotTest2,
                      lower               = as.matrix(bounds[i*2 - 1,5:9]),
                      upper               = as.matrix(bounds[i*2    ,5:9]),
                      control             = list(funEvals = 40),
                      clusterMethod       = as.character(bounds[i*2,1]),
                      tagsMethod          = as.character(bounds[i*2,3]),
                      articlesMethod      = as.character(bounds[i*2,4]),
                      noUsers             = 2000, 
                      trainningConnection = trainningConnection,
                      testingConnection   = testingConnection,
                      cacheFile           = cacheFile,
                      cacheDir            = cacheDir
                      
    )
  })
  experimentTimes[[length(experimentTimes) + 1]] = experimentTime
  spotResults[[length(spotResults) + 1]] = spotResult
}
clearConnections()

# min(unlist(lapply(X = spotResults, FUN = function(x) x$ybest)))
# [1] -0.2601
# > 

