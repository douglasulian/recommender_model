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
debugSource('scripts/cache.R')
debugSource('scripts/cluster.R')
debugSource('scripts/dataBase.R')
debugSource('scripts/distances.R')
debugSource('scripts/evaluation.R')
debugSource('scripts/plots.R')
debugSource('scripts/util.R')

#### Main ####
enableJIT(3)

# number of clusters must be less then or equal to the number of users available for clustering.
#            6000  2000   200
# articles   6681  8140  2272
# tags       4941 10849  4588
# users            1021   104
experiments = list(
  list(clusterMethod  = 'kmedoid', 
       tagsMethod     = 'index',
       articlesMethod = 'index', 
       fileName = '', 
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  0.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    ,  2
                                 , 'upper',  1.0             ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    ,  round(sqrt(6000)))),
  
  
  list(clusterMethod  = 'ward.D' , 
       tagsMethod     = 'index', 
       articlesMethod = 'index', 
       fileName = '', 
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  0.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    ,  2
                                 , 'upper',  1.0             ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    ,  round(sqrt(6000)))),
  list(clusterMethod  = 'kmedoid', 
       tagsMethod     = 'topn' , 
       articlesMethod = 'topn' , 
       fileName = '', 
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  1.0             ,  1                   ,  0.00                ,  0.00                   , 0.0                    ,  2
                                 , 'upper',  4941            ,  6681                ,  1.00                ,  1.00                   , 1.0                    ,  round(sqrt(6000)))),
  
  list(clusterMethod  = 'kmedoid', 
       tagsMethod     = 'index', 
       articlesMethod = 'topn' , 
       fileName = '',
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  0.0             ,  1                   ,  0.00                ,  0.00                  , 0.0                    ,  2
                                 , 'upper',  1.0             ,  6681                ,  1.00                ,  1.00                  , 1.0                    ,  round(sqrt(6000)))),
  
  list(clusterMethod  = 'kmedoid', 
       tagsMethod     = 'topn' , 
       articlesMethod = 'index', 
       fileName = '',
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  1.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    ,  2
                                 , 'upper',  4941            ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    ,  round(sqrt(6000)))),
  
  list(clusterMethod  = 'ward.D' , 
       tagsMethod     = 'topn' , 
       articlesMethod = 'topn' , 
       fileName = '',
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  1.0             ,  1                   ,  0.00                ,  0.00                  , 0.0                    ,  2
                                 , 'upper',  4941            ,  6681                ,  1.00                ,  1.00                  , 1.0                    ,  round(sqrt(6000)))),
  
  list(clusterMethod  = 'ward.D' , 
       tagsMethod     = 'index', 
       articlesMethod = 'topn' , 
       fileName = '',
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  0.0             ,  1                   ,  0.00                ,  0.00                  , 0.0                    ,  2
                                 , 'upper',  1.0             ,  6681                ,  1.00                ,  1.00                  , 1.0                    ,  round(sqrt(6000)))),
  list(clusterMethod  = 'ward.D' , 
       tagsMethod     = 'topn' , 
       articlesMethod = 'index', 
       fileName = '',
       funEvals = 200,
       bounds         = tribble( ~bound , ~tagsCutGamaIndex, ~articlesCutZetaIndex, ~forgCurveLambdaIndex, ~usersTimeDiffAlphaIndex, ~mixedDistanceBetaIndex, ~noClustersK
                                 , 'lower',  1.0             ,  0.0                 ,  0.00                ,  0.00                  , 0.0                    ,  2
                                 , 'upper',  4941            ,  1.0                 ,  1.00                ,  1.00                  , 1.0                    ,  round(sqrt(6000)))))


cacheFile      = 'cache.RData'
cacheDir       = 'cache//6000_all_clusters'

clearConnections()

trainningConnection = getDataBaseConnection(schema = 'trainning6000', dbUser = "cl-us-gzh", dbHost = "10.238.4.109", dbName = "cl-us-gzh", dbPass = "cl-us-gzh")
testingConnection   = getDataBaseConnection(schema = 'testing6000'  , dbUser = "cl-us-gzh", dbHost = "10.238.4.109", dbName = "cl-us-gzh", dbPass = "cl-us-gzh")

testingData         = getTestingData(testingConnection)
trainningData       = getTrainningData(trainningConnection)

spotResults         = list()
experimentTimes     = list()

printHeader()
result = tryCatch({
  for (i in 1:length(experiments)) {
    nextExperiment = experiments[[i]]
    fileName = getExperimentFileName(nextExperiment)
  
    if (file.exists(paste0(cacheDir,'//',fileName))) {
      
      load(file = paste0(cacheDir,'//',fileName))
      printCache(x = experiment$currentResult$x, 
                 y = experiment$currentResult$y,
                 clusterMethod  = experiment$clusterMethod,
                 tagsMethod     = experiment$tagsMethod,
                 articlesMethod = experiment$articlesMethod)
      
      if (nextExperiment$funEvals > nrow(experiment$currentResult$x)) {
        
        if (nrow(experiment$currentResult$x) > 0) {
  
          upperBound = experiment$bounds %>% filter(bound == 'upper')
          lowerBound = experiment$bounds %>% filter(bound == 'lower')
        
          alreadyRun = nrow(experiment$currentResult$x)
          runs       = nextExperiment$funEvals
  
          firstRest  = 10 - (alreadyRun %% 10)
          secondRest = runs %% 10
          full       = (runs - (alreadyRun + firstRest + secondRest)) %/% 10
          times      = 0
          
          if (firstRest > 0)
            times = times + 1
          
          if (secondRest > 0 && full >= 0 )
            times = times + 1
          
          if (full > 0)
            times = times + full
          
          for (i in 1:times) {
            if (i <= full + 1)
              nextRun = (alreadyRun + firstRest + ((i - 1) * 10))
            else 
              nextRun = (alreadyRun + firstRest + ((i - 2) * 10) + secondRest)
          
            experimentTime = system.time({
              spotResult = spotLoop(fun            = spotTest,
                                    x              = experiment$currentResult$x,
                                    y              = experiment$currentResult$y,
                                    trainningData  = trainningData,
                                    testingData    = testingData,
                                    cacheFile      = cacheFile,
                                    cacheDir       = cacheDir,
                                    clusterMethod  = as.character(experiment$clusterMethod),
                                    tagsMethod     = as.character(experiment$tagsMethod),
                                    articlesMethod = as.character(experiment$articlesMethod),
                                    lower          = as.matrix(lowerBound[1,2:7]),
                                    upper          = as.matrix(upperBound[1,2:7]),
                                    control        = list(funEvals = nextRun, seedSPOT = 1)
                                
                                
              )
            })
            experiment$currentResult = spotResult
            experiment$experimentTime = experiment$experimentTime + experimentTime
            
            save(list = c('experiment'),file = paste0(cacheDir,'//',experiment$fileName))
            
            sendEmail(subject = paste0('Experiment ',substr(experiment$fileName,start = 5,stop = 7), ' up to ',nrow(spotResult$x),' runs done.'),
                      body    = paste0('Experiment ',substr(experiment$fileName,start = 5,stop = 7), ' up to ',nrow(spotResult$x),' runs done.'))
            gc()
          }
        }
      }
    }
    else{
      experiment = nextExperiment
      experiment$fileName = fileName
      upperBound = experiment$bounds %>% filter(bound == 'upper')
      lowerBound = experiment$bounds %>% filter(bound == 'lower')
      
      experimentTime = system.time({
        
        spotResult = spot(fun            = spotTest,
                          trainningData  = trainningData,
                          testingData    = testingData,
                          cacheFile      = cacheFile,
                          cacheDir       = cacheDir,
                          clusterMethod  = as.character(experiment$clusterMethod),
                          tagsMethod     = as.character(experiment$tagsMethod),
                          articlesMethod = as.character(experiment$articlesMethod),
                          lower          = as.matrix(lowerBound[1,2:7]),
                          upper          = as.matrix(upperBound[1,2:7]),
                          control        = list(funEvals = experiment$funEvals)
  
  
        )
      })
      experiment$currentResult = spotResult
      experiment$experimentTime = experiment$experimentTime + experimentTime
      
      save(list = c('experiment'),file = paste0(cacheDir,'//',experiment$fileName))
    }
  }
}, error = function(e) {
  sendEmail(subject = 'There was a problem (error) with the experiment.',
            body    = error)
}, finally = {
  sendEmail(subject = 'All done, need more work!',
            body    = 'All done, need more work!')
})
clearConnections()

