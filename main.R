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

setwd("/home/douglas_ulian/recommender_model")

#### Import Functions ####
debugSource(fileName = 'scripts/util.R')
debugSource(fileName = 'scripts/distances.R')
debugSource(fileName = 'scripts/cluster.R')
debugSource(fileName = 'scripts/evaluation.R')
debugSource(fileName = 'scripts/dataBase.R')
debugSource(fileName = 'input.R')
#### Main ####
enableJIT(0)

clearConnections()

trainningConnection = getDataBaseConnection(schema = schema,
                                            dbUser = dbUser,
                                            dbHost = dbHost,
                                            dbName = dbName,
                                            dbPass = dbPass)

# number of clusters must be less then or equal to the number of users available for clustering.
# 2000 = 1021
# 200 = 104

# trainningData       = getTrainningData(trainningConnection)
# writeLog('')
# writeLog('')
# writeLog('')
# writeLog('DEEEEEEEEENSE')
# result = executeModel(trainningData           = trainningData,
#                       clusterMethod           = clusterMethod,
#                       noClustersK             = noClustersK,
#                       usersTimeDiffAlphaIndex = usersTimeDiffAlphaIndex,
#                       mixedDistanceBetaIndex  = mixedDistanceBetaIndex,
#                       forgCurveLambdaIndex    = forgCurveLambdaIndex,
#                       tagsCutGamaIndex        = tagsCutGamaIndex)

trainningSparseData = getTrainningSparseData(trainningConnection,alphaIndex = usersTimeDiffAlphaIndex)
#save(list = c('trainningSparseData'),file = 'trainningSparseData2000.RData')
#load("~/recommender_model/trainningSparseDataPrivate.RData")
writeLog('')
writeLog('')
writeLog('')
writeLog('SPARSEEEEEEEE') 
# resultSparse = executeModelSparse(trainningData           = trainningSparseData,
#                                   clusterMethod           = clusterMethod,
#                                   noClustersK             = noClustersK,
#                                   usersTimeDiffAlphaIndex = usersTimeDiffAlphaIndex,
#                                   mixedDistanceBetaIndex  = mixedDistanceBetaIndex,
#                                   forgCurveLambdaIndex    = forgCurveLambdaIndex,
#                                   tagsCutGamaIndex        = tagsCutGamaIndex)

printValue(parameter = 'clusterMethod'          , value = clusterMethod)
printValue(parameter = 'tagsMethod'             , value = tagsMethod)
printValue(parameter = 'tagsCutGamaIndex'       , value = tagsCutGamaIndex)
printValue(parameter = 'articlesMethod'         , value = articlesMethod)
printValue(parameter = 'articlesCutZetaIndex'   , value = articlesCutZetaIndex)
printValue(parameter = 'usersTimeDiffAlphaIndex', value = usersTimeDiffAlphaIndex)
printValue(parameter = 'forgCurveLambdaIndex'   , value = forgCurveLambdaIndex)
printValue(parameter = 'mixedDistanceBetaIndex' , value = mixedDistanceBetaIndex)

noClustersK = round(noClustersK)

if (tagsMethod == 'topn') {
  tagsCutGamaIndex = round(tagsCutGamaIndex)
}
if (articlesMethod == 'topn') {
  articlesCutZetaIndex = round(articlesCutZetaIndex)
}

lambdaIndex = forgCurveLambdaIndex
alphaIndex = usersTimeDiffAlphaIndex
betaIndex = mixedDistanceBetaIndex

parameters = list(alphaIndex  = alphaIndex,
                  betaIndex   = betaIndex,
                  lambdaIndex = lambdaIndex)

#aux = ncol(trainningData$usersArticlesMatrix)
#auxMatrix = matrix(data = rep(1,aux*aux),nrow = aux)
#auxMatrix[upper.tri(x = auxMatrix,diag = TRUE)] = 0
writeLog(         'behaviourDistanceTime                  = system.time({')

  
  # calculates behaviour distance
  #behaviourDistance = getJaccardDistanceSparse(usersArticlesMatrix           = trainningSparseData$usersArticlesMatrix,
  #                                             usersArticlesAttenCoeffMatrix = trainningSparseData$usersArticlesAttenCoeffMatrix,
  #                                             articlesPopularityIndexVector = trainningSparseData$articlesPopularityIndexVector,
  #                                             alphaIndex = alphaIndex)
  usersArticlesMatrix           = trainningSparseData$usersArticlesMatrix
  usersArticlesAttenCoeffMatrix = trainningSparseData$usersArticlesAttenCoeffMatrix
  articlesPopularityIndexVector = trainningSparseData$articlesPopularityIndexVector
  
  emails = colnames(usersArticlesMatrix)
  size   = ncol(usersArticlesMatrix)  
  
  # noCores = detectCores(all.tests = FALSE, logical = TRUE) - 2
  noCores = getNoCores()
  
  jaccard = sparseMatrix(x = 0L,i = size, j = size,dimnames = list(emails,emails))
  
  usersUsersIndexesMatrix = rcppEigenSparseProd(usersArticlesMatrix)
  usersUsersIndexesMatrix = tril(x = usersUsersIndexesMatrix, k = -1)
  usersUsersIndexesVector = as.data.frame(summary(usersUsersIndexesMatrix))
  usersUsersIndexesVector$row = usersUsersIndexesVector$i
  usersUsersIndexesVector$col = usersUsersIndexesVector$j
  usersUsersIndexesVector$x = NULL
  usersUsersIndexesVector$i = NULL
  usersUsersIndexesVector$j = NULL
  
  indexes = getIndexesFromVector(indexesVector = usersUsersIndexesVector,ncores = noCores)
  
  writeLog(paste0('sum(jaccard)                           = ',sum(jaccard)))
  writeLog(paste0('sum(usersArticlesMatrix)               = ',sum(usersArticlesMatrix)))
  writeLog(paste0('sum(usersArticlesAttenCoeffMatrix)     = ',sum(usersArticlesAttenCoeffMatrix)))
  writeLog(paste0('sum(articlesPopularityIndexVector[,3]) = ',sum(articlesPopularityIndexVector[,3])))
  writeLog(paste0('rownames(usersArticlesMatrix)[2]       = ',rownames(usersArticlesMatrix)[2]))
  writeLog(paste0('rownames(usersArticlesMatrix)[nrow-2]  = ',rownames(usersArticlesMatrix)[nrow(usersArticlesMatrix)-2]))
  writeLog(paste0('colnames(usersArticlesMatrix)[2]       = ',colnames(usersArticlesMatrix)[2]))
  writeLog(paste0('colnames(usersArticlesMatrix)[nrow-2]  = ',colnames(usersArticlesMatrix)[ncol(usersArticlesMatrix)-2]))
  
  gc()
  
  doParallel::registerDoParallel(cores = noCores)
  
  parallelResults = foreach(index = indexes,.init = jaccard, .combine = "+") %dopar% {
    
    getJaccardDistancesCSparse(rowIndexes = as.integer(index[,1]),
                               colIndexes = as.integer(index[,2]),
                               usersArticlesMatrix = usersArticlesMatrix,
                               usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                               articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
    
  }
  parallelResults[tril(parallelResults == 0,k = -1)] = 1
  parallelResults[parallelResults==2] = 0
  
  result = parallelResults
  
  writeLog(paste0('sum(result)                            = ',sum(result)))
  writeLog(paste0('ncol(result)                           = ',ncol(result)))
  writeLog(paste0('nrow(result)                           = ',nrow(result)))
  writeLog(paste0('result[1,2]                            = ',result[1,2] ))
  
  doParallel::stopImplicitCluster()
  rm(list = c('parallelResults','jaccard','usersArticlesAttenCoeffMatrix','usersUsersIndexesVector'))
  gc()
  behaviourDistance = result
  writeLog(paste0('behaviourDistance                      = ',as.character(sum(behaviourDistance))))
  #behaviourDistance = behaviourDistance * auxMatrix
  behaviourDistance = tril(x=behaviourDistance,k = -1)
  

writeLog(paste0(  'behaviourDistance                      = ',as.character(sum(behaviourDistance))))
writeLog(         'contentDistanceTime                    = system.time({')
gc()

contentDistanceTime = system.time({
  
  # Applies forgetting curve over content (tags)
  usersTagsMatrix = getUsersTagsMatrix(usersArticlesMatrix          = trainningSparseData$usersArticlesMatrix,
                                       articlesTagsMatrix           = trainningSparseData$articlesTagsMatrix,
                                       usersArticlesTimeDiffMatrix  = trainningSparseData$usersArticlesTimeDiffMatrix,
                                       lambdaIndex                  = lambdaIndex)
  writeLog(paste0('sum(usersTagsMatrix)                   = ',sum(usersTagsMatrix)))
  # Calculates content distance
  contentDistance = getCosineDistance(usersTagsMatrix = usersTagsMatrix)
})
writeLog(paste0(  'contentDistance                        = ',as.character(sum(contentDistance))))
writeLog(         'mixedDistanceTime                      = system.time({')
gc()

mixedDistanceTime = system.time({
  
  mixedDistance = betaIndex*behaviourDistance + contentDistance * (1 - betaIndex)
})
writeLog(         'return(list(contentDistance            = contentDistance')
gc()


#distances      = getDistancesSparse(trainningData = trainningSparseData, 
#                                    lambdaIndex = forgCurveLambdaIndex, 
#                                    alphaIndex = usersTimeDiffAlphaIndex, 
#                                    betaIndex = mixedDistanceBetaIndex)

printValue(parameter = 'distances'              , value = 'NEW'   )      

distance = mixedDistance

gc()

clusters       = getCluster(distance = distance, k = noClustersK, method = clusterMethod)

printValue(parameter = 'clusters'               , value = 'NEW'   )
printValue(parameter = 'noClustersK'            , value = noClustersK)  

clustersProfiles = getClustersProfiles(usersTagsM = usersTagsMatrix, 
                                       clusters = clusters$cluster$cluster,
                                       tagsCutGamaIndex = tagsCutGamaIndex,
                                       tagsMethod = tagsMethod)

sum(clustersProfiles)

#writeClustersToDB(result$clusters, trainningConnection)

#writeClustersProfilesToDB(result$clustersProfiles, trainningConnection)

#clearConnections()



