library(foreach)
library(doParallel)
library(parallel)
library(tibble)

debugSource('scripts/evaluation.R')
debugSource('scripts/dataBase.R')
debugSource('scripts/distances.R')

cacheDir = 'cache//6000_all_clusters'
cacheFile = 'cache.RData'

load(file = paste0(cacheDir,'//',cacheFile))

clearConnections()

trainningConnection = getDataBaseConnection(schema = 'trainning6000', dbUser = "cl-us-gzh", dbHost = "10.238.4.109", dbName = "cl-us-gzh", dbPass = "cl-us-gzh")
testingConnection   = getDataBaseConnection(schema = 'testing6000'  , dbUser = "cl-us-gzh", dbHost = "10.238.4.109", dbName = "cl-us-gzh", dbPass = "cl-us-gzh")

testingData         = getTestingData(testingConnection)
trainningData       = getTrainningData(trainningConnection)
usersTagsMatrix = getUsersTagsMatrix(usersArticlesMatrix          = trainningData$usersArticlesMatrix,
                                     articlesTagsMatrix           = trainningData$articlesTagsMatrix,
                                     usersArticlesTimeDiffMatrix  = trainningData$usersArticlesTimeDiffMatrix,
                                     lambdaIndex                  = 0.0)
usersTagsNormMTR = getNormMatrix(usersTagsMatrix, byCol = FALSE)

cache = loadCache(cacheDir = cacheDir,cacheFile = cacheFile)
arquivos = clustersCache$fileName

noCores = 15

tempo = system.time({
  doParallel::registerDoParallel(cores = noCores)
  parallelResults = list()
  parallelResults = foreach(arquivo = arquivos) %dopar% {
    
    loadResult = load(file = paste0(cacheDir,'//',arquivos[1]))
    usersClustersMTR = getUsersClustersMatrix(cluster$cluster$cluster)
    tagsClustersMTR  = getTagsClustersMatrix(usersTagsNormMTR,usersClustersMTR)
    apply(X = tagsClustersMTR > 0.001, MARGIN = 2,FUN = sum)
    teste = getTagsClustersTopNMatrix(tagsClustersM = tagsClustersMTR, n = 4941)
    apply(X = teste != 0, MARGIN = 2,FUN = sum)
  }
  doParallel::stopImplicitCluster()
  
})
solutions = parallelResults

clearConnections()