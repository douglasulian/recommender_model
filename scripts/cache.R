#### Libraries ####
list.of.packages = c('dplyr')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(dplyr)

#### Sources ####
debugSource(fileName = 'scripts/util.R')

#### Code ####
saveDistanceToCache = function(distancesCache, cacheDir ,distances, alphaIndex, betaIndex, lambdaIndex){
  
  fileName = getDistancesCacheNewFileName(distancesCache = distancesCache, alphaIndex = alphaIndex, betaIndex = betaIndex, lambdaIndex = lambdaIndex)
  
  if (enoughSpace()) {
    save(list = 'distances',compress = FALSE,                       file = paste0(cacheDir,'//',fileName))
  }
  else{
    save(list = 'distances',compress = TRUE , compression_level = 1,file = paste0(cacheDir,'//',fileName))
  }
  
  distancesCache = add_row(.data = distancesCache,
                           alphaIndex = alphaIndex, betaIndex = betaIndex, lambdaIndex = lambdaIndex, fileName = fileName)
  
  return(distancesCache)
}

saveClusterToCache = function(clustersCache, cacheDir, cluster, alphaIndex, betaIndex, lambdaIndex, clusterMethod, noClustersK){
  
  fileName = getClustersCacheNewFileName(clustersCache = clustersCache, alphaIndex = alphaIndex, betaIndex = betaIndex, lambdaIndex = lambdaIndex, clusterMethod = clusterMethod, noClustersK = noClustersK)
  
  if (enoughSpace()) {
    save(list = 'cluster', compress = FALSE,                        file = paste0(cacheDir,'//',fileName))  
  }
  else {
    save(list = 'cluster', compress = TRUE , compression_level = 1, file = paste0(cacheDir,'//',fileName))
  }
  
  
  
  clustersCache = add_row(.data = clustersCache,
                          alphaIndex = alphaIndex, 
                          betaIndex = betaIndex, 
                          lambdaIndex = lambdaIndex, 
                          clusterMethod = clusterMethod,
                          noClustersK = noClustersK,
                          fileName = fileName)
  return(clustersCache)  
}

getDistanceFromCache = function(distancesCache, cacheDir, curAlphaIndex, curBetaIndex, curLambdaIndex){
  distanceFound = distancesCache %>% filter(alphaIndex == curAlphaIndex & betaIndex == curBetaIndex & lambdaIndex == curLambdaIndex)
  if (nrow(distanceFound) > 0) {
    load(file = paste0(cacheDir,'//',distanceFound$fileName[1]))
    return(list(distances = distances,
                distancesCache = distancesCache))
  }
  else{
    distanceFound = distancesCache %>% filter(alphaIndex == curAlphaIndex & lambdaIndex == curLambdaIndex)
    if (nrow(distanceFound) > 0) {
      load(file = paste0(cacheDir,'//',distanceFound$fileName[1]))
      
      distances = setMixedDistanceIndex(distances = distances, newBetaIndex = curBetaIndex)
      distancesCache = saveDistanceToCache(distancesCache = distancesCache, cacheDir, distances = distances,alphaIndex = curAlphaIndex, betaIndex = curBetaIndex, lambdaIndex = curLambdaIndex)
      
      return(list(distances = distances,
                  distancesCache = distancesCache))
    }
    else{
      return(list(distances = NULL,distancesCache = distancesCache))
    }
  } 
}

getClusterFromCache = function(clustersCache, cacheDir,curAlphaIndex, curBetaIndex, curLambdaIndex, curClusterMethod, curNoClustersK){
  clustersFound = clustersCache %>% filter(alphaIndex == curAlphaIndex & 
                                             betaIndex == curBetaIndex & 
                                             lambdaIndex == curLambdaIndex &
                                             clusterMethod == curClusterMethod &
                                             noClustersK == curNoClustersK)
  if (nrow(clustersFound) > 0) {
    load(file = paste0(cacheDir,'//',clustersFound$fileName[1]))
    return(cluster)
  }
  else{
    return(NULL)
  } 
}

removeClusterFromCache = function(cacheFile, cacheDir, fileNameToDelete){
  load(file = paste0(cacheDir,'//',cacheFile))
  clustersFound = clustersCache %>% filter(fileName == fileNameToDelete)
  
  if (nrow(clustersFound) > 0) {
    for (i in 1:nrow(clustersFound)) {
      file.remove(paste0(cacheDir,'//',clustersFound$fileName[i]))
    }
    clustersCache = clustersCache %>% filter(fileName != fileNameToDelete)
  }
  save(list = c('distancesCache','clustersCache'), compress = TRUE, file = paste0(cacheDir,'//',cacheFile))
}

removeDistanceFromCache = function(cacheFile, cacheDir, fileNameToDelete){
  load(file = paste0(cacheDir,'//',cacheFile))
  distancesFound = distancesCache %>% filter(fileName == fileNameToDelete)
  
  if (nrow(distancesFound) > 0) {
    for (i in 1:nrow(distancesFound)) {
      file.remove(paste0(cacheDir,'//',distancesFound$fileName[i]))
    }
    distancesCache = distancesCache %>% filter(fileName != fileNameToDelete)
  }
  save(list = c('distancesCache','clustersCache'), compress = TRUE, file = paste0(cacheDir,'//',cacheFile))
}


getDistancesCacheNewFileName = function(distancesCache,alphaIndex, betaIndex, lambdaIndex){
  if (length(distancesCache$fileName) == 0) {
    nextIndex = 1
  }
  else{
    nextIndex      = max(vapply(X = distancesCache$fileName,FUN = function(fileName) return(as.numeric(substr(x = fileName,start = 11,stop = 15))),FUN.VALUE = numeric(1))) + 1
  }
  strAlphaIndex  = substr(x = paste0(sprintf(alphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strBetaIndex   = substr(x = paste0(sprintf(betaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLambdaIndex = substr(x = paste0(sprintf(lambdaIndex,fmt = '%f'),'0000'),start = 1,stop = 4)
  strNextIndex   = paste0('0000',as.character(nextIndex))
  strNextIndex   = substr(x = strNextIndex,start = nchar(strNextIndex) - 4,stop = nchar(strNextIndex))
  newFileName    = paste0('distances-',strNextIndex,'-',strAlphaIndex,'-',strBetaIndex,'-',strLambdaIndex,'.RData')
  
  return(newFileName)
}

getClustersCacheNewFileName = function(clustersCache,alphaIndex, betaIndex, lambdaIndex, clusterMethod, noClustersK){
  if (length(clustersCache$fileName) == 0) {
    nextIndex = 1
  }
  else{
    nextIndex      = max(vapply(X = clustersCache$fileName,FUN = function(fileName) return(as.numeric(substr(x = fileName,start = 10,stop = 14))),FUN.VALUE = numeric(1))) + 1 
  }
  strAlphaIndex  = substr(x = paste0(sprintf(alphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strBetaIndex   = substr(x = paste0(sprintf(betaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLambdaIndex = substr(x = paste0(sprintf(lambdaIndex,fmt = '%f'),'0000'),start = 1,stop = 4)
  strNoClustersK = paste0('00000',sprintf(noClustersK,fmt = '%i'))
  strNoClustersK = substr(x = strNoClustersK,start = nchar(strNoClustersK) - 4,stop = nchar(strNoClustersK))
  strNextIndex   = paste0('0000',as.character(nextIndex))
  strNextIndex   = substr(x = strNextIndex,start = nchar(strNextIndex) - 4,stop = nchar(strNextIndex))
  newFileName    = paste0('clusters-',strNextIndex,'-',strAlphaIndex,'-',strBetaIndex,'-',strLambdaIndex,'-',clusterMethod,'-',strNoClustersK,'.RData')
  return(newFileName)
}

saveSolutionToCache = function(solutionsCache,
                               cacheDir      ,
                               solution      , 
                               clusterMethod ,
                               tagsMethod    ,
                               articlesMethod,
                               alphaIndex    ,
                               betaIndex     ,
                               lambdaIndex   ,
                               gamaIndex     ,
                               zetaIndex     ,
                               noClustersK   ){
  
  fileName = getSolutionsCacheNewFileName(solutionsCache = solutionsCache, 
                                          clusterMethod  = clusterMethod ,
                                          tagsMethod     = tagsMethod    ,
                                          articlesMethod = articlesMethod,
                                          alphaIndex     = alphaIndex    ,
                                          betaIndex      = betaIndex     ,
                                          lambdaIndex    = lambdaIndex   ,
                                          gamaIndex      = gamaIndex     ,
                                          zetaIndex      = zetaIndex     ,
                                          noClustersK    = noClustersK   )
  
  save(list = 'solution', compress = TRUE ,compression_level = 1, file = paste0(cacheDir,'//',fileName))
  # save(list = 'solution', compress = FALSE, file = paste0(cacheDir,'//',fileName))
  
  solutionsCache = add_row(.data = solutionsCache, 
                           clusterMethod  = clusterMethod ,
                           tagsMethod     = tagsMethod    ,
                           articlesMethod = articlesMethod,
                           alphaIndex     = alphaIndex    ,
                           betaIndex      = betaIndex     ,
                           lambdaIndex    = lambdaIndex   ,
                           gamaIndex      = gamaIndex     ,
                           zetaIndex      = zetaIndex     ,
                           noClustersK    = noClustersK   ,
                           fileName       = fileName)
  return(solutionsCache)  
}

getSolutionFromCache = function(solutionsCache   , 
                                cacheDir         ,
                                curClusterMethod ,
                                curTagsMethod    ,
                                curArticlesMethod,
                                curAlphaIndex    ,
                                curBetaIndex     ,
                                curLambdaIndex   ,
                                curGamaIndex     ,
                                curZetaIndex     ,
                                curNoClustersK   ){
  solutionsFound = solutionsCache %>% filter(clusterMethod  == curClusterMethod  &
                                               tagsMethod     == curTagsMethod     &
                                               articlesMethod == curArticlesMethod &
                                               alphaIndex     == curAlphaIndex     &
                                               betaIndex      == curBetaIndex      &
                                               lambdaIndex    == curLambdaIndex    &
                                               gamaIndex      == curGamaIndex      &
                                               zetaIndex      == curZetaIndex      &
                                               noClustersK    == curNoClustersK)
  if (nrow(solutionsFound) > 0) {
    load(file = paste0(cacheDir,'//',solutionsFound$fileName[1]))
    return(solution)
  }
  else{
    return(NULL)
  } 
}

removeSolutionFromCache = function(cacheFile, cacheDir, fileNameToDelete){
  load(file = paste0(cacheDir,'//',cacheFile))
  solutionsFound = solutionsCache %>% filter(fileName == fileNameToDelete)
  
  if (nrow(solutionsFound) > 0) {
    for (i in 1:nrow(solutionsFound)) {
      file.remove(paste0(cacheDir,'//',solutionsFound$fileName[i]))
    }
    solutionsCache = solutionsCache %>% filter(fileName != fileNameToDelete)
  }
  save(list = c('distancesCache','clustersCache','solutionsCache'), compress = TRUE, file = paste0(cacheDir,'//',cacheFile))
}

getSolutionsCacheNewFileName = function(solutionsCache, 
                                        clusterMethod ,
                                        tagsMethod    ,
                                        articlesMethod,
                                        alphaIndex    ,
                                        betaIndex     ,
                                        lambdaIndex   ,
                                        gamaIndex     ,
                                        zetaIndex     ,
                                        noClustersK   ){
  if (length(solutionsCache$fileName) == 0) {
    nextIndex = 1
  }
  else{
    nextIndex = max(vapply(X = solutionsCache$fileName,FUN = function(fileName) return(as.numeric(substr(x = fileName,start = 11, stop = 15))),FUN.VALUE = numeric(1))) + 1 
  }
  
  strClusterMethod  = substr(x = clusterMethod,start = 1,stop = 1)
  strTagsMethod     = substr(x = tagsMethod,start = 1,stop = 1)
  strArticlesMethod = substr(x = articlesMethod,start = 1,stop = 1)
  strAlphaIndex     = substr(x = paste0(sprintf(alphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strBetaIndex      = substr(x = paste0(sprintf(betaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLambdaIndex    = substr(x = paste0(sprintf(lambdaIndex,fmt = '%f'),'0000'),start = 1,stop = 4)
  strGamaIndex      = substr(x = paste0(sprintf(gamaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strZetaIndex      = substr(x = paste0(sprintf(zetaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strNoClustersK    = paste0('00000',sprintf(noClustersK,fmt = '%i'))
  strNoClustersK    = substr(x = strNoClustersK,start = nchar(strNoClustersK) - 4,stop = nchar(strNoClustersK))
  
  strNextIndex      = paste0('0000',as.character(nextIndex))
  strNextIndex      = substr(x = strNextIndex,start = nchar(strNextIndex) - 4,stop = nchar(strNextIndex))
  
  newFileName       = paste0('solutions-',strNextIndex     ,'-'
                             ,strClusterMethod ,'-'
                             ,strTagsMethod    ,'-'
                             ,strArticlesMethod,'-'
                             ,strAlphaIndex    ,'-'
                             ,strBetaIndex     ,'-'
                             ,strLambdaIndex   ,'-'
                             ,strGamaIndex     ,'-'
                             ,strZetaIndex     ,'-'
                             ,strNoClustersK             
                             ,'.RData')
  
  # 'cache//solutions-00001-k-t-i-0.01-0.02-0.03-0.04-0.05-0001.RData')
  return(newFileName)
}

saveCache = function(cacheDir, cacheFile, cache){
  # save(list = c('distanas.tricesCache','clustersCache','solutionsCache'), compress = TRUE,compression_level = 1, file = paste0(cacheDir,'//',cacheFile))
  distancesCache = cache$distancesCache
  clustersCache  = cache$clustersCache
  solutionsCache = cache$solutionsCache
  save(list = c('distancesCache','clustersCache','solutionsCache'), compress = TRUE,compression_level = 1, file = paste0(cacheDir,'//',cacheFile))
}

loadCache = function(cacheDir,cacheFile){
  if (!file.exists(paste0(cacheDir,'//',cacheFile))) {
    distancesCache = tribble(~fileName, ~alphaIndex, ~betaIndex, ~lambdaIndex                                                                                  )
    clustersCache  = tribble(~fileName, ~alphaIndex, ~betaIndex, ~lambdaIndex, ~clusterMethod,~noClustersK                                                     )
    solutionsCache = tribble(~fileName, ~alphaIndex, ~betaIndex, ~lambdaIndex, ~clusterMethod,~noClustersK,~tagsMethod, ~articlesMethod, ~gamaIndex, ~zetaIndex)
  } 
  else{
    load(file = paste0(cacheDir,'//',cacheFile))
  }
  return(list(distancesCache = distancesCache, clustersCache = clustersCache, solutionsCache = solutionsCache))
}

getExperimentFileName = function(exp){
  strClusterMethod  = substr(x = exp$clusterMethod,start = 1,stop = 1)
  strTagsMethod     = substr(x = exp$tagsMethod,start = 1,stop = 1)
  strArticlesMethod = substr(x = exp$articlesMethod,start = 1,stop = 1)
  bounds = exp$bounds
  upperBound = bounds %>% filter(bound == 'upper')
  lowerBound = bounds %>% filter(bound == 'lower')
  strUpperAlphaIndex  = substr(x = paste0(sprintf(upperBound$usersTimeDiffAlphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLowerAlphaIndex  = substr(x = paste0(sprintf(lowerBound$usersTimeDiffAlphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  
  strUpperBetaIndex   = substr(x = paste0(sprintf(upperBound$mixedDistanceBetaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLowerBetaIndex   = substr(x = paste0(sprintf(lowerBound$mixedDistanceBetaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  
  strUpperLambdaIndex = substr(x = paste0(sprintf(upperBound$forgCurveLambdaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLowerLambdaIndex = substr(x = paste0(sprintf(lowerBound$forgCurveLambdaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  
  strUpperGamaIndex   = substr(x = paste0(sprintf(upperBound$tagsCutGamaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLowerGamaIndex   = substr(x = paste0(sprintf(lowerBound$tagsCutGamaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  
  strUpperZetaIndex   = substr(x = paste0(sprintf(upperBound$articlesCutZetaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLowerZetaIndex   = substr(x = paste0(sprintf(lowerBound$articlesCutZetaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  
  strUpperNoClustersK = paste0('000',sprintf(upperBound$noClustersK,fmt = '%i'))
  strUpperNoClustersK = substr(x = strUpperNoClustersK,start = nchar(strUpperNoClustersK) - 2,stop = nchar(strUpperNoClustersK))
  
  strLowerNoClustersK = paste0('000',sprintf(lowerBound$noClustersK,fmt = '%i'))
  strLowerNoClustersK = substr(x = strLowerNoClustersK,start = nchar(strLowerNoClustersK) - 2,stop = nchar(strLowerNoClustersK))
  
  newFileName       = paste0('exp-'
                             ,strClusterMethod   
                             ,strTagsMethod      
                             ,strArticlesMethod  ,'-','a','-'
                             ,strLowerAlphaIndex ,'-'
                             ,strUpperAlphaIndex ,'-','b','-'
                             ,strLowerBetaIndex  ,'-'
                             ,strUpperBetaIndex  ,'-','l','-'
                             ,strLowerLambdaIndex,'-'
                             ,strUpperLambdaIndex,'-','k','-'
                             ,strLowerNoClustersK,'-'     
                             ,strUpperNoClustersK,'-','g','-'
                             ,strLowerGamaIndex  ,'-'
                             ,strUpperGamaIndex  ,'-','z','-'
                             ,strLowerZetaIndex  ,'-'
                             ,strUpperZetaIndex            
                             ,'.RData')
  return(newFileName)
  
  
}