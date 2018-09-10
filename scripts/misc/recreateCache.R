library(foreach)
library(doParallel)
library(parallel)
library(tibble)

cacheDir = 'cache//6000_all_clusters'
cacheFile = 'cache.RData'
noCores = 15

distancesCache = tribble(~fileName, ~alphaIndex, ~betaIndex, ~lambdaIndex                                                                                  )
clustersCache  = tribble(~fileName, ~alphaIndex, ~betaIndex, ~lambdaIndex, ~clusterMethod,~noClustersK                                                     )
solutionsCache = tribble(~fileName, ~alphaIndex, ~betaIndex, ~lambdaIndex, ~clusterMethod,~noClustersK,~tagsMethod, ~articlesMethod, ~gamaIndex, ~zetaIndex)

load( file = paste0(cacheDir,'//',cacheFile))

cacheFiles = distancesCache$fileName
existingFiles = list.files(path = cacheDir, pattern = "distances.*RData",all.files = TRUE,recursive = FALSE,ignore.case = FALSE)

newCacheFiles = existingFiles[which(!cacheFiles %in% existingFiles)]


tempo = system.time({
doParallel::registerDoParallel(cores = noCores)
parallelResults = list()
parallelResults = foreach(newCacheFile = newCacheFiles) %dopar% {
  
  loadResult = load(file = paste0(cacheDir,'//',newCacheFiles[1]))

  list(alphaIndex     = distances$parameters$alphaIndex    ,
       betaIndex      = distances$parameters$betaIndex     ,
       lambdaIndex    = distances$parameters$lambdaIndex   ,
       fileName       = newCacheFile)
}
doParallel::stopImplicitCluster()

})

for (i in 1:length(parallelResults)) {
  x = parallelResults[[i]]
  distancesCache = add_row(.data       = distancesCache,
                           alphaIndex  = x$alphaIndex, 
                           betaIndex   = x$betaIndex, 
                           lambdaIndex = x$lambdaIndex, 
                           fileName    = x$fileName)
}

##############################
##############################
##############################

cacheFiles = solutionsCache$fileName
existingFiles = list.files(path = cacheDir, pattern = "solutions.*RData",all.files = TRUE,recursive = FALSE,ignore.case = FALSE)
newCacheFiles = existingFiles[which(!existingFiles %in% cacheFiles)]

tempo = system.time({
  doParallel::registerDoParallel(cores = noCores)
  parallelResults = list()
  parallelResults = foreach(newCacheFile = newCacheFiles) %dopar% {
    
    loadResult = load(file = paste0(cacheDir,'//',newCacheFile))
    
    list(clusterMethod  = solution$parameters$clusterMethod ,
         tagsMethod     = solution$parameters$tagsMethod    ,
         articlesMethod = solution$parameters$articlesMethod,
         alphaIndex     = solution$parameters$alphaIndex    ,
         betaIndex      = solution$parameters$betaIndex     ,
         lambdaIndex    = solution$parameters$lambdaIndex   ,
         gamaIndex      = solution$parameters$gamaIndex     ,
         zetaIndex      = solution$parameters$zetaIndex     ,
         noClustersK    = solution$parameters$k   ,
         fileName       = newCacheFile)
  }
  doParallel::stopImplicitCluster()
  
})

for (i in 1:length(parallelResults)) {
  x = parallelResults[[i]]
  solutionsCache = add_row(.data = solutionsCache,
                           clusterMethod  = x$clusterMethod ,
                           tagsMethod     = x$tagsMethod    ,
                           articlesMethod = x$articlesMethod,
                           alphaIndex     = x$alphaIndex    ,
                           betaIndex      = x$betaIndex     ,
                           lambdaIndex    = x$lambdaIndex   ,
                           gamaIndex      = x$gamaIndex     ,
                           zetaIndex      = x$zetaIndex     ,
                           noClustersK    = x$noClustersK   ,
                           fileName       = x$fileName)
}


##############################
##############################
##############################

cacheFiles = clustersCache$fileName
existingFiles = list.files(path = cacheDir, pattern = "clusters.*RData",all.files = TRUE,recursive = FALSE,ignore.case = FALSE)
newCacheFiles = existingFiles[which(!existingFiles %in% cacheFiles)]

clustDist = tribble(~cluster,~distance
  ,'clusters-00100-0.23-0.77-0.28-kmedoid-00052.RData','distances-00007-0.23-0.77-0.28.RData'
  ,'clusters-00101-0.40-0.61-0.25-kmedoid-00002.RData','distances-00101-0.40-0.61-0.25.RData'
  ,'clusters-00102-0.34-0.49-0.35-kmedoid-00047.RData','distances-00102-0.34-0.49-0.35.RData'
  ,'clusters-00103-0.91-0.08-0.05-kmedoid-00004.RData','distances-00103-0.91-0.08-0.05.RData'
  ,'clusters-00104-0.35-0.77-0.61-kmedoid-00009.RData','distances-00104-0.35-0.77-0.61.RData'
  ,'clusters-00105-0.23-0.00-0.10-kmedoid-00049.RData','distances-00105-0.23-0.00-0.10.RData'
  ,'clusters-00106-0.63-0.91-0.40-kmedoid-00031.RData','distances-00106-0.63-0.91-0.40.RData'
  ,'clusters-00107-0.89-0.20-0.60-kmedoid-00066.RData','distances-00107-0.89-0.20-0.60.RData'
  ,'clusters-00108-0.65-0.77-0.27-kmedoid-00050.RData','distances-00108-0.65-0.77-0.27.RData'
  ,'clusters-00109-0.72-0.98-0.08-kmedoid-00054.RData','distances-00109-0.72-0.98-0.08.RData'
  ,'clusters-00110-0.69-0.83-0.90-kmedoid-00052.RData','distances-00110-0.69-0.83-0.90.RData'
  ,'clusters-00111-0.58-0.93-0.11-kmedoid-00060.RData','distances-00111-0.58-0.93-0.11.RData'
  ,'clusters-00112-0.48-0.69-0.85-kmedoid-00052.RData','distances-00112-0.48-0.69-0.85.RData'
  ,'clusters-00113-0.54-0.61-0.79-kmedoid-00065.RData','distances-00113-0.54-0.61-0.79.RData'
  ,'clusters-00114-0.12-0.99-0.55-kmedoid-00032.RData','distances-00114-0.12-0.99-0.55.RData'
  ,'clusters-00115-0.05-0.87-0.12-kmedoid-00045.RData','distances-00115-0.05-0.87-0.12.RData'
  ,'clusters-00116-0.27-0.54-0.82-kmedoid-00068.RData','distances-00116-0.27-0.54-0.82.RData'
  ,'clusters-00117-0.95-0.63-0.83-kmedoid-00055.RData','distances-00117-0.95-0.63-0.83.RData'
  ,'clusters-00118-0.24-0.82-0.06-kmedoid-00073.RData','distances-00118-0.24-0.82-0.06.RData'
  ,'clusters-00119-0.00-0.51-0.22-kmedoid-00060.RData','distances-00119-0.00-0.51-0.22.RData'
  ,'clusters-00120-0.53-0.40-0.22-kmedoid-00053.RData','distances-00120-0.53-0.40-0.22.RData'
  ,'clusters-00121-0.84-0.38-0.99-kmedoid-00057.RData','distances-00121-0.84-0.38-0.99.RData'
  ,'clusters-00122-0.13-0.94-0.03-kmedoid-00062.RData','distances-00122-0.13-0.94-0.03.RData'
  ,'clusters-00123-0.76-0.17-0.54-kmedoid-00064.RData','distances-00123-0.76-0.17-0.54.RData'
  ,'clusters-00124-0.34-0.13-0.30-kmedoid-00045.RData','distances-00124-0.34-0.13-0.30.RData'
  ,'clusters-00125-0.34-0.64-0.21-kmedoid-00060.RData','distances-00125-0.34-0.64-0.21.RData'
  ,'clusters-00126-0.31-0.92-0.91-kmedoid-00050.RData','distances-00126-0.31-0.92-0.91.RData'
  ,'clusters-00127-0.16-0.66-0.27-kmedoid-00054.RData','distances-00127-0.16-0.66-0.27.RData'
  ,'clusters-00128-0.79-0.55-0.50-kmedoid-00064.RData','distances-00128-0.79-0.55-0.50.RData'
  ,'clusters-00129-0.38-0.14-0.58-kmedoid-00045.RData','distances-00129-0.38-0.14-0.58.RData'
  ,'clusters-00130-0.80-0.83-0.41-kmedoid-00049.RData','distances-00130-0.80-0.83-0.41.RData'
  ,'clusters-00131-0.05-0.20-0.59-ward.D-00011.RData','distances-00001-0.05-0.20-0.59.RData'
  ,'clusters-00132-0.74-0.15-0.65-ward.D-00062.RData','distances-00002-0.74-0.15-0.65.RData'
  ,'clusters-00133-0.13-0.44-0.30-ward.D-00059.RData','distances-00003-0.13-0.44-0.30.RData'
  ,'clusters-00134-0.59-0.39-0.89-ward.D-00073.RData','distances-00004-0.59-0.39-0.89.RData'
  ,'clusters-00135-0.86-0.88-0.04-ward.D-00031.RData','distances-00005-0.86-0.88-0.04.RData'
  ,'clusters-00136-0.31-0.91-0.98-ward.D-00036.RData','distances-00006-0.31-0.91-0.98.RData'
  ,'clusters-00137-0.23-0.77-0.28-ward.D-00052.RData','distances-00007-0.23-0.77-0.28.RData'
  ,'clusters-00138-0.95-0.04-0.12-ward.D-00002.RData','distances-00008-0.95-0.04-0.12.RData'
  ,'clusters-00139-0.48-0.65-0.71-ward.D-00042.RData','distances-00009-0.48-0.65-0.71.RData'
  ,'clusters-00140-0.61-0.50-0.42-ward.D-00020.RData','distances-00010-0.61-0.50-0.42.RData'
  ,'clusters-00141-0.89-0.84-0.83-ward.D-00065.RData','distances-00131-0.89-0.84-0.83.RData'
  ,'clusters-00142-0.57-0.66-0.36-ward.D-00045.RData','distances-00132-0.57-0.66-0.36.RData'
  ,'clusters-00143-0.27-0.60-0.27-ward.D-00046.RData','distances-00133-0.27-0.60-0.27.RData'
  ,'clusters-00144-0.79-0.83-0.15-ward.D-00021.RData','distances-00134-0.79-0.83-0.15.RData'
  ,'clusters-00145-0.30-0.45-0.08-ward.D-00022.RData','distances-00135-0.30-0.45-0.08.RData'
  ,'clusters-00146-0.83-0.56-0.42-ward.D-00076.RData','distances-00136-0.83-0.56-0.42.RData'
  ,'clusters-00147-0.10-0.91-0.83-ward.D-00050.RData','distances-00137-0.10-0.91-0.83.RData'
  ,'clusters-00148-0.48-0.65-0.71-kmedoid-00042.RData','distances-00138-0.10-0.27-0.89.RData'
  
)

tempo = system.time({
  doParallel::registerDoParallel(cores = noCores)
  parallelResults = list()
  parallelResults = foreach(newCacheFile = newCacheFiles) %dopar% {
    
    loadResult = load(file = paste0(cacheDir,'//',newCacheFile))
    
    distancefileName = (clustDist %>% filter(cluster == newCacheFile))$distance
    distance = (distancesCache %>% filter(fileName == distancefileName))
    
    list(alphaIndex    = distance$alphaIndex, 
         betaIndex     = distance$betaIndex, 
         lambdaIndex   = distance$lambdaIndex, 
         clusterMethod = cluster$parameters$method,
         noClustersK   = cluster$parameters$k,
         fileName      = newCacheFile)
  }
  doParallel::stopImplicitCluster()
  
})

for (i in 1:length(parallelResults)) {
  x = parallelResults[[i]]
  clustersCache = add_row(.data         = clustersCache,
                          alphaIndex    = x$alphaIndex, 
                          betaIndex     = x$betaIndex, 
                          lambdaIndex   = x$lambdaIndex, 
                          clusterMethod = x$clusterMethod,
                          noClustersK   = x$noClustersK,
                          fileName      = x$fileName)
}

# View(distancesCache)
# View(clustersCache)
# 
cache = list(
  distancesCache = distancesCache,
  clustersCache = clustersCache,
  solutionsCache = solutionsCache
)

saveCache(cacheDir = cacheDir,cacheFile = cacheFile,cache = cache)