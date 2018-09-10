# debugSource('scripts//cache.R')
# cacheDir = 'cache//6000_all_clusters'
# cacheFile = 'cache.RData'
# newCacheFile = 'cacheNew.RData'
# oldCacheFile = 'cacheOld.RData'
# load(file = paste0(cacheDir,'//',cacheFile))

# clustersCache$fileName[2] = 'clusters-00002-0.74-0.15-0.65-kmedoid-00062.RData'
# solutionsCache$fileName[2] = 'solutions-00002-k-t-t-0.74-0.15-0.65-4714-6306-00062.RData'
# distancesCache$fileName[2] = 'distances-00002-0.74-0.15-0.65.RData'
# saveCache(cacheDir = cacheDir, cacheFile = cacheFile, distancesCache = distancesCache,solutionsCache = solutionsCache,clustersCache = clustersCache)
# file.rename(from = paste0(cacheDir,'//',cacheFile),to = paste0(cacheDir,'//',oldCacheFile))
# file.rename(from = paste0(cacheDir,'//',newCacheFile),to = paste0(cacheDir,'//',cacheFile))