#### Libraries ####
list.of.packages = c('lsa','fastcluster','fpc','cluster','dbscan')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

# Loads clustering libraries
library(fastcluster) # Hierarquichal clustering
library(cluster) # Partitioning clustering
library(lsa) # Loads LSA library that contains cosine distance calculation for matrices.
library(fpc)
library(dbscan)

#### Code ####
cluster = function(distance, method, k){
  k = round(k)
  if (method == 'ward.D') {
    writeLog("if (method == 'ward.D') {")
    cluster = getHierarchicalCluster(distance = distance, k = k, method = 'ward.D')
  }
  else if (method == 'kmedoid') {
    writeLog('cluster = getKMedoidCluster(distance = distance, k = k)')
    cluster = getKMedoidCluster(distance = distance, k = k)
  }
  writeLog('parameters = list(method = method,')
  # clusterTab = getClustersTab(clusters = list(cluster = cluster), contentData = usersTagsMatrix)
  parameters = list(method = method,
                    k = k)
  return(list(cluster = cluster,
              parameters = parameters))
}

getHierarchicalCluster = function(distance, method ,k){
  
  if (class(distance) == 'matrix') {
    distance = as.dist(distance)  
  }
  
  clusterTime = system.time({
    
    hierarcCluster = hclust(d = distance,method = method)
  })

  solution = cutree(hierarcCluster,k = k) 
  
  cluster = getClusterSolution(users = labels(solution),
                               usersClusters = as.vector(solution),
                               time = clusterTime,
                               k = k,
                               method = method,
                               diss = distance,
                               clusterObject = hierarcCluster
                               )
  rm(solution)
  rm(hierarcCluster)
  rm(distance)
  return(cluster)
}

getKMedoidCluster = function(distance, k){
  writeLog('if (class(distance) == "matrix") { ')
  if (class(distance) == 'matrix') {
    distance = as.dist(distance)  
  }
  writeLog('kmedCluster = pam(x = distance, diss = TRUE,k = k)')
  clusterTime = system.time({
    kmedCluster = pam(x = distance, diss = TRUE, k = k, cluster.only = TRUE, do.swap = TRUE, pamonce = 2)
  })
  writeLog('cluster = getClusterSolution(users = labels(kmedCluster$clustering),')
  cluster = getClusterSolution(users = labels(kmedCluster),
                               usersClusters = as.vector(kmedCluster),
                               time = clusterTime,
                               k = k,
                               method = 'PAM',
                               diss = distance,
                               clusterObject = kmedCluster)
  writeLog('return(cluster)')
  return(cluster)
}

getKMeansCluster = function(distance, data, k){
  if (class(distance) == 'matrix') {
    distance = as.dist(distance)  
  }
  clusterTime = system.time({
    kmeansCluster = (kmeans(data, k))
  })
  
  cluster = getClusterSolution(users = labels(kmeansCluster$cluster),
                               usersClusters = as.vector(kmeansCluster$cluster),
                               time = clusterTime,
                               k = k,
                               method = 'kmeans',
                               diss = distance,
                               clusterObject = kmeansCluster)
  
  return(cluster)
}

getDBSCANCluster = function(distance, eps){
  if (class(distance) == 'matrix') {
    distance = as.dist(distance)  
  }
  
  clusterTime = system.time({
    dbscanCluster = dbscan(x = distance, eps = eps, minPts = minPts)
  })
  
  cluster = getClusterSolution(users = labels(dbscanCluster$cluster),
                               usersClusters = as.vector(dbscanCluster$cluster),
                               time = clusterTime,
                               k = k,
                               method = 'kmeans',
                               diss = distance,
                               clusterObject = dbscanCluster)
  
  return(cluster)
}

getClustersTab = function(clusters,contentData){
  emails = rownames(contentData)
  clustersTab = as.tibble(matrix(data = emails, ncol = 1, dimnames = list(rep(1:length(emails)),c('email'))))
  for (clusterIndex in 1:length(clusters)) {
    cluster = clusters[[clusterIndex]]
    columnName = paste0(cluster$dissType,' ',cluster$method,' ','k=',cluster$k,' ','(',round(x = cluster$time[3],digits = 2),')')
    solution = cluster$cluster
    clustersTab = add_column(.data = clustersTab, !!columnName := as.integer(solution[,2]),.after = clusterIndex)
  }
  
  tags = t(sapply(X = clustersTab[,1]$email, FUN = getMainTags,usersTagsMatrix = contentData,qtt = 5))
  
  # Selects the top n main tags for a user
  clustersTab = add_column(.data = as.tibble(clustersTab),mainTag1 = tags[,2],mainTag2 = tags[,3],mainTag3 = tags[,4],mainTag4 = tags[,5],mainTag5 = tags[,6],.after = ncol(clustersTab))
  return(clustersTab)
}

# Returns the top n tags for the user in the usersTagsMatrix
getMainTags = function(usersTagsMatrix,email,qtt,withValue = FALSE){
  tags = c(sort(usersTagsMatrix[email,usersTagsMatrix[email,] > 0],decreasing = TRUE)[1:qtt],rep(list("<NA>" = 0),qtt))[1:qtt]
  tagNames = names(tags)
  names(tagNames) = rep(1:qtt)
  names(tags) = rep(1:qtt)
  
  if (withValue)
    result = mapply(FUN = function(x,y) return(paste(x,' (',trimws(trunc(y*10)/10),')',sep = '')),tagNames,tags)
  else
    result = mapply(FUN = function(x,y) return(paste(x,sep = '')),tagNames,tags)
  return(c(email = email,result))
}


getClusterSolution = function(users,usersClusters,time,method, k, diss,clusterObject){
  cluster = list()
  cluster$method = method
  cluster$k = k
  cluster$diss = diss
  cluster$time = time
  cluster$cluster = matrix(data = c(users,usersClusters),ncol = 2,dimnames = list(seq(1:length(users)),c('user','cluster')))
  cluster$clusterObject = clusterObject
  return(cluster)
}

clusterTest = function(distances, k){
  auxDistances = list()
  length(auxDistances) = 3
  
  auxDistances[[1]]$distance = as.dist(distances$contentDistance)
  auxDistances[[1]]$type = 'content'
  auxDistances[[2]]$distance = as.dist(distances$behaviourDistance)
  auxDistances[[2]]$type = 'behaviour'
  auxDistances[[3]]$distance = as.dist(distances$mixedDistance)
  auxDistances[[3]]$type = 'mixed'
  contentData = distances$data$usersTagsMatrix
  
  # clusterMethods = c('complete', 'single','average', 'centroid','media','ward.D')
  clusterMethods = c('average','ward.D')
  clusters = list()
  length(clusters) = length(clusterMethods)*3 + 3 + 1 
  
  # emails = rownames(as.matrix(auxDistances[[1]]$distance))
  curTest = 0
  for (distanceIndex in 1:length(auxDistances)) {
    distanceType = auxDistances[[distanceIndex]]$type
    for (methodIndex in 1:length(clusterMethods)) {
      curTest = curTest + 1
      clusters[[curTest]] = getHierarchicalCluster(distance = distance, k = k, method = clusterMethods[methodIndex])
    }
    
    curTest = curTest + 1
    clusters[[curTest]] = getKMedoidCluster(distance = distance, k = k)
    
    if (distanceType == 'content') {
      curTest = curTest + 1
      clusters[[curTest]] = getKMeansCluster(distance = distance, k = k, data = contentData)
    }
    
  }
  
  return(clusters)
}
