#### Libraries ####
list.of.packages = c('dplyr')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(dplyr)

#### Sources ####
#debugSource(fileName = 'scripts/util.R')
#debugSource(fileName = 'scripts/distances.R')
#debugSource(fileName = 'scripts/cluster.R')
#### Code ####


# Creates the users versus cluster matrix
getUsersClustersMatrix = function(cluster){
  usersClustersM = as.tibble(cluster)
  usersClustersM = add_column(usersClustersM,belongs = rep(1,nrow(usersClustersM)),.after = 'cluster')
  usersClustersM = spread(data = usersClustersM,key = cluster,value = belongs, fill = 0)
  usersClustersM = as.data.frame(usersClustersM)
  rownames(usersClustersM) = usersClustersM$user
  usersClustersM$user = NULL
  return(usersClustersM)
}

# Creates the tags versus clusters matrix, with tag weight per cluster
getTagsClustersMatrix = function(usersTagsM,usersClustersM){
  # Restricts the users to those present in both matrices and equalizes the user dimension on both
  usersIntersect    = getIntersect(rownames(usersTagsM),rownames(usersClustersM))
  usersTagsM        = equalize(m = usersTagsM    ,toEqualize = usersIntersect,byRow = TRUE)
  usersClustersM    = equalize(m = usersClustersM,toEqualize = usersIntersect,byRow = TRUE)

  # Counts
  countM = matrix(data = 1,nrow = nrow(usersTagsM),ncol = ncol(usersTagsM),dimnames = list(rownames(usersTagsM),colnames(usersTagsM)))
  countM[usersTagsM == 0] = 0
  countM = matrixMult(m1 = as.matrix(t(countM)),m2 = as.matrix(usersClustersM))
  tagsClustersM = matrixMult(m1 = as.matrix(t(usersTagsM)), m2 = as.matrix(usersClustersM))
  tagsClustersM = tagsClustersM / countM
  tagsClustersM[is.nan(tagsClustersM)] = 0
  return(tagsClustersM)
}
# 
# # Creates the articles suggested per cluster matrix based on cluster tags and test articles tags matrix
# getArticlesClustersSuggestionsMatrix = function(articlesTagsM,tagsClusterM){
#   #   Restricts tha articles versus tags to those that appear at the clusters 
#   #   Removes from the tags that appear at the test articles those that do not participate of the recommendation
#   tagsIntersect = getIntersect(colnames(articlesTagsM) ,rownames(tagsClusterM))
#   
#   articlesTagsM = equalize(m = articlesTagsM,toEqualize = tagsIntersect ,byRow = FALSE)
#   tagsClusterM  = equalize(m = tagsClusterM ,toEqualize = tagsIntersect ,byRow = TRUE)
#   
#   return(matrixMult(m1 = as.matrix(articlesTagsM), m2 = as.matrix(tagsClusterM)))
# }
# 
# # Creates the articles versus users suggestion matrix
# getArticlesUsersSuggMatrix = function(articlesClustersSuggM,usersClustersMTR){
# 
#   articlesUsersSuggMatrix = matrixMult(m1 = as.matrix(articlesClustersSuggM), m2 = t(as.matrix(usersClustersMTR)))
#   
#   articlesUsersSuggMatrix = articlesUsersSuggMatrix[apply(FUN = sum, MARGIN = 1,X = articlesUsersSuggMatrix) != 0,, drop = FALSE]
#   return(articlesUsersSuggMatrix)
# }
# 
# 
getTagsClustersTopNMatrix = function(tagsClustersM, n){
  return(getTopNMatrix(m = tagsClustersM,n = n))
}
# 
# getArticlesClustersTopNMatrix = function(articlesClusters,n){
#   return(getTopNMatrix(m = articlesClusters,n = n))
# }
# 
# # Creates a new matrix based on m, only keeping values that are above the limit
getCutMatrix = function(m, limit){
  cutM = matrix(data = limit,
                nrow = nrow(m),
                ncol = ncol(m),
                byrow = TRUE)
  mNorm = getNormMatrix(m,byCol = TRUE)

  maxMNorm = max(mNorm)

  if (limit > maxMNorm) {
    limit = maxMNorm
  }

  m[mNorm < cutM] = 0
  return(m)
}
# 
# Select tags per cluster that are within the limit index (after normalization per cluster)
getTagsClustersCutMatrix = function(tagsClustersM, limit){
  return(getCutMatrix(m = tagsClustersM, limit = limit))
}
# 
# # Select tags per cluster that are within the limit index (after normalization per cluster)
# getArticlesClustersCutMatrix = function(articlesClustersM, limit){
#   return(getCutMatrix(m = articlesClustersM, limit = limit))
# }

executeModel = function(trainningData,
                        clusterMethod,
                        noClustersK,
                        usersTimeDiffAlphaIndex,
                        mixedDistanceBetaIndex,
                        forgCurveLambdaIndex,
                        tagsCutGamaIndex
){
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
  
  distances      = getDistances(trainningData, lambdaIndex = forgCurveLambdaIndex, alphaIndex = usersTimeDiffAlphaIndex, betaIndex = mixedDistanceBetaIndex)
  
  printValue(parameter = 'distances'              , value = 'NEW'   )      
  
  distance = distances$mixedDistance
  
  gc()
  
  clusters       = getCluster(distance = distance, k = noClustersK, method = clusterMethod)
  
  printValue(parameter = 'clusters'               , value = 'NEW'   )
  printValue(parameter = 'noClustersK'            , value = noClustersK)  
  
  clustersProfiles = getClustersProfiles(usersTagsM = distances$usersTagsMatrix, 
                                         clusters = clusters$cluster$cluster,
                                         tagsCutGamaIndex = tagsCutGamaIndex,
                                         tagsMethod = tagsMethod)
  
  return(list(clusters  = clusters$cluster$cluster,
              clustersProfiles = clustersProfiles))
  
}

executeModelSparse = function(trainningData,
                        clusterMethod,
                        noClustersK,
                        usersTimeDiffAlphaIndex,
                        mixedDistanceBetaIndex,
                        forgCurveLambdaIndex,
                        tagsCutGamaIndex
){
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
  
  distances      = getDistancesSparse(trainningData, lambdaIndex = forgCurveLambdaIndex, alphaIndex = usersTimeDiffAlphaIndex, betaIndex = mixedDistanceBetaIndex)
  
  printValue(parameter = 'distances'              , value = 'NEW'   )      
  
  distance = distances$mixedDistance
  
  gc()
  
  clusters       = getCluster(distance = distance, k = noClustersK, method = clusterMethod)
  
  printValue(parameter = 'clusters'               , value = 'NEW'   )
  printValue(parameter = 'noClustersK'            , value = noClustersK)  
  
  clustersProfiles = getClustersProfiles(usersTagsM = distances$usersTagsMatrix, 
                                         clusters = clusters$cluster$cluster,
                                         tagsCutGamaIndex = tagsCutGamaIndex,
                                         tagsMethod = tagsMethod)
  
  return(list(clusters  = clusters$cluster$cluster,
              clustersProfiles = clustersProfiles))
  
}

getClustersProfiles = function(usersTagsM, clusters, tagsCutGamaIndex, tagsMethod){
  #### Creates the recommendation profile
  # Normalizes the users versus tags matrix so that the index cut can
  # be applied with the same scale to all users
  usersTagsNormMTR = getNormMatrix(usersTagsM, byCol = FALSE)
  
  # Builds a users versus cluster matrix from trainning (transforms
  # the cluster table into a matrix with zeros and ones)
  usersClustersMTR = getUsersClustersMatrix(clusters)
  
  # Builds a tags versus cluster matrix crossing tags fro users and
  # clusters from users 
  tagsClustersMTR  = getTagsClustersMatrix(usersTagsNormMTR,usersClustersMTR)
  
  # Tests if solution should be evaluated based on "top n" tags or
  # over tag relevance (gamaIndex)
  if (tagsMethod == 'topn') {
    # No need to normalize tags per cluster
    # Top N of each cluster will be selected
    tagsClustersMTR    = getTagsClustersTopNMatrix(tagsClustersM = tagsClustersMTR, n = tagsCutGamaIndex)
  } else if (tagsMethod == 'index') {
    # Applies the gamaIndex to the clusters x tags matrix, returning a new 
    # matrix with ones at tags that are above the gamaIndex and zeros on the 
    # rest
    tagsClustersMTR    = getTagsClustersCutMatrix(tagsClustersM = tagsClustersMTR, limit = tagsCutGamaIndex)
  } else stop(call. = TRUE)
  return(tagsClustersMTR)
}
