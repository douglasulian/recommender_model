#### Libraries ####
list.of.packages = c('dplyr')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(dplyr)

#### Sources ####
debugSource(fileName = 'scripts/util.R')
debugSource(fileName = 'scripts/distances.R')
debugSource(fileName = 'scripts/cluster.R')
#### Code ####

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
  
  clusters       = cluster(distance = distance, k = noClustersK, method = clusterMethod)
  
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

evaluateSolution = function(testingData, usersTagsM, cluster, clusterMethod, tagsMethod, articlesMethod, alphaIndex, betaIndex, lambdaIndex, gamaIndex, zetaIndex, k){
  writeLog('evaluating solution')
  parameters = list(clusterMethod  = clusterMethod,
                    tagsMethod     = tagsMethod,
                    articlesMethod = articlesMethod,
                    alphaIndex     = alphaIndex,
                    betaIndex      = betaIndex,
                    lambdaIndex    = lambdaIndex,
                    gamaIndex      = gamaIndex,
                    zetaIndex      = zetaIndex,
                    k              = k)
  writeLog('usersTagsNormMTR = getNormMatrix(usersTagsM, byCol = FALSE)')
  #### Creates the recommendation profile
  # Normalizes the users versus tags matrix so that the index cut can
  # be applied with the same scale to all users
  usersTagsNormMTR = getNormMatrix(usersTagsM, byCol = FALSE)
  
  writeLog('usersClustersMTR = getUsersClustersMatrix(cluster)')
  # Builds a users versus cluster matrix from trainning (transforms
  # the cluster table into a matrix with zeros and ones)
  usersClustersMTR = getUsersClustersMatrix(cluster)
  
  writeLog('tagsClustersMTR  = getTagsClustersMatrix(usersTagsNormMTR,usersClustersMTR)')
  # Builds a tags versus cluster matrix crossing tags fro users and
  # clusters from users 
  tagsClustersMTR  = getTagsClustersMatrix(usersTagsNormMTR,usersClustersMTR)

  writeLog('articlesTagsMTS  = getArticlesTagsMatrix(con = con)')  
  ### Crosses the recommendation profile with the sample to be recommended
  # Builds the articles versus tags matrix from the test dataset
  # articlesTagsMTS  = getArticlesTagsMatrix(con = con)
  articlesTagsMTS  = testingData$articlesTagsMTS
  
  writeLog('articlesTagsMTS = getArticleTagsMTS(articlesTagsMTS, tags = colnames(usersTagsM))')
  # Restricts to the tags used while clustering.
  articlesTagsMTS = getArticleTagsMTS(articlesTagsMTS, tags = colnames(usersTagsM))
  
  writeLog('if (tagsMethod == "topn") {')
  # Tests if solution should be evaluated based on "top n" tags or
  # over tag relevance (gamaIndex)
  if (tagsMethod == 'topn') {
    # No need to normalize tags per cluster
    # Top N of each cluster will be selected
    tagsClustersMTR    = getTagsClustersTopNMatrix(tagsClustersM = tagsClustersMTR, n = gamaIndex)
  }
  else if (tagsMethod == 'index') {
    # Applies the gamaIndex to the clusters x tags matrix, returning a new 
    # matrix with ones at tags that are above the gamaIndex and zeros on the 
    # rest
    tagsClustersMTR    = getTagsClustersCutMatrix(tagsClustersM = tagsClustersMTR, limit = gamaIndex)
  }
  else stop(call. = TRUE)
  
  writeLog('articlesClustersSuggM = getArticlesClustersSuggestionsMatrix(articlesTagsM = articlesTagsMTS, tagsClusterM = tagsClustersMTR)')
  # Crosses the articles versus tags matrix from the test dataset with the 
  # tags versus clusters matrix creating the articles (recommmended) versus 
  # clusters matrix, each recommendation will have the total tags weight to 
  # that article in that cluster
  articlesClustersSuggM = getArticlesClustersSuggestionsMatrix(articlesTagsM = articlesTagsMTS, tagsClusterM = tagsClustersMTR)
  
  writeLog('if (articlesMethod == "topn" ) {')
  if (articlesMethod == 'topn' ) {
    # Returns the articles within the top N articles weight
    articlesClustersSuggM = getArticlesClustersTopNMatrix(articlesClusters = articlesClustersSuggM, n = zetaIndex)  
  }
  else if (articlesMethod == 'index') {
    
    articlesClustersSuggM = getArticlesClustersCutMatrix(articlesClustersM = articlesClustersSuggM, limit = zetaIndex)
  }
  else stop(call. = TRUE)
  
  writeLog('articlesUsersSuggM       = getArticlesUsersSuggMatrix(articlesClustersSuggM = articlesClustersSuggM, usersClustersMTR = usersClustersMTR)')
  # Builds the articles versus users suggestion matrix
  articlesUsersSuggM       = getArticlesUsersSuggMatrix(articlesClustersSuggM = articlesClustersSuggM, usersClustersMTR = usersClustersMTR)
  
  writeLog('usersArticlesMatricesTS  = getUsersArticlesMatrices(con = con)')
  # Creates the users versus articles test matrix, to access the performance
  # from suggestion and from randomness
  # usersArticlesMatricesTS  = getUsersArticlesMatrices(con = con)
  usersArticlesMatricesTS  = testingData$usersArticlesMatricesTS
  usersArticlesTestMTS     = usersArticlesMatricesTS$usersArticlesM
  
  writeLog('positiveHits  = 0')
  # Tests the suggestion per cluster
  positiveHits  = 0
  negativeHits  = 0
  typeOneError  = 0
  typeTwoError  = 0
  totalUsers    = 0
  totalArticles = 0
  
  # noCores = detectCores(all.tests = FALSE, logical = TRUE) - 2
  noCores = getNoCores()
  doParallel::registerDoParallel(cores = noCores)
  
  hits = foreach(clusterIndex = 1:ncol(usersClustersMTR), .combine = '+') %dopar% {
    # writeLog('usersCluster              = rownames(usersClustersMTR[usersClustersMTR[,clusterIndex] == 1,,drop = FALSE])')
    # Retrieves the list of users that belong to the current cluster
    usersCluster              = rownames(usersClustersMTR[usersClustersMTR[,clusterIndex] == 1,,drop = FALSE])
    # writeLog('articlesUsersClusterSuggM = articlesUsersSuggM[,colnames(articlesUsersSuggM) %in% usersCluster, drop = FALSE]')
    # Filters the suggestion to those for the current cluster's users (where
    # article's weight is larger than zero)
    articlesUsersClusterSuggM = articlesUsersSuggM[,colnames(articlesUsersSuggM) %in% usersCluster, drop = FALSE]
    articlesUsersClusterSuggM = articlesUsersClusterSuggM[articlesUsersClusterSuggM[,1] > 0,, drop = FALSE]
    # writeLog('articlesUsersTestSampleM  = getArticlesUsersSuggTestM(usersArticlesTestMTS = usersArticlesTestMTS,articlesUsersSuggM = articlesUsersClusterSuggM)')
    # Creates the test sample (recommendation + not recommendation) for
    # the articles suggested to the current cluster
    articlesUsersTestSampleM  = getArticlesUsersSuggTestM(usersArticlesTestMTS = usersArticlesTestMTS,articlesUsersSuggM = articlesUsersClusterSuggM)
    articlesSample            = rownames(articlesUsersTestSampleM)
    # writeLog('usersIntersect            = getIntersect(vectorA = colnames(articlesUsersClusterSuggM), vectorB = colnames(articlesUsersTestSampleM))')
    # Restricts the users to those present in the suggestion matrix
    # and those present at the test dataset
    usersIntersect            = getIntersect(vectorA = colnames(articlesUsersClusterSuggM), vectorB = colnames(articlesUsersTestSampleM))
    # writeLog('    if (length(usersIntersect) > 0 & length(articlesSample) > 0) {')
    if (length(usersIntersect) > 0 & length(articlesSample) > 0) {
      totalUsers                = totalUsers + length(usersIntersect)
      totalArticles             = totalArticles + length(articlesSample)

      articlesUsersClusterSuggM = equalize(m = articlesUsersClusterSuggM,toEqualize = articlesSample ,byRow = TRUE)
      articlesUsersClusterSuggM = equalize(m = articlesUsersClusterSuggM,toEqualize = usersIntersect ,byRow = FALSE)
      articlesUsersTestSampleM  = equalize(m = articlesUsersTestSampleM ,toEqualize = articlesSample ,byRow = TRUE)
      articlesUsersTestSampleM  = equalize(m = articlesUsersTestSampleM ,toEqualize = usersIntersect ,byRow = FALSE)

      positiveHits = sum((articlesUsersClusterSuggM != 0) * (articlesUsersTestSampleM != 0))
      typeOneError = sum((articlesUsersClusterSuggM != 0) * (articlesUsersTestSampleM == 0))
      
      c(positiveHits,typeOneError)
    }
    else{
      c(0,0)
    }
  }
  doParallel::stopImplicitCluster()
  positiveHits = hits[1]
  typeOneError = hits[2]
  
  writeLog('usersArticlesBenchmarkMTS = usersArticlesTestMTS')
  # Calculates the whole test sample negativeHits and typeTwoErrors (without recommendations) 
  # to be compared to the positiveHits and typeOneErrors from the recommendation
  usersArticlesBenchmarkMTS = usersArticlesTestMTS  
  usersIntersect            = getIntersect(colnames(usersArticlesBenchmarkMTS),colnames(articlesUsersSuggM))
  usersArticlesBenchmarkMTS = usersArticlesBenchmarkMTS[,colnames(usersArticlesBenchmarkMTS) %in% usersIntersect, drop = FALSE]
  usersArticlesBenchmarkMTS = usersArticlesBenchmarkMTS[as.vector(apply(X = usersArticlesBenchmarkMTS, MARGIN = 1, FUN = sum) > 0),, drop = FALSE]
  usersArticlesNotSuggM     = usersArticlesBenchmarkMTS[!(rownames(usersArticlesBenchmarkMTS) %in% rownames(articlesUsersSuggM)),, drop = FALSE]
  
  realNegativeHits = nrow(usersArticlesNotSuggM)*ncol(usersArticlesNotSuggM) - sum(usersArticlesNotSuggM > 0)
  realTypeTwoError = sum(usersArticlesNotSuggM > 0)
  negativeHits = (positiveHits + typeOneError)*(realNegativeHits/(realNegativeHits + realTypeTwoError))
  typeTwoError = (positiveHits + typeOneError)*(realTypeTwoError/(realNegativeHits + realTypeTwoError))
  
  
  
  result = measureResults(tp = positiveHits,
                          tn = negativeHits,
                          fp = typeOneError,
                          fn = typeTwoError,
                          tu = totalUsers,
                          ta = totalArticles)
  
  result$parameters = parameters
  
  # getSummary(result)
  writeLog('done')
  return(result)
}

measureResults = function(tp, tn, fp, fn, tu, ta){
  
  precision        = round(x = tp / (tp + fp),digits = 4)
  recall           = round(x = tp / (tp + fn),digits = 4)
  fallout          = round(x = fp / (fp + tn),digits = 4)
  missRate         = round(x = fn / (tp + fn),digits = 4)
  inversePrecision = round(x = tn / (fn + tn),digits = 4)
  inverseRecall    = round(x = tn / (fp + tn),digits = 4)
  markedness       = round(tp / (tp + fp) + tn / (fn + tn) - 1,digits = 4)
  informedness     = round(tp / (tp + fn) + tn / (fp + tn) - 1,digits = 4)
  matthewsCorr     = round(sqrt(markedness * informedness),digits = 4)
  clusteredUsers   = tu
  articlesSugg     = ta
  
  return(list(
    truePositives    = tp,
    trueNegatives    = tn,
    falsePositives   = fp,
    falseNegatives   = fn,
    precision        = precision,
    recall           = recall,
    fallout          = fallout,
    missRate         = missRate,
    inverseRecall    = inverseRecall,
    inversePrecision = inversePrecision,
    markedness       = markedness,
    informedness     = informedness,
    matthewsCorr     = matthewsCorr,
    clusteredUsers   = clusteredUsers,
    articlesSugg     = articlesSugg
  ))
  
}

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

# Creates the articles suggested per cluster matrix based on cluster tags and test articles tags matrix
getArticlesClustersSuggestionsMatrix = function(articlesTagsM,tagsClusterM){
  #   Restricts tha articles versus tags to those that appear at the clusters 
  #   Removes from the tags that appear at the test articles those that do not participate of the recommendation
  tagsIntersect = getIntersect(colnames(articlesTagsM) ,rownames(tagsClusterM))
  
  articlesTagsM = equalize(m = articlesTagsM,toEqualize = tagsIntersect ,byRow = FALSE)
  tagsClusterM  = equalize(m = tagsClusterM ,toEqualize = tagsIntersect ,byRow = TRUE)
  
  return(matrixMult(m1 = as.matrix(articlesTagsM), m2 = as.matrix(tagsClusterM)))
}

# Creates the articles versus users suggestion matrix
getArticlesUsersSuggMatrix = function(articlesClustersSuggM,usersClustersMTR){

  articlesUsersSuggMatrix = matrixMult(m1 = as.matrix(articlesClustersSuggM), m2 = t(as.matrix(usersClustersMTR)))
  
  articlesUsersSuggMatrix = articlesUsersSuggMatrix[apply(FUN = sum, MARGIN = 1,X = articlesUsersSuggMatrix) != 0,, drop = FALSE]
  return(articlesUsersSuggMatrix)
}

# Creates a summary matrix based on the cluster result
getSummary = function(result){
  tp = result$truePositives
  tn = result$trueNegatives
  fp = result$falsePositives
  fn = result$falseNegatives
  total = tp + tn + fp + fn
  summary = matrix(data = c( tp/100        , fp/100       ,(tp + fp)/100   , tp/(tp + fp)    , fp/(tp + fp)    ,
                             fn/100        , tn/100       ,(fn + tn)/100   , fn/(fn + tn)    , tn/(fn + tn)    ,
                             (tp + fn)/100 ,(fp + tn)/100 ,(total/100)     , (tp + fn)/total , (fp + tn)/total ,
                             tp/(tp + fn)  , fp/(fp + tn) ,(tp + fp)/total , 0               , 0               ,
                             fn/(tp + fn)  , tn/(fp + tn) ,(fn + tn)/total , 0               , 0                ),
                   nrow = 5,
                   byrow = TRUE,
                   dimnames = list(c('Recommended','Not recommended','Total','%','%'),c('Relevant','Irrelevant','Total','%','%')))
  
  summary = round(summary*100,2)
  cat('\n')
  cat(paste0('#############################################################', '\n'))
  cat(paste0('## Parameters','\n'))
  cat(paste0('#############################################################', '\n'))
  cat(paste0('Clusters Method                           : ',result$parameters$clusterMethod,'\n'))
  cat(paste0('Number of Clusters (k)                    : ',result$parameters$k,'\n'))
  cat(paste0('Tags Selection Method                     : ',result$parameters$tagsMethod,'\n'))
  cat(paste0('Tags Selection Index (gamaIndex)          : ',result$parameters$gamaIndex,'\n'))
  cat(paste0('Articles Selection Method                 : ',result$parameters$articlesMethod,'\n'))
  cat(paste0('Articles Selection Index (zetaIndex)      : ',result$parameters$zetaIndex,'\n'))
  cat(paste0('User x User Time Effect Index (alphaIndex): ',result$parameters$alphaIndex,'\n'))
  cat(paste0('Cont. Forgetting Curve Index (lambdaIndex): ',result$parameters$lambdaIndex,'\n'))
  cat(paste0('Cont./Behav. Proportion Index (betaIndex) : ',result$parameters$betaIndex,'\n'))
  cat('\n')
  cat(paste0('#############################################################', '\n'))
  cat(paste0('## Confusion Matrix', '\n'))
  cat(paste0('#############################################################', '\n'))
  print(summary)
  cat('\n')
  cat(paste0('#############################################################', '\n'))
  cat(paste0('## Results','\n'))
  cat(paste0('#############################################################', '\n'))
  cat(paste0('Precision        : ',result$precision       ,'\n'))
  cat(paste0('Recall           : ',result$recall          ,'\n'))
  cat(paste0('Fallout          : ',result$fallout         ,'\n'))
  cat(paste0('Miss rate        : ',result$missRate        ,'\n'))
  cat(paste0('Inverse precision: ',result$inversePrecision,'\n'))
  cat(paste0('Inverse recall   : ',result$inverseRecall   ,'\n'))
  cat(paste0('Markedness       : ',result$markedness      ,'\n'))
  cat(paste0('Informedness     : ',result$informedness    ,'\n'))
  cat(paste0('Matthews Cor.    : ',result$matthewsCorr    ,'\n'))
  cat('\n')
  cat(paste0('Clustered Users  : ',result$clusteredUsers  ,'\n'))
  cat(paste0('Recom. Articles  : ',result$articlesSugg    ,'\n'))
  cat('\n')
  return(summary)
}

# Returns the part of the users versus articles test matrix that match the articles suggestion
getArticlesUsersSuggTestM = function(usersArticlesTestMTS, articlesUsersSuggM){
  return(usersArticlesTestMTS[rownames(usersArticlesTestMTS) %in% rownames(articlesUsersSuggM),, drop = FALSE])
}

# Creates the sample to apply recommendation
getArticleTagsMTS = function(articlesTagsMTS, tags) {
  #   Defines a tags vector containning those that belong to the recommendation and exist on the test dataset.
  #   Tags that appear at the recommendation but do not appear at the test have no contribution to the recommendation.
  #   Tags that appear at the test but are not part of recommendation also have no contribution to the recommendation.
  tagsIntersect     = getIntersect(colnames(articlesTagsMTS) ,tags)
  
  #   Restricts tha articles versus tags to those that appear at the clusters 
  #   Removes from the tags that appear at the test articles those that do not participate of the recommendation
  articlesTagsMTS   = equalize(m = articlesTagsMTS  ,toEqualize = tagsIntersect ,byRow = FALSE)
  
  #   Restricts the articles to those that have some index of recommendation (based on tags)
  #   Removes from the test articles those that have no recommended tag
  articlesTagsMTS   = articlesTagsMTS[apply(X = articlesTagsMTS,MARGIN = 1,FUN = sum) > 0,, drop = FALSE]
  
  return(articlesTagsMTS)
}

getTagsClustersTopNMatrix = function(tagsClustersM, n){
 return(getTopNMatrix(m = tagsClustersM,n = n))
}

getArticlesClustersTopNMatrix = function(articlesClusters,n){
  return(getTopNMatrix(m = articlesClusters,n = n))
}

# Creates a new matrix based on m, only keeping values that are above the limit
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

# Select tags per cluster that are within the limit index (after normalization per cluster)
getTagsClustersCutMatrix = function(tagsClustersM, limit){
  return(getCutMatrix(m = tagsClustersM, limit = limit))
}

# Select tags per cluster that are within the limit index (after normalization per cluster)
getArticlesClustersCutMatrix = function(articlesClustersM, limit){
  return(getCutMatrix(m = articlesClustersM, limit = limit))
}

